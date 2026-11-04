! add_log_mod.f90 -- change log maintenance commands for Emacs.
! Ported from GNU Emacs 31.1 lisp/vc/add-log.el.
!
! Copyright (C) 1985-1986, 1988, 1993-1994, 1997-1998, 2000-2026 Free
! Software Foundation, Inc.
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

!> Change logs: adding entries (`add_change_log_entry',
!> \[add_change_log_entry_other_window]) with the name of the subprogram
!> at point (`add_log_current_subprogram', which modes refine through
!> `add_log_current_subprogram_function'), finding the ChangeLog file of
!> a file (`find_change_log'), the buffer for it, or the `*changes to
!> DIR*' buffer standing for one that doesn't exist
!> (`add_log_find_changelog_buffer'), the options naming the user and the
!> date in ChangeLog headers, Change Log mode with its faces, font lock
!> and \[find_change_log_entry_file], and reading and inserting ChangeLog
!> entries (`change_log_read_subprograms', `insert_change_log_entries').
!> The part that looks at a Diff buffer is in the submodule
!> add_log_diff_mod.
!>
!> Deferred: see PORTING.md's status table (the add_log_mod row).
module add_log_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_mod, only: status_type
    use timefns_mod, only: format_time_string
    use handle_mod, only: handle_type
    use data_mod, only: string_setting_type, forward_setting, append_locals_clearer
    use hooks_mod, only: hook_type
    use commands_mod, only: commands
    use callint_mod, only: current_prefix_arg, PREFIX_NONE
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_or_make_buffer, get_file_buffer, &
                          is_buffer_live, buffer_serial_live
    use fileio_mod, only: expand_file_name, file_name_directory, file_name_nondirectory, is_file_directory, &
                          file_exists, file_name_as_directory, directory_file_name
    use files_mod, only: find_file_noselect, abbreviate_file_name, file_chase_links, find_file, file_relative_name, &
                         file_truename, is_backup_file_name, file_name_sans_versions
    use fns_mod, only: count_characters
    use derived_mod, only: is_derived_mode
    use modes_mod, only: major_modes
    use syntax_mod, only: make_syntax_table, modify_syntax_entry, skip_chars_forward, skip_chars_backward, &
                          skip_syntax_backward
    use faces_mod, only: named_face_type, display_when, face_attrs
    use buffer_mod, only: fill_column, left_margin
    use indent_mod, only: indent_tabs_mode, indent_to, indent_relative_first_indent_point, current_column
    use xdisp_mod, only: tab_width, message
    use paragraphs_mod, only: paragraph_start, paragraph_separate, page_delimiter, forward_paragraph
    use page_mod, only: forward_page
    use editfns_mod, only: point, goto_char, point_min, point_max, is_eob, char_before, delete_region, buffer_substring, &
                           line_beginning_position, line_end_position, point_marker, user_full_name, widen, &
                           narrow_to_region, buffer_size
    use cmds_mod, only: forward_line, beginning_of_line, end_of_line
    use insdel_mod, only: insert
    use search_mod, only: looking_at, re_search_forward, re_search_backward, search_forward, regexp_quote, string_match, &
                          match_beginning, case_fold_search, SEARCH_NOERROR
    use subr_mod, only: match_string_no_properties
    use undo_mod, only: undo_boundary
    use marker_mod, only: marker_position, destroy_marker
    use window_mod, only: window_buffer, window_dedication, switch_to_buffer, switch_to_buffer_other_window, &
                          DEDICATED_NONE
    use minibuf_mod, only: read_string
    use minibuffer_mod, only: read_file_name
    use replace_mod, only: format_prompt
    use startup_mod, only: user_mail_address_value
    use vc_hooks_mod, only: vc_working_revision
    use keymap_mod, only: keymap_set
    use font_core_mod, only: font_lock_defaults, font_lock_keyword_type, font_lock_highlight_type
    use font_lock_mod, only: font_lock_keyword, font_lock_highlight, font_lock_anchored
    use string_type_mod, only: string_type, append_string
    implicit none(type, external)
    private

    public :: find_change_log, change_log_name, add_log_find_changelog_buffer, add_log_pseudo_changelog_buffer_name, &
              add_log_iso8601_time_string, change_log_mode, init_add_log
    public :: change_log_entry_type, change_log_read_subprograms, insert_change_log_entries
    public :: add_log_name_procedure, add_log_file_name_procedure, add_log_name_function_type
    public :: add_change_log_entry, add_change_log_entry_other_window, add_log_current_subprogram, add_log_file_name, &
              read_change_log_name, is_add_log_changelog_buffer, search_change_log_version_number, &
              search_change_log_file_name, find_change_log_entry_file, change_log_font_lock_keywords

    character(len=*), parameter :: LF = achar(10), TAB = achar(9)

    !> The entries of one file, as `diff_add_log_current_subprograms' gives
    !> them and `insert_change_log_entries' inserts them: the FILE and the
    !> names of the SUBPROGRAMS changed in it.
    type :: change_log_entry_type
        character(len=:), allocatable :: file
        type(string_type), allocatable :: subprograms(:)
    end type change_log_entry_type

    abstract interface
        !> A function giving a name (in NAME, unallocated for none): the
        !> interface of `add_log_current_subprogram_function' and
        !> `add_log_buffer_file_name_function'.  Not a port.
        recursive subroutine add_log_name_procedure(name)
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: name
        end subroutine add_log_name_procedure

        !> An `add_log_file_name_function': the file name (in NAME) to use
        !> in a ChangeLog entry for the file BUFFER_FILE.  Not a port.
        subroutine add_log_file_name_procedure(buffer_file, name)
            implicit none(type, external)
            character(len=*), intent(in) :: buffer_file
            character(len=:), allocatable, intent(out) :: name
        end subroutine add_log_file_name_procedure
    end interface

    !> One buffer's own function.
    type :: add_log_name_entry_type
        integer(int64) :: serial = 0
        procedure(add_log_name_procedure), pointer, nopass :: fn => null()
    end type add_log_name_entry_type

    !> The type of `add_log_current_subprogram_function' and
    !> `add_log_buffer_file_name_function': a default `value' (assign with
    !> `=>'; null when unset) and buffers' own (`set_local').
    type :: add_log_name_function_type
        procedure(add_log_name_procedure), pointer, nopass :: value => null()
        type(add_log_name_entry_type), allocatable :: entries(:)
    contains
        !> Make FN the current buffer's function (set_local).
        procedure :: set_local => name_function_set_local
        !> The current buffer's function (null when unset).
        procedure :: current => current_name_function
        !> Make the current buffer use the default again.
        procedure :: clear_local => name_function_clear_local
    end type add_log_name_function_type

    !> Regexp matching the file names of unindented ChangeLog entries.
    !> Port of add-log.el change-log-unindented-file-names-re.
    character(len=*), parameter, public :: change_log_unindented_file_names_regexp = "^[*] \([^ ,:(["//LF//"]+\)"

    !> Regexp matching a tag name in change log entries.  Port of
    !> add-log.el change-log-tag-re.
    character(len=*), parameter, public :: change_log_tag_regexp = &
                                           "(\(\(?:\sw\|\s_\)+\(?:[, "//TAB//"]+\(?:\sw\|\s_\)+\)*\))"

    !> Regexp matching the indented file names of ChangeLog entries (the
    !> name in group 2).  Port of add-log.el change-log-file-names-re.
    character(len=*), parameter, public :: change_log_file_names_regexp = "^\( +\|"//TAB//"\)\* \([^ ,:(["//LF//"]+\)"

    !> Regexp matching the date line that starts a ChangeLog entry.  Port
    !> of add-log.el change-log-start-entry-re.
    character(len=*), parameter, public :: change_log_start_entry_regexp = "^\sw.........[0-9:+ ]*"

    !> A function giving (in S) the date for a ChangeLog header.
    abstract interface
        ! Not a port.
        subroutine add_log_time_procedure(s)
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: s
        end subroutine add_log_time_procedure
    end interface

    !> Name of a change log file for \[add_change_log_entry].  Port of
    !> add-log.el change-log-default-name (unallocated when unset; becomes
    !> buffer-local when `find_change_log' sets it).
    type(string_setting_type), public :: change_log_default_name = string_setting_type(name="change_log_default_name")

    !> Normal hook run by `change_log_mode'.  Port of add-log.el
    !> change-log-mode-hook.
    type(hook_type), target, public :: change_log_mode_hook = hook_type(name="change_log_mode_hook")

    !> If not null, function to guess name of surrounding function.
    !> It is called by `add_log_current_subprogram' with no argument, and
    !> should return the function's name as a string, or unallocated if
    !> point is outside a function.  Port of add-log.el
    !> add-log-current-defun-function (modes give buffers their own with
    !> `set_local').
    type(add_log_name_function_type), public :: add_log_current_subprogram_function

    !> If not null, function to call to identify the full filename of a buffer.
    !> This function is called with no argument.  The default is to
    !> use `buffer_file_name'.  Port of add-log.el
    !> add-log-buffer-file-name-function (null for the default).
    type(add_log_name_function_type), public :: add_log_buffer_file_name_function

    !> If not null, function to call to identify the filename for a ChangeLog entry.
    !> This function is called with one argument, the value of variable
    !> `buffer_file_name' in that buffer.  If this is null, the default is to
    !> use the file's name relative to the directory of the change log file.
    !> Port of add-log.el add-log-file-name-function.
    procedure(add_log_file_name_procedure), pointer, public :: add_log_file_name_function => null()

    !> Full name of user, for inclusion in ChangeLog daily headers.
    !> This defaults to the value returned by the function `user_full_name'.
    !> Port of add-log.el add-log-full-name (unallocated for the default).
    character(len=:), allocatable, public :: add_log_full_name

    !> Email addresses of user, for inclusion in ChangeLog headers.
    !> This defaults to the value of `user_mail_address'.  Port of add-log.el
    !> add-log-mailing-address (one address; unallocated for the default).
    character(len=:), allocatable, public :: add_log_mailing_address

    !> Function that defines the time format.
    !> For example, `add_log_iso8601_time_string', which gives the
    !> date in international ISO 8601 format,
    !> and `current_time_string' are two valid values.  Port of add-log.el
    !> add-log-time-format.
    procedure(add_log_time_procedure), pointer, public :: add_log_time_format => add_log_iso8601_time_string

    !> If true, normally keep day's log entries for one file together.
    !>
    !> Log entries for a given file made with \[add_change_log_entry] or
    !> \[add_change_log_entry_other_window] will only be added to others for that file made
    !> today if this variable is true or that file comes first in today's
    !> entries.  Otherwise another entry for that file will be started.  An
    !> original log:
    !>
    !>         * foo (...): ...
    !>         * bar (...): change 1
    !>
    !> in the latter case, \[add_change_log_entry_other_window] in a buffer visiting `bar', yields:
    !>
    !>         * bar (...): -!-
    !>         * foo (...): ...
    !>         * bar (...): change 1
    !>
    !> and in the former:
    !>
    !>         * foo (...): ...
    !>         * bar (...): change 1
    !>         (...): -!-
    !>
    !> The NEW_ENTRY arg to `add_change_log_entry' can override the effect of
    !> this variable.  Port of add-log.el add-log-keep-changes-together.
    logical, public :: add_log_keep_changes_together = .false.

    !> If true, `add_change_log_entry' will always start a new record.
    !> Port of add-log.el add-log-always-start-new-record.
    logical, public :: add_log_always_start_new_record = .false.

    !> If true, enable recording version numbers with the changes.  Port of
    !> add-log.el change-log-version-info-enabled.
    logical, public :: change_log_version_info_enabled = .false.

    !> List of regexps to search for version number.
    !> The version number must be in group 1.
    !> Note: The search is conducted only within 10%, at the beginning of the file.
    !> Port of add-log.el change-log-version-number-regexp-list.
    character(len=64), allocatable, public :: change_log_version_number_regexps(:)

    !> Heuristic regexp used by `add_log_current_subprogram' for unknown major modes.
    !> The regexp's first submatch is placed in the ChangeLog entry, in
    !> parentheses.  Port of add-log.el add-log-current-defun-header-regexp.
    character(len=:), allocatable, public :: add_log_current_subprogram_header_regexp

    !> If true, don't create ChangeLog files for log entries.
    !> If a ChangeLog file does not already exist, a true value
    !> means to put log entries in a suitably named buffer.  Port of
    !> add-log.el add-log-dont-create-changelog-file.
    logical, public :: add_log_dont_create_changelog_file = .true.

    !> List of files that cause `find_change_log' to stop in containing directory.
    !> This applies if no pre-existing ChangeLog is found.  If empty, then in such
    !> a case simply use the directory containing the changed file.  Port of
    !> add-log.el change-log-directory-files.
    character(len=8), allocatable, public :: change_log_directory_files(:)

    !> Face used to highlight dates in date lines.  Port of add-log.el
    !> change-log-date.
    type(named_face_type), target, public :: change_log_date
    !> Face for highlighting author names.
    type(named_face_type), target, public :: change_log_name_face
    !> Face for highlighting author email addresses.
    type(named_face_type), target, public :: change_log_email
    !> Face for highlighting file names.
    type(named_face_type), target, public :: change_log_file
    !> Face for highlighting parenthesized lists of functions or variables.
    type(named_face_type), target, public :: change_log_list
    !> Face for highlighting conditionals of the form `[...]'.
    type(named_face_type), target, public :: change_log_conditionals
    !> Face for highlighting items of the form `<....>'.
    type(named_face_type), target, public :: change_log_function
    !> Face for highlighting acknowledgments.
    type(named_face_type), target, public :: change_log_acknowledgment

    !> Keymap for Change Log major mode.  Port of add-log.el
    !> change-log-mode-map.
    type(handle_type), public :: change_log_mode_map

    !> Syntax table used while in `change_log_mode'.  Port of add-log.el
    !> change-log-mode-syntax-table.
    type(handle_type), public :: change_log_mode_syntax_table

    interface
        !> The source buffer of the hunk at point in a Diff buffer (the
        !> null handle for none), for `find_change_log'.  Not a port: the
        !> `diff_find_source_location' part of add-log.el:find-change-log.
        !> (In add_log_diff_mod.)
        recursive module subroutine get_diff_source_buffer(buffer)
            implicit none(type, external)
            type(handle_type), intent(out) :: buffer
        end subroutine get_diff_source_buffer
    end interface

contains

    ! ------------------------------------------------------------------
    ! Buffer-local functions
    ! ------------------------------------------------------------------

    ! Not a port.
    subroutine name_function_set_local(self, fn)
        class(add_log_name_function_type), intent(inout) :: self
        procedure(add_log_name_procedure) :: fn
        type(add_log_name_entry_type) :: entry
        integer :: i

        if (current == 0) return
        if (.not. allocated(self%entries)) allocate (self%entries(0))
        i = findloc(self%entries%serial, buffers(current)%serial, dim=1)

        if (i == 0) then
            ! Reuse the entry of a dead buffer.
            do i = 1, size(self%entries)
                if (.not. buffer_serial_live(self%entries(i)%serial)) exit
            end do

            if (i > size(self%entries)) self%entries = [self%entries, entry]
            self%entries(i)%serial = buffers(current)%serial
        end if

        self%entries(i)%fn => fn
    end subroutine name_function_set_local

    ! Not a port.
    function current_name_function(self) result(fn)
        class(add_log_name_function_type), intent(in) :: self
        procedure(add_log_name_procedure), pointer :: fn
        integer :: i

        fn => self%value
        if (current == 0) return
        if (.not. allocated(self%entries)) return
        i = findloc(self%entries%serial, buffers(current)%serial, dim=1)

        if (i > 0) then
            if (associated(self%entries(i)%fn)) fn => self%entries(i)%fn
        end if
    end function current_name_function

    ! Not a port.
    subroutine name_function_clear_local(self)
        class(add_log_name_function_type), intent(inout) :: self
        integer :: i

        if (current == 0) return
        if (.not. allocated(self%entries)) return
        i = findloc(self%entries%serial, buffers(current)%serial, dim=1)

        if (i > 0) then
            self%entries(i)%serial = 0
            self%entries(i)%fn => null()
        end if
    end subroutine name_function_clear_local

    ! Forget the current buffer's own add-log functions
    ! (kill-all-local-variables).  Not a port.
    subroutine clear_add_log_locals()
        call add_log_current_subprogram_function%clear_local()
        call add_log_buffer_file_name_function%clear_local()
    end subroutine clear_add_log_locals

    ! ------------------------------------------------------------------
    ! Reading and inserting entries
    ! ------------------------------------------------------------------

    !> Read ChangeLog formatted function names at point until END.
    !> Move point to the end of names read and return the function names
    !> as a list of strings.  Port of add-log.el:change-log-read-defuns.
    recursive subroutine change_log_read_subprograms(names, status, end)
        use editfns_mod, only: point, goto_char
        use syntax_mod, only: skip_chars_forward, skip_chars_backward
        use search_mod, only: looking_at, match_end
        use subr_mod, only: match_string_no_properties
        type(string_type), allocatable, intent(out) :: names(:)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: end
        character(len=:), allocatable :: tag
        integer(int64) :: moved
        logical :: tagged

        allocate (names(0))

        do
            moved = skip_chars_forward(":"//LF//"[:blank:]", status, end)
            if (.not. status%ok()) return

            if (present(end)) then
                if (point() >= end) exit
            end if

            tagged = looking_at(change_log_tag_regexp, status)
            if (.not. status%ok()) return
            if (.not. tagged) exit
            tag = match_string_no_properties(1)
            call goto_char(match_end_0())
            call split_tag(tag)
        end do

        moved = skip_chars_backward(LF//"[:blank:]", status)

    contains

        integer(int64) function match_end_0() result(pos)
            use search_mod, only: match_end
            type(status_type) :: st

            pos = match_end(0, st)
        end function match_end_0

        ! (split-string TAG ",[[:blank:]]*" t), appended to NAMES.
        subroutine split_tag(tag)
            character(len=*), intent(in) :: tag
            integer :: i, j, k

            i = 1

            do
                j = index(tag(i:), ",")

                if (j == 0) then
                    call append(tag(i:))
                    exit
                end if

                call append(tag(i:i + j - 2))
                k = verify(tag(i + j:), " "//TAB)
                i = merge(len(tag) + 1, i + j + k - 1, k == 0)
            end do
        end subroutine split_tag

        subroutine append(name)
            character(len=*), intent(in) :: name

            if (len(name) == 0) return
            call append_string(names, name)
        end subroutine append
    end subroutine change_log_read_subprograms

    !> Format and insert CHANGELOGS into current buffer.
    !> CHANGELOGS is a list in the form returned by
    !> `diff_add_log_current_subprograms'.  Port of
    !> add-log.el:change-log-insert-entries.
    recursive subroutine insert_change_log_entries(changelogs, status)
        type(change_log_entry_type), intent(in) :: changelogs(:)
        type(status_type), intent(out) :: status
        integer :: i, j

        status = status_type()

        do i = 1, size(changelogs)
            call insert("* "//changelogs(i)%file, status)
            if (.not. status%ok()) return

            if (size(changelogs(i)%subprograms) == 0) then
                call insert(":"//LF, status)
            else
                call insert(" ", status)

                do j = 1, size(changelogs(i)%subprograms)
                    if (status%ok()) call insert("("//changelogs(i)%subprograms(j)%text//"):"//LF, status)
                end do
            end if

            if (.not. status%ok()) return
        end do
    end subroutine insert_change_log_entries

    ! ------------------------------------------------------------------
    ! Change Log mode commands
    ! ------------------------------------------------------------------

    !> Return the file-name for the change under point (at WHERE; NAME
    !> unallocated for none, FILE_BEGINNING where it starts).  Port of
    !> add-log.el:change-log-search-file-name.
    recursive subroutine search_change_log_file_name(where, name, file_beginning)
        integer(int64), intent(in) :: where
        character(len=:), allocatable, intent(out) :: name
        integer(int64), intent(out), optional :: file_beginning
        type(status_type) :: st
        integer(int64) :: opoint, found, beginning
        logical :: matched

        opoint = point()
        beginning = 0
        call goto_char(where)
        call beginning_of_line()

        if (looking_at(change_log_start_entry_regexp, st)) then
            ! We are at the start of an entry, search forward for a file
            ! name.
            call file_name_after(2)
        else if (looking_at(change_log_file_names_regexp, st)) then
            ! We found a file name.
            call take(2)
        else
            ! Look backwards for either a file name or the log entry start.
            call re_search_backward("\("//change_log_start_entry_regexp//"\)\|\("//change_log_file_names_regexp//"\)", &
                                    found, st, noerror=SEARCH_NOERROR)

            if (found > 0) then
                if (match_beginning(1, st) > 0) then
                    ! We got the start of the entry, look forward for a
                    ! file name.
                    call file_name_after(2)
                else
                    call take(4)
                end if
            else
                ! We must be before any file name, look forward.
                call file_name_after(2)
            end if
        end if

        if (present(file_beginning)) file_beginning = beginning
        call goto_char(opoint)

    contains

        ! (re-search-forward change-log-file-names-re nil t), then group N.
        subroutine file_name_after(n)
            integer, intent(in) :: n
            integer(int64) :: pos

            call re_search_forward(change_log_file_names_regexp, pos, st, noerror=SEARCH_NOERROR)
            if (pos > 0) call take(n)
        end subroutine file_name_after

        subroutine take(n)
            integer, intent(in) :: n

            name = match_string_no_properties(n, matched=matched)

            if (matched) then
                beginning = match_beginning(n, st)
            else
                deallocate (name)
            end if
        end subroutine take
    end subroutine search_change_log_file_name

    !> Visit the file for the change under point.  Port of
    !> add-log.el:change-log-find-file.
    recursive subroutine find_change_log_entry_file(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file

        status = status_type()
        call search_change_log_file_name(point(), file)

        if (allocated(file)) then
            if (file_exists(file)) then
                call find_file(file, status)
                return
            end if
        else
            file = ""
        end if

        call message("No such file or directory: "//file)
    end subroutine find_change_log_entry_file

    !> Return the date in ISO 8601 form (YYYY-MM-DD), local time, as S.
    !> Port of add-log.el:add-log-iso8601-time-string (no
    !> `add_log_time_zone_rule'; a subroutine, as `add_log_time_format'
    !> points to it).
    subroutine add_log_iso8601_time_string(s)
        character(len=:), allocatable, intent(out) :: s

        s = format_time_string("%Y-%m-%d")
    end subroutine add_log_iso8601_time_string

    !> Return (system-dependent) default name for a change log file.
    !> Port of add-log.el:change-log-name.
    function change_log_name() result(name)
        character(len=:), allocatable :: name

        if (allocated(change_log_default_name%value)) then
            name = change_log_default_name%value
        else
            name = "ChangeLog"
        end if
    end function change_log_name

    !> Prompt for a change log name.  Port of
    !> add-log.el:prompt-for-change-log-name.
    recursive subroutine read_change_log_name(name, status)
        character(len=:), allocatable, intent(out) :: name
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: default, answer, expanded

        default = change_log_name()
        call read_file_name(format_prompt("Log file", default), answer, status, default_filename=default)
        if (.not. status%ok()) return
        call expand_file_name(answer, expanded, status)
        if (.not. status%ok()) return

        if (len(file_name_nondirectory(expanded)) == 0) then
            ! Handle something that is syntactically a directory name.
            ! Look for ChangeLog or whatever in that directory.
            call expand_file_name(file_name_nondirectory(default), name, status, expanded)
        else if (is_file_directory(expanded)) then
            ! Handle specifying a file that is a directory.
            call expand_file_name(file_name_nondirectory(default), name, status, file_name_as_directory(expanded))
        else
            name = expanded
        end if
    end subroutine read_change_log_name

    !> Return version number of current buffer's file.
    !> This is the value returned by `vc_working_revision' or, if that is
    !> unallocated, by matching `change_log_version_number_regexps'
    !> (VERSION unallocated for none).  Port of
    !> add-log.el:change-log-version-number-search.
    recursive subroutine search_change_log_version_number(version)
        character(len=:), allocatable, intent(out) :: version
        integer(int64) :: total, limit, opoint, begv, zv_from_end, found
        type(status_type) :: st
        character(len=:), allocatable :: file
        logical :: has_revision
        integer :: k

        total = buffer_size()
        ! The version number can be anywhere in the file, but
        ! restrict search to the file beginning: 10% should be
        ! enough to prevent some mishits.
        !
        ! Apply percentage only if buffer size is bigger than
        ! approx 100 lines.
        limit = 0
        if (total > 100*80) limit = point() + total/10

        if (allocated(buffers(current)%filename)) then
            ! A copy: the backend can grow (reallocate) the buffer registry.
            file = buffers(current)%filename
            call vc_working_revision(file, version, has_revision)
            if (has_revision) return
            if (allocated(version)) deallocate (version)
        end if

        if (.not. allocated(change_log_version_number_regexps)) return
        opoint = point()
        begv = buffers(current)%begv
        zv_from_end = buffers(current)%text%z - buffers(current)%zv
        call widen()

        do k = 1, size(change_log_version_number_regexps)
            call goto_char(point_min())

            if (limit > 0) then
                call re_search_forward(trim(change_log_version_number_regexps(k)), found, st, bound=limit, &
                                       noerror=SEARCH_NOERROR)
            else
                call re_search_forward(trim(change_log_version_number_regexps(k)), found, st, noerror=SEARCH_NOERROR)
            end if

            if (st%ok() .and. found > 0) then
                version = match_string_no_properties(1)
                exit
            end if
        end do

        call goto_char(opoint)
        call narrow_to_region(begv, buffers(current)%text%z - zv_from_end, st)
    end subroutine search_change_log_version_number

    !> Find a change log file for \[add_change_log_entry] and return the name.
    !>
    !> Optional arg FILE_NAME specifies the file to use.
    !> If FILE_NAME is absent, use the value of `change_log_default_name'.
    !> If `change_log_default_name' is unset, behave as though it were "ChangeLog".
    !>
    !> If `change_log_default_name' contains a leading directory component, then
    !> simply find it in the current directory.  Otherwise, search in the current
    !> directory and its successive parents for a file so named.  Stop at the first
    !> such file that exists (or has a buffer visiting it), or the first directory
    !> that contains any of `change_log_directory_files'.  If no match is found,
    !> use the current directory.  To override the choice of this function,
    !> simply create an empty ChangeLog file first by hand in the desired place.
    !>
    !> Once a file is found, `change_log_default_name' is set locally in the
    !> current buffer to the complete file name.
    !> Optional arg BUFFER_FILE overrides `buffer_file_name'.  Port of
    !> add-log.el:find-change-log.
    recursive function find_change_log(file_name_in, buffer_file) result(file_name)
        character(len=*), intent(in), optional :: file_name_in, buffer_file
        character(len=:), allocatable :: file_name, cbase, dir, try, up, source, chased
        type(handle_type) :: old, source_buffer
        type(status_type) :: st
        logical :: found
        integer :: k

        ! If we are called from a diff, first switch to the source buffer;
        ! in order to respect buffer-local settings of change-log-default-name, etc.
        old = current_buffer()

        if (is_derived_mode(major_modes%diff_mode)) then
            call get_diff_source_buffer(source_buffer)
            if (is_buffer_live(source_buffer)) call set_buffer(source_buffer, st)
        end if

        ! If user specified a file name or if this buffer knows which one to use,
        ! just use that.
        if (present(file_name_in)) then
            file_name = file_name_in
        else if (allocated(change_log_default_name%value)) then
            if (len(file_name_directory(change_log_default_name%value)) > 0) then
                file_name = change_log_default_name%value
            end if
        end if

        if (.not. allocated(file_name)) then
            ! Chase links in the source file
            ! and use the change log in the dir where it points.
            source = ""

            if (present(buffer_file)) then
                source = buffer_file
            else if (allocated(buffers(current)%filename)) then
                source = buffers(current)%filename
            end if

            if (len(source) > 0) then
                call file_chase_links(source, chased, st)
                file_name = file_name_directory(chased)
            else
                file_name = buffers(current)%directory
            end if

            if (is_file_directory(file_name)) then
                dir = file_name
                call expand_file_name(change_log_name(), file_name, st, dir)
            end if

            ! Chase links before visiting the file.
            ! This makes it easier to use a single change log file
            ! for several related directories.
            call file_chase_links(file_name, chased, st)
            call expand_file_name(chased, file_name, st)
            cbase = file_name_nondirectory(change_log_name())
            ! locate-dominating-file with the test.
            dir = file_name_directory(abbreviate_file_name(file_name))
            found = .false.

            do while (len(dir) > 0)
                call expand_file_name(cbase, try, st, dir)

                if (is_buffer_live(get_file_buffer(try)) .or. file_exists(try)) then
                    found = .true.
                else if (allocated(change_log_directory_files)) then
                    ! Stop at VCS root?
                    do k = 1, size(change_log_directory_files)
                        call expand_file_name(trim(change_log_directory_files(k)), try, st, dir)

                        if (file_exists(try)) then
                            found = .true.
                            exit
                        end if
                    end do
                end if

                if (found) exit
                up = file_name_directory(directory_file_name(dir))
                if (up == dir) exit
                dir = up
            end do

            if (found) call expand_file_name(cbase, file_name, st, file_name_as_directory(dir))
        end if

        ! Make a local variable in this buffer so we needn't search again.
        call change_log_default_name%set_local(file_name)
        if (is_buffer_live(old)) call set_buffer(old, st)
    end function find_change_log

    !> Compute file-name of BUFFER_FILE to be used in entries in LOG_FILE
    !> (NAME; unallocated when BUFFER_FILE is absent or the change log
    !> itself).  Port of add-log.el:add-log-file-name.
    recursive subroutine add_log_file_name(log_file, name, buffer_file)
        character(len=*), intent(in) :: log_file
        character(len=:), allocatable, intent(out) :: name
        character(len=*), intent(in), optional :: buffer_file
        character(len=:), allocatable :: dir, rel, new, true_file, true_dir
        type(status_type) :: st, st2

        ! Never want to add a change log entry for the ChangeLog file itself.
        if (.not. present(buffer_file)) return
        if (buffer_file == log_file .and. len(buffer_file) == len(log_file)) return

        if (associated(add_log_file_name_function)) then
            call add_log_file_name_function(buffer_file, name)
            return
        end if

        dir = file_name_directory(log_file)
        rel = file_relative_name(buffer_file, dir)

        ! Sometimes with symlinks, the two buffers may have names that
        ! appear to belong to different directory trees.  So check the
        ! file-truenames, to see if we get a better result.
        if (index(rel, "../") == 1) then
            call file_truename(buffer_file, true_file, st)
            call file_truename(dir, true_dir, st2)

            if (st%ok() .and. st2%ok()) then
                new = file_relative_name(true_file, true_dir)
                if (count_characters(new) < count_characters(rel)) rel = new
            end if
        end if

        ! If we have a backup file, it's presumably because we're
        ! comparing old and new versions (e.g. for deleted
        ! functions) and we'll want to use the original name.
        if (is_backup_file_name(rel)) then
            name = file_name_sans_versions(rel)
        else
            name = rel
        end if
    end subroutine add_log_file_name

    !> Compute a suitable name for a non-file visiting ChangeLog buffer.
    !> CHANGELOG_FILE_NAME is the file name of the actual ChangeLog file
    !> if it were to exist.  Port of
    !> add-log.el:add-log--pseudo-changelog-buffer-name.
    function add_log_pseudo_changelog_buffer_name(changelog_file_name) result(name)
        character(len=*), intent(in) :: changelog_file_name
        character(len=:), allocatable :: name

        name = "*changes to "//abbreviate_file_name(file_name_directory(changelog_file_name))//"*"
    end function add_log_pseudo_changelog_buffer_name

    !> Return true if BUFFER holds a change log for CHANGELOG_FILE_NAME.
    !> Port of add-log.el:add-log--changelog-buffer-p.
    logical function is_add_log_changelog_buffer(changelog_file_name, buffer) result(is)
        character(len=*), intent(in) :: changelog_file_name
        type(handle_type), intent(in) :: buffer
        character(len=:), allocatable :: name

        is = .false.
        if (.not. is_buffer_live(buffer)) return

        if (allocated(buffers(buffer%index)%filename)) then
            name = buffers(buffer%index)%filename
        else
            name = buffers(buffer%index)%name
            if (name /= add_log_pseudo_changelog_buffer_name(changelog_file_name)) return
            is = len(name) == len(add_log_pseudo_changelog_buffer_name(changelog_file_name))
            return
        end if

        is = name == changelog_file_name .and. len(name) == len(changelog_file_name)
    end function is_add_log_changelog_buffer

    !> Find a ChangeLog buffer for CHANGELOG_FILE_NAME.
    !> Respect `add_log_pseudo_changelog_buffer_name', which see.  Port of
    !> add-log.el:add-log-find-changelog-buffer.
    recursive subroutine add_log_find_changelog_buffer(changelog_file_name, buffer, status)
        character(len=*), intent(in) :: changelog_file_name
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status

        status = status_type()

        if (file_exists(changelog_file_name) .or. .not. add_log_dont_create_changelog_file) then
            call find_file_noselect(changelog_file_name, buffer, status)
        else
            call get_or_make_buffer(add_log_pseudo_changelog_buffer_name(changelog_file_name), buffer, status)
        end if
    end subroutine add_log_find_changelog_buffer

    ! ------------------------------------------------------------------
    ! Adding entries
    ! ------------------------------------------------------------------

    !> Find ChangeLog buffer, add an entry for today and an item for this file.
    !> Optional arg WHOAMI (the prefix argument) true means prompt for
    !> user name and email (stored in `add_log_full_name'
    !> and `add_log_mailing_address').
    !>
    !> Second arg CHANGELOG_FILE_NAME is the file name of the change log.
    !> If absent, use the value of `change_log_default_name'.  If the file
    !> thus named exists, it is used for the new entry.  If it doesn't
    !> exist, it is created, unless `add_log_dont_create_changelog_file' is .true.,
    !> in which case a suitably named buffer that doesn't visit any file
    !> is used for keeping entries pertaining to CHANGELOG_FILE_NAME's
    !> directory.
    !>
    !> Third arg OTHER_WINDOW true means visit in other window.
    !>
    !> Fourth arg NEW_ENTRY true means always create a new entry at the front;
    !> never append to an existing entry.  Option `add_log_keep_changes_together'
    !> otherwise affects whether a new entry is created.
    !>
    !> Fifth arg PUT_NEW_ENTRY_ON_NEW_LINE true means that if a new
    !> entry is created, put it on a new line by itself, do not put it
    !> after a comma on an existing line.
    !>
    !> Option `add_log_always_start_new_record' true means always create a
    !> new record, even when the last record was made on the same date and by
    !> the same person.
    !>
    !> The change log file can start with a copyright notice and a copying
    !> permission notice.  The first blank line indicates the end of these
    !> notices.
    !>
    !> Today's date is calculated in local time.  Port of
    !> add-log.el:add-change-log-entry (one mailing address; no
    !> `add_log_time_zone_rule' or `use_hard_newlines').
    recursive subroutine add_change_log_entry(status, whoami, changelog_file_name, other_window, new_entry, &
                                              put_new_entry_on_new_line)
        use expression_motion_mod, only: backward_expression
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: whoami, other_window, new_entry, put_new_entry_on_new_line
        character(len=*), intent(in), optional :: changelog_file_name
        character(len=:), allocatable :: subprogram, version, buffer_file_name, buffer_file, log_file, item, full_name, &
                                         mailing_address, entry_text, time, answer, text
        procedure(add_log_name_procedure), pointer :: fn
        type(handle_type) :: shown, log_buffer, pos
        type(status_type) :: st
        integer(int64) :: bound, found, saved, moved, here, beginning, p, mincol
        integer(int32) :: c
        logical :: hit, saved_case, merge, other, new, on_new_line, noindent, blank, has_c

        other = .false.
        if (present(other_window)) other = other_window
        new = .false.
        if (present(new_entry)) new = new_entry
        on_new_line = .false.
        if (present(put_new_entry_on_new_line)) on_new_line = put_new_entry_on_new_line
        call add_log_current_subprogram(subprogram)
        if (change_log_version_info_enabled) call search_change_log_version_number(version)
        fn => add_log_buffer_file_name_function%current()

        if (associated(fn)) then
            call fn(buffer_file_name)
        else if (allocated(buffers(current)%filename)) then
            buffer_file_name = buffers(current)%filename
        end if

        if (allocated(buffer_file_name)) then
            call expand_file_name(buffer_file_name, buffer_file, status)
            if (.not. status%ok()) return
        end if

        call expand_file_name(find_change_log(changelog_file_name, buffer_file), log_file, status)
        if (.not. status%ok()) return
        ! Set ITEM to the file name to use in the new item.
        call add_log_file_name(log_file, item, buffer_file)
        ! Don't add entries from the ChangeLog file/buffer to itself.
        hit = .false.

        if (allocated(buffers(current)%filename)) then
            hit = log_file == buffers(current)%filename .and. len(log_file) == len(buffers(current)%filename)
        end if

        if (.not. hit) then
            shown = window_buffer(status=st)
            if (.not. st%ok()) shown = handle_type()

            if (is_add_log_changelog_buffer(log_file, shown)) then
                ! If the selected window already shows the desired buffer don't show
                ! it again (particularly important if other-window is true).
                ! This is important for diff-add-change-log-entries-other-window.
                call set_buffer(shown, status)
            else
                if (window_dedication(status=st) /= DEDICATED_NONE) then
                    if (st%ok()) other = .true.
                end if

                call add_log_find_changelog_buffer(log_file, log_buffer, status)
                if (.not. status%ok()) return

                if (other) then
                    call switch_to_buffer_other_window(log_buffer, status)
                else
                    call switch_to_buffer(buffers(log_buffer%index)%name, status)
                end if
            end if

            if (.not. status%ok()) return
        end if

        if (.not. is_derived_mode(major_modes%change_log_mode)) then
            call change_log_mode(status)
            if (.not. status%ok()) return
        end if

        call undo_boundary()
        call goto_char(point_min())

        if (allocated(add_log_full_name)) then
            full_name = add_log_full_name
        else
            full_name = user_full_name()
        end if

        if (allocated(add_log_mailing_address)) then
            mailing_address = add_log_mailing_address
        else
            mailing_address = user_mail_address_value()
        end if

        if (present(whoami)) then
            if (whoami) then
                call read_string("Full name: ", answer, status, initial_input=full_name)
                if (.not. status%ok()) return
                full_name = answer
                ! Note that some sites have room and phone number fields in
                ! full name which look silly when inserted.  Rather than do
                ! anything about that here, let user give prefix argument so that
                ! s/he can edit the full name field in prompter if s/he wants.
                call read_string("Mailing address: ", answer, status, initial_input=mailing_address)
                if (.not. status%ok()) return
                mailing_address = answer
            end if
        end if

        ! If file starts with a copyright and permission notice, skip them.
        ! Assume they end at first blank line.
        if (looking_at("Copyright", st)) then
            call search_forward(LF//LF, found, status)
            if (.not. status%ok()) return
            moved = skip_chars_forward(LF, st)
        end if

        ! Advance into first entry if it is usable; else make new one.
        call add_log_time_format(time)
        entry_text = time//"  "//full_name//"  <"//mailing_address//">"
        hit = .false.

        if (.not. add_log_always_start_new_record) then
            if (looking_at(regexp_quote(entry_text), st)) then
                ! Reject multiple author entries.  (Bug#8645)
                saved = point()
                moved = forward_line(1_int64)
                hit = .not. looking_at("[ "//TAB//"]+.*<.*>$", st)
                call goto_char(saved)
            end if
        end if

        if (hit) then
            moved = forward_line(1_int64)
        else
            call insert(entry_text//LF//LF, status)
            if (.not. status%ok()) return
            moved = forward_line(-1_int64)
        end if

        ! Determine where we should stop searching for a usable
        ! item to add to, within this entry.
        saved = point()

        if (looking_at(LF//"*[^"//LF//"* "//TAB//"]", st)) then
            moved = skip_chars_forward(LF, st)
        else if (add_log_keep_changes_together) then
            ! page delimits entries for date
            call forward_page(st)
        else
            ! paragraph delimits entries for file
            moved = forward_paragraph(1_int64)
        end if

        bound = point()
        call goto_char(saved)
        ! Now insert the new line for this item.
        call re_search_forward("^\s *\* *$", found, st, bound=bound, noerror=SEARCH_NOERROR)

        if (found > 0) then
            ! Put this file name into the existing empty item.
            if (allocated(item)) call insert(item, status)
        else
            hit = .false.

            if (.not. new) then
                text = "* "
                if (allocated(item)) text = text//item
                saved_case = case_fold_search%value
                case_fold_search = .false.
                ! Don't accept `foo.bar' when looking for `foo':
                call re_search_forward(regexp_quote(text)//"\(\s \|[(),:]\)", found, st, bound=bound, &
                                       noerror=SEARCH_NOERROR)
                case_fold_search = saved_case
                hit = found > 0
            end if

            if (hit) then
                ! Add to the existing item for the same file.
                call re_search_forward("^\s *$\|^\s \*", found, st, noerror=SEARCH_NOERROR)

                if (found > 0) then
                    call goto_char(match_beginning(0, st))
                else
                    call goto_char(point_max())
                    call insert(LF, status)
                    if (.not. status%ok()) return
                end if

                ! Delete excess empty lines; make just 2.
                call delete_empty_lines(status)
                if (status%ok()) call insert(LF//LF, status)
                if (.not. status%ok()) return
                moved = forward_line(-2_int64)
                call indent_relative_first_indent_point(status, noindent)
            else
                ! Make a new item.
                do while (looking_at("\sW", st))
                    moved = forward_line(1_int64)
                    if (moved /= 0) exit
                end do

                call delete_empty_lines(status)
                if (status%ok()) call insert(LF//LF//LF, status)
                if (.not. status%ok()) return
                moved = forward_line(-2_int64)
                mincol = indent_to(left_margin%value, status)
                if (status%ok()) call insert("* ", status)

                if (allocated(item)) then
                    if (status%ok()) call insert(item, status)
                end if
            end if
        end if

        if (.not. status%ok()) return

        ! Now insert the function name, if we have one.
        ! Point is at the item for this file,
        ! either at the end of the line or at the first blank line.
        if (.not. allocated(subprogram)) then
            ! No function name, so put in a colon unless we have just a star.
            saved = point()
            call beginning_of_line()
            blank = looking_at("\s *\(\* *\)?$", st)
            call goto_char(saved)

            if (.not. blank) then
                call insert(": ", status)

                if (allocated(version)) then
                    if (status%ok()) call insert(version//" ", status)
                end if
            end if

            return
        end if

        ! Make it easy to get rid of the function name.
        call undo_boundary()
        saved = point()
        call beginning_of_line()
        blank = looking_at("\s *$", st)
        call goto_char(saved)

        if (.not. blank) then
            call insert(" ", status)
            if (.not. status%ok()) return
        end if

        ! See if the prev function name has a message yet or not.
        ! If not, merge the two items.
        call point_marker(pos)
        moved = skip_syntax_backward(" ")
        moved = skip_chars_backward("):", st)
        merge = .false.

        if (.not. on_new_line) then
            if (looking_at("):", st)) then
                here = point()
                call backward_expression(1_int64, status)

                if (status%ok()) then
                    beginning = point()
                    call goto_char(here)
                    call buffer_substring(beginning, here, text, st)

                    if (text == subprogram .and. len(text) == len(subprogram)) then
                        call delete_region(beginning, here, status)
                    end if
                end if

                if (status%ok()) then
                    merge = fill_column%value > current_column() + count_characters(subprogram) + 4
                end if
            end if
        end if

        if (status%ok()) then
            call marker_position(pos, p, st)

            if (merge) then
                moved = skip_chars_backward(", ", st)
                call delete_region(point(), p, status)
                call char_before(c, has_c)

                if (status%ok()) then
                    if (.not. has_c .or. c /= iachar("(")) call insert(", ", status)
                end if
            else
                if (.not. on_new_line) then
                    if (looking_at("):", st)) call delete_region(point() + 1, line_end_position(), status)
                end if

                call marker_position(pos, p, st)
                call goto_char(p)
                if (status%ok()) call insert("(", status)
            end if
        end if

        call destroy_marker(pos, st)
        if (.not. status%ok()) return
        call insert(subprogram//"): ", status)

        if (allocated(version)) then
            if (status%ok()) call insert(version//" ", status)
        end if

    contains

        ! (while (and (not (is_eob)) (looking-at "^\\s *$"))
        !   (delete-region (point) (line-beginning-position 2)))
        recursive subroutine delete_empty_lines(status)
            type(status_type), intent(out) :: status
            type(status_type) :: st

            status = status_type()

            do while (.not. is_eob())
                if (.not. looking_at("^\s *$", st)) exit
                call delete_region(point(), line_beginning_position(2_int64), status)
                if (.not. status%ok()) return
            end do
        end subroutine delete_empty_lines
    end subroutine add_change_log_entry

    !> Find change log file in other window and add entry and item.
    !> This is just like `add_change_log_entry' except that it displays
    !> the change log file in another window.
    !> If this command needs to split the current window, it by default obeys
    !> the user options `split_height_threshold' and `split_width_threshold',
    !> when it decides whether to split the window horizontally or vertically.
    !> Port of add-log.el:add-change-log-entry-other-window.
    recursive subroutine add_change_log_entry_other_window(status, whoami, file_name)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: whoami
        character(len=*), intent(in), optional :: file_name

        call add_change_log_entry(status, whoami, file_name, other_window=.true.)
    end subroutine add_change_log_entry_other_window

    recursive subroutine cmd_add_change_log_entry(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        call read_change_log_name(name, status)
        if (.not. status%ok()) return
        call add_change_log_entry(status, current_prefix_arg%kind /= PREFIX_NONE, name)
    end subroutine cmd_add_change_log_entry

    recursive subroutine cmd_add_change_log_entry_other_window(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        if (current_prefix_arg%kind /= PREFIX_NONE) then
            call read_change_log_name(name, status)
            if (.not. status%ok()) return
            call add_change_log_entry_other_window(status, .true., name)
        else
            call add_change_log_entry_other_window(status)
        end if
    end subroutine cmd_add_change_log_entry_other_window

    recursive subroutine cmd_find_change_log_entry_file(status)
        type(status_type), intent(out) :: status

        call find_change_log_entry_file(status)
    end subroutine cmd_find_change_log_entry_file

    !> Return name of function definition point is in, or unallocated.
    !>
    !> Other modes are handled by a heuristic that looks in the 10K before
    !> point for uppercase headings starting in the first column or
    !> identifiers followed by `:' or `='.  See variables
    !> `add_log_current_subprogram_header_regexp' and
    !> `add_log_current_subprogram_function'.
    !>
    !> Has a preference of looking backwards.  Port of
    !> add-log.el:add-log-current-defun (C, Lisp, LaTeX, Texinfo and Perl
    !> come with their modes).
    recursive subroutine add_log_current_subprogram(name)
        character(len=:), allocatable, intent(out) :: name
        procedure(add_log_name_procedure), pointer :: fn
        character(len=:), allocatable :: result
        type(status_type) :: st
        integer(int64) :: opoint, found
        logical :: saved_case, matched

        opoint = point()
        fn => add_log_current_subprogram_function%current()

        if (associated(fn)) then
            call fn(name)
        else
            ! If all else fails, try heuristics
            saved_case = case_fold_search%value
            case_fold_search = .false.
            call end_of_line()
            call re_search_backward(add_log_current_subprogram_header_regexp, found, st, &
                                    bound=max(point() - 10000, point_min()), noerror=SEARCH_NOERROR)

            if (st%ok() .and. found > 0) then
                result = match_string_no_properties(1, matched=matched)
                if (.not. matched) result = match_string_no_properties(0)

                ! Strip whitespace away
                if (string_match("\([^ "//TAB//LF//achar(13)//achar(12)//"].*[^ "//TAB//LF//achar(13)//achar(12)// &
                                 "]\)", result, st) > 0) then
                    result = match_string_no_properties(1, result)
                end if

                name = result
            end if

            case_fold_search = saved_case
        end if

        call goto_char(opoint)
    end subroutine add_log_current_subprogram

    ! ------------------------------------------------------------------
    ! Change Log mode
    ! ------------------------------------------------------------------

    !> Additional expressions to highlight in Change Log mode.  Port of
    !> add-log.el change-log-font-lock-keywords.
    subroutine change_log_font_lock_keywords(keywords)
        type(font_lock_keyword_type), allocatable, intent(out) :: keywords(:)
        character(len=*), parameter :: NAMES = "\([^(),"//LF//"]+\|(\(setf\|SETF\) [^() ,"//LF//"]+)\)"
        type(font_lock_highlight_type) :: date(2), person(2), files(4), file(1), list(1), functions(2), h(1)

        allocate (keywords(6))
        ! Date lines, new (2000-01-01) and old (Sat Jan  1 00:00:00 2000) styles.
        ! Fixme: this regexp is just an approximate one and may match
        ! wrongly with a non-date line existing as a random note.  In
        ! addition, using any kind of fixed setting like this doesn't
        ! work if a user customizes add-log-time-format.
        date(1) = font_lock_highlight(0, face="change_log_date")
        ! Name and e-mail; some people put e-mail in parens, not angles.
        person(1) = font_lock_highlight(1, face="change_log_name")
        person(2) = font_lock_highlight(2, face="change_log_email")
        date(2) = font_lock_anchored(person, regexp="\([^<(]+?\)["//" "//TAB// &
                                     "]*[(<]\([A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+\)[>)]")
        keywords(1) = font_lock_keyword(regexp="^[0-9-]+ +\|^ \{11,\}\|^"//TAB//" \{3,\}\|"// &
                                        "^\(Sun\|Mon\|Tue\|Wed\|Thu\|Fri\|Sat\) [A-Z][a-z][a-z] [0-9:+ ]+", &
                                        highlights=date)
        ! File names.
        files(1) = font_lock_highlight(2, face="change_log_file")
        ! Possibly further names in a list:
        file(1) = font_lock_highlight(1, face="change_log_file")
        files(2) = font_lock_anchored(file, regexp="\=, \([^ ,:(["//LF//"]+\)")
        ! Possibly a parenthesized list of names:
        list(1) = font_lock_highlight(1, face="change_log_list")
        files(3) = font_lock_anchored(list, regexp="\= ("//NAMES)
        files(4) = font_lock_anchored(list, regexp="\=, *"//NAMES)
        keywords(2) = font_lock_keyword(regexp=change_log_file_names_regexp, highlights=files)
        ! Function or variable names.
        functions(1) = font_lock_highlight(2, face="change_log_list")
        functions(2) = font_lock_anchored(list, regexp="\=, *"//NAMES)
        keywords(3) = font_lock_keyword(regexp="^\( +\|"//TAB//"\)("//NAMES, highlights=functions)
        ! Conditionals.
        h(1) = font_lock_highlight(1, face="change_log_conditionals")
        keywords(4) = font_lock_keyword(regexp="\[!?\([^]"//LF//"]+\)\]\(:\| (\)", highlights=h)
        ! Function of change.
        h(1) = font_lock_highlight(1, face="change_log_function")
        keywords(5) = font_lock_keyword(regexp="<\([^>"//LF//"]+\)>\(:\| (\)", highlights=h)
        ! Acknowledgments.
        ! Don't include plain "From" because that is vague;
        ! we want to encourage people to say something more specific.
        ! Note that the FSF does not use "Patches by"; our convention
        ! is to put the name of the author of the changes at the top
        ! of the change log entry.
        h(1) = font_lock_highlight(3, face="change_log_acknowledgment")
        keywords(6) = font_lock_keyword(regexp="\(^\( +\|"//TAB//"\)\|  \)\(Thanks to\|Patch\(es\)? by\|"// &
                                        "Report\(ed by\| from\)\|Suggest\(ed by\|ion from\)\)", highlights=h)
    end subroutine change_log_font_lock_keywords

    !> Major mode for editing change logs; like Indented Text mode.
    !> Prevents numeric backups and sets `left_margin' to 8 and `fill_column' to 74.
    !> New log entries are usually made with \[add_change_log_entry] or \[add_change_log_entry_other_window].
    !> Each entry behaves as a paragraph, and the entries for one day as a page.
    !> Runs `change_log_mode_hook'.
    !>
    !> \{change_log_mode_map}
    !>
    !> Port of add-log.el:change-log-mode (the settings PORTING.md's
    !> add_log_mod row lists are deferred).
    recursive subroutine change_log_mode(status)
        type(status_type), intent(out) :: status
        type(font_lock_keyword_type), allocatable :: keywords(:)
        logical :: saved

        if (.not. associated(major_modes%change_log_mode%execute)) call init_add_log()
        call major_modes%change_log_mode%begin(saved, status)
        if (.not. status%ok()) return
        call left_margin%set_local(8_int64)
        call fill_column%set_local(74_int64)
        call indent_tabs_mode%set_local(.true.)
        call tab_width%set_local(8_int64)
        ! We really do want "^" in paragraph-start below: it is only the
        ! lines that begin at column 0 (despite the left-margin of 8) that
        ! we are looking for.  Adding `* ' allows eliding the blank line
        ! between entries for different files.
        call paragraph_start%set_local("\s *$\|"//achar(12)//"\|^\<")
        call paragraph_separate%set_local(paragraph_start%value)
        ! Match null string on the date-line so that the date-line
        ! is grouped with what follows.
        call page_delimiter%set_local("^\<\|^"//achar(12))
        call change_log_font_lock_keywords(keywords)
        call font_lock_defaults%set_local(keywords=keywords, keywords_only=.true.)
        call major_modes%change_log_mode%end(saved, status)
    end subroutine change_log_mode

    !> The faces of add-log.el.  Not a port.
    subroutine define_faces()
        type(named_face_type), pointer :: f

        f => change_log_date
        call f%define("change_log_date", "Face used to highlight dates in date lines.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_string_face"))
        f => change_log_name_face
        call f%define("change_log_name", "Face for highlighting author names.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_constant_face"))
        f => change_log_email
        call f%define("change_log_email", "Face for highlighting author email addresses.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_variable_name_face"))
        f => change_log_file
        call f%define("change_log_file", "Face for highlighting file names.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_function_name_face"))
        f => change_log_list
        call f%define("change_log_list", "Face for highlighting parenthesized lists of functions or variables.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_keyword_face"))
        f => change_log_conditionals
        call f%define("change_log_conditionals", "Face for highlighting conditionals of the form `[...]'.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_variable_name_face"))
        f => change_log_function
        call f%define("change_log_function", "Face for highlighting items of the form `<....>'.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_variable_name_face"))
        f => change_log_acknowledgment
        call f%define("change_log_acknowledgment", "Face for highlighting acknowledgments.")
        call f%spec_entry(display_when(), face_attrs(inherit="font_lock_comment_face"))
    end subroutine define_faces

    !> Define Change Log mode, its syntax table, keys and faces, the
    !> commands, and (with CTL_X_4_MAP) C-x 4 a.  Not a port.
    subroutine init_add_log(ctl_x_4_map)
        type(handle_type), intent(in), optional :: ctl_x_4_map
        type(status_type) :: st

        call define_faces()

        if (.not. allocated(change_log_directory_files)) then
            change_log_directory_files = [character(len=8) :: ".bzr", ".git", ".hg", ".svn"]
        end if

        if (.not. allocated(change_log_version_number_regexps)) then
            !  (defconst ad-version "2.15"
            ! Revision: pcl-cvs.el,v 1.72 1999/09/05 20:21:54 monnier Exp
            change_log_version_number_regexps = [character(len=64) :: &
                                                 "^(def[^ "//TAB//LF//"]+[ "//TAB//"]+[^ "//TAB//LF//"][ "//TAB// &
                                                 """\([0-9]+\.[0-9.]+\)", &
                                                 "^;+ *Revision: +[^ "//TAB//LF//"]+[ "//TAB//"]+\([0-9]+\.[0-9.]+\)"]
        end if

        if (.not. allocated(add_log_current_subprogram_header_regexp)) then
            add_log_current_subprogram_header_regexp = "^\([[:upper:]][[:upper:]_ ]*[[:upper:]_]\|"// &
                                                       "[-_[:alnum:]]*[[:alpha:]][-_[:alnum:]]*\)[ "//TAB//"]*[:=]"
        end if

        call forward_setting(change_log_default_name, local_if_set=.true.)
        call append_locals_clearer(clear_add_log_locals)

        if (change_log_mode_syntax_table%index == 0) then
            call make_syntax_table(change_log_mode_syntax_table, st)
            call modify_syntax_entry(iachar("`"), iachar("`"), "'   ", st, change_log_mode_syntax_table)
            call modify_syntax_entry(iachar("'"), iachar("'"), "'   ", st, change_log_mode_syntax_table)
        end if

        call major_modes%change_log_mode%define("change_log_mode", "Change Log", change_log_mode, &
                                                change_log_mode_hook, parent=major_modes%text_mode, &
                                                syntax_table=change_log_mode_syntax_table)
        change_log_mode_map = major_modes%change_log_mode%map
        commands%change_log_mode%execute => change_log_mode
        commands%add_change_log_entry%execute => cmd_add_change_log_entry
        commands%add_change_log_entry_other_window%execute => cmd_add_change_log_entry_other_window
        commands%find_change_log_entry_file%execute => cmd_find_change_log_entry_file
        call keymap_set(change_log_mode_map, "C-c C-f", "find_change_log_entry_file", st)
        if (present(ctl_x_4_map)) call keymap_set(ctl_x_4_map, "a", "add_change_log_entry_other_window", st)
    end subroutine init_add_log
end module add_log_mod
