! recent_files_mod.f90 -- keep track of recently opened files.
! Ported from GNU Emacs 31.1 lisp/recentf.el.
!
! Copyright (C) 1999-2026 Free Software Foundation, Inc.
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

!> This package maintains a list of recently opened files and makes it
!> easy to visit them.  The recent files list is automatically saved
!> across Fortmacs sessions.  To enable it, turn on `recent_files_mode'; visit
!> one of the files with `recent_files_open'.
!>
!> The list is saved in `recent_files_save_file' in namelist syntax
!> (namelist_mod):
!>
!>     &recentf
!>      recent_files_files(1:2) = "~/notes.txt", "~/src/Fortmacs/src/rectangle_mod.f90",
!>     /
!>
!> `recent_files_exclude' and `recent_files_keep' are arrays of
!> `recent_files_check_type' records, each a regexp or a test;
!> `recent_files_filename_handlers' is an array of `recent_files_filename_handler_type'.
!>
!> Deferred: the "Open Recent" menu with its filters (`recent_files_menu_filter'
!> and the predefined and rule based filters; there is no menu bar), the
!> dialogs `recent_files_edit_list', `recent_files_open_files' and
!> `recent_files_open_more_files' (widget.el is not ported) and with them
!> `recent_files_filter_changer_current' in the saved file and the digit
!> shortcuts; remote files (`recent_files_access_file'); tracking files as
!> they are written (`write_file_functions' is not ported); a time
!> string in `recent_files_auto_cleanup'; the warning when the list can't be
!> saved (warnings.el); the obsolete `recent_files_load_hook',
!> `recent_files_trunc_list' and `recent_files_elements'.
module recent_files_mod
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE
    use handle_mod, only: handle_type
    use value_mod, only: value_type, make_list
    use data_mod, only: set, symbol_value, choice_setting_type
    use hooks_mod, only: hook_type
    use modes_mod, only: minor_modes
    use commands_mod, only: commands
    use timer_mod, only: execute_with_timer, execute_with_idle_timer, destroy_timer, is_timer
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_or_make_buffer, generate_new_buffer_name, &
                          close_buffer
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: buffer_string
    use file_input_output_mod, only: is_file_readable, expand_file_name, write_region, insert_file_contents, set_file_modes
    use files_mod, only: find_file, find_file_hook, close_buffer_hook, abbreviate_file_name, file_truename, &
                         file_name_extension
    use directory_editor_mod, only: completion_ignored_extensions
    use configuration_mod, only: state_directory
    use coding_mod, only: coding_system_for_write
    use search_mod, only: string_match
    use keyboard_mod, only: exit_fortmacs_hook
    use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value
    use minibuffer_reading_tables_mod, only: completion_table_type, TABLE_LIST
    use minibuffer_mod, only: completing_read, REQUIRE_MATCH_YES
    use easy_minor_mode_mod, only: mode_called_interactively
    use display_engine_mod, only: message
    use namelist_mod, only: namelist_group_type, namelist_item_type, read_namelist, namelist_text, NAMELIST_STRING
    implicit none(type, external)
    private

    public :: recent_files_mode, is_recent_files_enabled, is_recent_files_exclude_file_by_extension, &
              recent_files_keep_default_test, &
              recent_files_add_file, recent_files_remove_if_non_kept, is_recent_files_include, is_recent_files_keep, &
              recent_files_push, &
              recent_files_expand_file_name, recent_files_apply_filename_handlers, recent_files_open, &
              recent_files_open_most_recent_file, &
              recent_files_save_list, recent_files_load_list, recent_files_cleanup, recent_files_track_opened_file, &
              recent_files_track_closed_file, abbreviate_file_name_handler, file_truename_handler, initialize_recent_files

    !> A recently opened file.
    type, public :: recent_files_file_type
        character(len=:), allocatable :: name
    end type recent_files_file_type

    !> List of recently opened files for `recent_files_mode'.  Port of
    !> recentf.el recentf-list.
    type(recent_files_file_type), allocatable, public :: recent_files_files(:)

    abstract interface
        !> A test on a file name for `recent_files_exclude' and `recent_files_keep'.
        !> Not a port.
        logical function recent_files_test_procedure(file)
            implicit none(type, external)
            character(len=*), intent(in) :: file
        end function recent_files_test_procedure

        !> A function of `recent_files_filename_handlers': NAME transformed.
        !> Not a port.
        subroutine recent_files_filename_handler_procedure(name, result, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: name
            character(len=:), allocatable, intent(out) :: result
            type(status_type), intent(out) :: status
        end subroutine recent_files_filename_handler_procedure

        !> `recent_files_menu_action': open FILE.  Not a port.
        recursive subroutine recent_files_action_procedure(file, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: file
            type(status_type), intent(out) :: status
        end subroutine recent_files_action_procedure
    end interface

    !> An element of `recent_files_exclude' or `recent_files_keep': a REGULAR_EXPRESSION, or a
    !> TEST (when REGULAR_EXPRESSION is blank).
    type, public :: recent_files_check_type
        character(len=256) :: regular_expression = ""
        procedure(recent_files_test_procedure), pointer, nopass :: test => null()
    end type recent_files_check_type

    !> An element of `recent_files_filename_handlers'.
    type, public :: recent_files_filename_handler_type
        procedure(recent_files_filename_handler_procedure), pointer, nopass :: handler => null()
    end type recent_files_filename_handler_type

    !> `recent_files_maximum_saved_items' for no limit.
    integer, parameter, public :: RECENT_FILES_MAXIMUM_SAVED_ITEMS_UNLIMITED = huge(0)

    !> Maximum number of items of the recent list that will be saved.
    !> RECENT_FILES_MAXIMUM_SAVED_ITEMS_UNLIMITED means to save the whole list.
    !> See the command `recent_files_save_list'.  Port of recentf.el
    !> recentf-max-saved-items.
    integer, public :: recent_files_maximum_saved_items = 25

    !> File to save the recent list into.  Port of recentf.el
    !> recentf-save-file (in the state directory,
    !> `~/.local/state/fortmacs/recentf').
    character(len=:), allocatable, public :: recent_files_save_file

    !> Mode bits of recentf save file, as an integer, or -1.
    !> If not -1, after writing `recent_files_save_file', set its mode bits to
    !> this value.  This is decimal, not octal.  The default is 384 (0600
    !> in octal), which gives R/W access only to the user who owns that
    !> file.  See also the function `set_file_modes'.  Port of recentf.el
    !> recentf-save-file-modes (-1 for none).
    integer, public :: recent_files_save_file_modes = 384

    !> List of regexps and tests for filenames excluded from the
    !> recent list.  When a filename matches any of the regexps or
    !> satisfies any of the tests it is excluded from the recent
    !> list.  A test is a function that is passed a filename to check
    !> and that must return true to exclude it.  Port of recentf.el
    !> recentf-exclude.
    type(recent_files_check_type), allocatable, public :: recent_files_exclude(:)

    !> List of file-name extensions that `recent_files' should ignore.
    !> This has effect only if the function
    !> `is_recent_files_exclude_file_by_extension' is in the list that is the
    !> value of `recent_files_exclude'.  If this option is empty and
    !> `is_recent_files_exclude_file_by_extension' is a member of
    !> `recent_files_exclude', a file will be ignored if its extension is in
    !> `completion_ignored_extensions', which see.  Port of recentf.el
    !> recentf-exclude-ignored-extensions.
    character(len=32), allocatable, public :: recent_files_exclude_ignored_extensions(:)

    !> List of regexps and tests for filenames kept in the recent
    !> list.  Regexps and tests are tried in the specified order.
    !> When empty all filenames are kept in the recent list.
    !> When a filename matches any of the regexps or satisfies any of the
    !> tests it is kept in the recent list.
    !> The default is to keep readable files.
    !> A test is a function that is passed a filename to check and
    !> that must return true to keep it.  Port of recentf.el recentf-keep.
    type(recent_files_check_type), allocatable, public :: recent_files_keep(:)

    !> Function to invoke with a filename item of the recentf menu.
    !> The default is to call `find_file' to edit the selected file.  Port
    !> of recentf.el recentf-menu-action.
    procedure(recent_files_action_procedure), pointer, public :: recent_files_menu_action => find_file

    !> The setting `recent_files_auto_cleanup': .false., .true. or a number of
    !> seconds (R6-39: `recent_files_auto_cleanup%seconds = 300'; negative
    !> when no number is set).  Not a port.
    type, extends(choice_setting_type), public :: recent_files_auto_cleanup_choice_setting_type
        integer :: seconds = -1
    contains
        procedure :: clear_kinds => recent_files_auto_cleanup_clear_kinds
        procedure :: other_value => recent_files_auto_cleanup_other_value
        procedure :: kind_hint => recent_files_auto_cleanup_kind_hint
    end type recent_files_auto_cleanup_choice_setting_type

    !> Define when to automatically cleanup the recent list.
    !> That is, remove duplicates, non-kept, and excluded files.
    !>
    !> The following values can be set:
    !>
    !> - .true.
    !>     Cleanup when turning the mode on (default).
    !> - .false.
    !>     Never cleanup the list automatically.
    !> - A number of seconds (`recent_files_auto_cleanup%seconds')
    !>     Cleanup each time Fortmacs has been idle that number of seconds.
    !>
    !> See also the command `recent_files_cleanup', that can be used to
    !> manually cleanup the list.  Port of recentf.el recentf-auto-cleanup
    !> (a time string is Deferred).
    type(recent_files_auto_cleanup_choice_setting_type), public :: recent_files_auto_cleanup = &
                                                   recent_files_auto_cleanup_choice_setting_type(name="recent_files_auto_cleanup", &
                                                                                                                 value=.true.)

    !> True means to initialize `file_name_history' with the recent list.
    !> If `file_name_history' is not empty, do nothing.  Port of
    !> recentf.el recentf-initialize-file-name-history.
    logical, public :: recent_files_initialize_file_name_history = .true.

    !> Functions to post process recent file names.
    !> They are successively passed a file name to transform it.  Port of
    !> recentf.el recentf-filename-handlers (the default holds
    !> `abbreviate_file_name_handler', for `abbreviate_file_name').
    type(recent_files_filename_handler_type), allocatable, public :: recent_files_filename_handlers(:)

    !> Whether to show verbose messages about low-level recentf actions.
    !> False means to not show messages related to the recentf machinery.
    !> True means show messages that were printed by default on Emacs <=
    !> 31.1.  Port of recentf.el recentf-show-messages.
    logical, public :: recent_files_show_messages = .true.

    !> The interval between auto saves of recently opened files, in
    !> seconds.  If set to 0, disables timer-based auto saving.  Port of
    !> recentf.el recentf-autosave-interval (0 for none).
    integer, public :: recent_files_autosave_interval = 0

    !> True if recentf searches and matches should ignore case (false on
    !> GNU/Linux).  Port of recentf.el recentf-case-fold-search.
    logical, parameter, public :: recent_files_case_fold_search = .false.

    !> Hook run after entering or leaving `recent_files_mode'.
    type(hook_type), target, public :: recent_files_mode_hook = hook_type(name="recent_files_mode_hook")

    !> Keymap to use in `recent_files_mode'.  Port of recentf.el
    !> recentf-mode-map.
    type(handle_type), public :: recent_files_mode_map

    !> Timer used to automatically cleanup the recent list.
    !> See also the option `recent_files_auto_cleanup'.  Port of recentf.el
    !> recentf-auto-cleanup-timer.
    type(handle_type) :: recent_files_auto_cleanup_timer
    type(handle_type) :: recent_files_autosave_timer

contains

    !> Return true if `recent_files_mode' is currently enabled.  Port of
    !> recentf.el:recentf-enabled-p.
    logical function is_recent_files_enabled()
        is_recent_files_enabled = exit_fortmacs_hook%member(recent_files_save_list_adapter)
    end function is_recent_files_enabled

    !> Whether to ignore FILE_NAME due to its file-name extension.
    !> This test function is meant to be added to the list in
    !> `recent_files_exclude'.  It returns true if FILE_NAME's extension is in
    !> the list that is the value of `recent_files_exclude_ignored_extensions';
    !> if that variable is empty, this function consults
    !> `completion_ignored_extensions' instead.  Port of
    !> recentf.el:recentf-exclude-file-by-extension-p.
    logical function is_recent_files_exclude_file_by_extension(file_name) result(ignored)
        character(len=*), intent(in) :: file_name
        character(len=:), allocatable :: extension, dotted
        logical :: found
        integer :: k

        ignored = .false.
        call file_name_extension(file_name, extension, found)
        if (.not. found) return
        dotted = "."//extension

        if (allocated(recent_files_exclude_ignored_extensions)) then
            if (size(recent_files_exclude_ignored_extensions) > 0) then
                do k = 1, size(recent_files_exclude_ignored_extensions)
                    if (ends_with(dotted, trim(recent_files_exclude_ignored_extensions(k)))) ignored = .true.
                end do

                return
            end if
        end if

        do k = 1, size(completion_ignored_extensions)
            if (ends_with(dotted, trim(completion_ignored_extensions(k)))) ignored = .true.
        end do

    contains

        !> Whether S ends with SUFFIX (the regexp-opt of the extensions
        !> anchored at the end).
        pure logical function ends_with(s, suffix)
            character(len=*), intent(in) :: s, suffix

            ends_with = len(suffix) > 0 .and. len(suffix) <= len(s)
            if (ends_with) ends_with = s(len(s) - len(suffix) + 1:) == suffix
        end function ends_with
    end function is_recent_files_exclude_file_by_extension

    !> Return true if FILE should be kept in the recent list.  Port of
    !> recentf.el:recentf-keep-default-predicate (no remote files).
    logical function recent_files_keep_default_test(file) result(keep)
        character(len=*), intent(in) :: file

        keep = is_file_readable(file)
    end function recent_files_keep_default_test

    !> Return true if strings S1 and S2 have identical contents.  Port of
    !> recentf.el:recentf-string-equal (`recent_files_case_fold_search' is
    !> false).
    pure logical function recent_files_string_equal(s1, s2) result(same)
        character(len=*), intent(in) :: s1, s2

        same = len(s1) == len(s2)
        if (same) same = s1 == s2
    end function recent_files_string_equal

    !> The index in `recent_files_files' of ELEMENT, 0 if it is not there.  Port of
    !> recentf.el:recentf-string-member.
    integer function recent_files_string_member(element) result(k)
        character(len=*), intent(in) :: element

        if (allocated(recent_files_files)) then
            do k = 1, size(recent_files_files)
                if (recent_files_string_equal(element, recent_files_files(k)%name)) return
            end do
        end if

        k = 0
    end function recent_files_string_member

    !> Remove element K of `recent_files_files'.  Not a port.
    subroutine delete_entry(k)
        integer, intent(in) :: k
        type(recent_files_file_type), allocatable :: kept(:)
        integer :: j

        allocate (kept(size(recent_files_files) - 1))

        do j = 1, size(kept)
            call move_alloc(recent_files_files(merge(j, j + 1, j < k))%name, kept(j)%name)
        end do

        call move_alloc(kept, recent_files_files)
    end subroutine delete_entry

    !> Push FILENAME into the recent list, if it isn't there yet.
    !> If it is there yet, move it at the beginning of the list.  Port of
    !> recentf.el:recentf-push.
    subroutine recent_files_push(filename)
        character(len=*), intent(in) :: filename
        type(recent_files_file_type) :: file
        integer :: k

        if (.not. allocated(recent_files_files)) allocate (recent_files_files(0))
        k = recent_files_string_member(filename)
        if (k > 0) call delete_entry(k)
        file%name = filename
        recent_files_files = [file, recent_files_files]
    end subroutine recent_files_push

    !> Apply `recent_files_filename_handlers' to file NAME.
    !> Return the transformed file name, or NAME if any handler failed.
    !> Port of recentf.el:recentf-apply-filename-handlers.
    function recent_files_apply_filename_handlers(name) result(filename)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: filename, next
        type(status_type) :: st
        integer :: k

        filename = name
        if (.not. allocated(recent_files_filename_handlers)) return

        do k = 1, size(recent_files_filename_handlers)
            if (.not. associated(recent_files_filename_handlers(k)%handler)) cycle
            call recent_files_filename_handlers(k)%handler(filename, next, st)

            if (.not. st%ok()) then
                filename = name
                return
            end if

            filename = next
        end do
    end function recent_files_apply_filename_handlers

    !> Convert file NAME to absolute, and canonicalize it.
    !> NAME is first passed to the function `expand_file_name', then to
    !> `recent_files_filename_handlers' to post process it.  Port of
    !> recentf.el:recentf-expand-file-name.
    function recent_files_expand_file_name(name) result(filename)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: filename, expanded
        type(status_type) :: st

        call expand_file_name(name, expanded, st)
        if (.not. st%ok()) expanded = name
        filename = recent_files_apply_filename_handlers(expanded)
    end function recent_files_expand_file_name

    !> Whether FILENAME matches CHECK (a regexp, or a test).
    !> Not a port.
    logical function is_matching(check, filename) result(matches)
        type(recent_files_check_type), intent(in) :: check
        character(len=*), intent(in) :: filename
        type(status_type) :: st

        if (len_trim(check%regular_expression) > 0) then
            matches = string_match(trim(check%regular_expression), filename, st) > 0
            if (.not. st%ok()) matches = .false.
        else if (associated(check%test)) then
            matches = check%test(filename)
        else
            matches = .false.
        end if
    end function is_matching

    !> Return true if FILENAME should be included in the recent list.
    !> That is, if it doesn't match any of the `recent_files_exclude' checks.
    !> Port of recentf.el:recentf-include-p.
    logical function is_recent_files_include(filename) result(keepit)
        character(len=*), intent(in) :: filename
        integer :: k

        keepit = .true.
        if (.not. allocated(recent_files_exclude)) return

        do k = 1, size(recent_files_exclude)
            if (is_matching(recent_files_exclude(k), filename)) then
                keepit = .false.
                return
            end if
        end do
    end function is_recent_files_include

    !> Return true if FILENAME should be kept in the recent list.
    !> That is, if it matches any of the `recent_files_keep' checks.  Port of
    !> recentf.el:recentf-keep-p.
    logical function is_recent_files_keep(filename) result(keepit)
        character(len=*), intent(in) :: filename
        integer :: k

        keepit = .true.
        if (.not. allocated(recent_files_keep)) return
        if (size(recent_files_keep) == 0) return
        keepit = .false.

        do k = 1, size(recent_files_keep)
            if (is_matching(recent_files_keep(k), filename)) then
                keepit = .true.
                return
            end if
        end do
    end function is_recent_files_keep

    !> Add or move FILENAME at the beginning of the recent list.
    !> Does nothing if the name satisfies any of the `recent_files_exclude'
    !> regexps or tests.  Port of recentf.el:recentf-add-file.
    subroutine recent_files_add_file(filename)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: name

        name = recent_files_expand_file_name(filename)
        if (is_recent_files_include(name)) call recent_files_push(name)
    end subroutine recent_files_add_file

    !> Remove FILENAME from the recent list, if file is not kept.
    !> REMOVED says whether FILENAME has been removed.  Port of
    !> recentf.el:recentf-remove-if-non-kept.
    subroutine recent_files_remove_if_non_kept(filename, removed)
        character(len=*), intent(in) :: filename
        logical, intent(out), optional :: removed
        integer :: k

        if (present(removed)) removed = .false.
        if (is_recent_files_keep(filename)) return
        k = recent_files_string_member(recent_files_expand_file_name(filename))
        if (k == 0) return
        call delete_entry(k)
        if (present(removed)) removed = .true.
    end subroutine recent_files_remove_if_non_kept

    !> `abbreviate_file_name' as a `recent_files_filename_handlers' element.
    !> Not a port.
    subroutine abbreviate_file_name_handler(name, result, status)
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status

        status = status_type()
        result = abbreviate_file_name(name)
    end subroutine abbreviate_file_name_handler

    !> file-truename as a `recent_files_filename_handlers' element.  Not a port.
    subroutine file_truename_handler(name, result, status)
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status

        call file_truename(name, result, status)
    end subroutine file_truename_handler

    !> Automatic cleanup of the recent list.  Port of
    !> recentf.el:recentf-auto-cleanup.
    subroutine execute_recent_files_auto_cleanup()
        type(status_type) :: st

        if (is_timer(recent_files_auto_cleanup_timer)) call destroy_timer(recent_files_auto_cleanup_timer)
        recent_files_auto_cleanup_timer = handle_type()
        if (.not. minor_modes%recent_files_mode%enabled()) return

        if (recent_files_auto_cleanup%seconds > 0) then
            call execute_with_idle_timer(recent_files_auto_cleanup_timer, real(recent_files_auto_cleanup%seconds, real64), .true., &
                                         recent_files_cleanup_adapter, st)
        else if (recent_files_auto_cleanup%value) then
            call recent_files_cleanup()
        end if
    end subroutine execute_recent_files_auto_cleanup

    !> Prompt for FILE in `recent_files_files' and visit it.
    !> Enable `recent_files_mode' if it isn't already.  Port of
    !> recentf.el:recentf-open (also `recent_files').
    recursive subroutine recent_files_open(status)
        type(status_type), intent(out) :: status
        type(completion_table_type) :: table
        type(value_type), allocatable :: names(:)
        character(len=:), allocatable :: file
        integer :: k

        if (.not. minor_modes%recent_files_mode%enabled()) then
            call minor_modes%recent_files_mode%enable(status)
            if (.not. status%ok()) return
        end if

        if (.not. allocated(recent_files_files)) allocate (recent_files_files(0))
        allocate (names(size(recent_files_files)))

        do k = 1, size(recent_files_files)
            names(k) = value_type(text=recent_files_files(k)%name)
        end do

        table%kind = TABLE_LIST
        table%list = make_list(names)
        table%category = "recentf"
        call completing_read("Open recent file: ", table, file, status, require_match=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        call recent_files_menu_action(file, status)
    end subroutine recent_files_open

    !> Open the Nth most recent file.
    !> Optional argument N must be a valid digit number.  It defaults to 1.
    !> 1 opens the most recent file, 2 the second most recent one, etc..
    !> 0 opens the tenth most recent file.  Port of
    !> recentf.el:recentf-open-most-recent-file.
    recursive subroutine recent_files_open_most_recent_file(n_in, status)
        integer(int64), intent(in) :: n_in
        type(status_type), intent(out) :: status
        integer(int64) :: n
        character(len=24) :: buffer

        n = n_in

        if (n == 0) then
            n = 10
        else if (.not. (n > 0 .and. n < 10)) then
            write (buffer, "(i0)") n
            call set_error(status, GENERIC_ERROR_CODE, "Recent file number out of range [0-9], "//trim(buffer))
            return
        end if

        if (.not. allocated(recent_files_files)) allocate (recent_files_files(0))

        if (n > size(recent_files_files)) then
            call set_error(status, GENERIC_ERROR_CODE, "Not that many recent files")
            return
        end if

        call recent_files_menu_action(recent_files_files(n)%name, status)
    end subroutine recent_files_open_most_recent_file

    recursive subroutine execute_recent_files_open_most_recent_file(status)
        type(status_type), intent(out) :: status

        call recent_files_open_most_recent_file(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_recent_files_open_most_recent_file

    !> Insert the name of the file just opened or written into the recent
    !> list.  Port of recentf.el:recentf-track-opened-file.
    recursive subroutine recent_files_track_opened_file(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (allocated(buffers(current)%filename)) call recent_files_add_file(buffers(current)%filename)
    end subroutine recent_files_track_opened_file

    !> Update the recent list when a buffer is closed.
    !> That is, remove a non kept file from the recent list.  Port of
    !> recentf.el:recentf-track-closed-file.
    recursive subroutine recent_files_track_closed_file(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (allocated(buffers(current)%filename)) call recent_files_remove_if_non_kept(buffers(current)%filename)
    end subroutine recent_files_track_closed_file

    !> Cancel `recent_files_autosave_timer', if set.  Port of
    !> recentf.el:recentf--cancel-autosave-timer.
    subroutine recent_files_cancel_autosave_timer()
        if (is_timer(recent_files_autosave_timer)) call destroy_timer(recent_files_autosave_timer)
        recent_files_autosave_timer = handle_type()
    end subroutine recent_files_cancel_autosave_timer

    !> Set or cancel an invocation of `recent_files_save_list' on a timer.
    !> If `recent_files_mode' is enabled, set the timer, otherwise cancel the
    !> timer.  Port of recentf.el:recentf--manage-autosave-timer.
    subroutine recent_files_manage_autosave_timer()
        type(status_type) :: st

        if (minor_modes%recent_files_mode%enabled() .and. recent_files_autosave_interval > 0 .and. &
            .not. is_timer(recent_files_autosave_timer)) then
            call execute_with_timer(recent_files_autosave_timer, real(recent_files_autosave_interval, real64), &
                                    recent_files_save_list_adapter, st, real(recent_files_autosave_interval, real64))
        else
            call recent_files_cancel_autosave_timer()
        end if
    end subroutine recent_files_manage_autosave_timer

    !> Save the recent list.
    !> Write data into the file specified by `recent_files_save_file'.  Port of
    !> recentf.el:recentf-save-list (INTERACTIVE says it was called as a
    !> command).
    recursive subroutine recent_files_save_list(interactive)
        logical, intent(in), optional :: interactive
        type(namelist_group_type) :: group
        type(namelist_item_type) :: item
        type(handle_type) :: old, h
        type(status_type) :: st, wst
        character(len=:), allocatable :: file, wrote
        logical :: inter
        integer :: k, n

        inter = .false.
        if (present(interactive)) inter = interactive
        if (.not. allocated(recent_files_files)) allocate (recent_files_files(0))
        group%name = "recentf"
        n = min(size(recent_files_files), max(recent_files_maximum_saved_items, 0))

        if (n == 0) then
            item = namelist_item_type(name="recent_files_files")
            call item%append_logical(.false.)
        else
            item = namelist_item_type(name="recent_files_files", subscripted=.true., lower=1, upper=n)

            do k = 1, n
                call item%append_string(recent_files_files(k)%name)
            end do
        end if

        call group%append(item)
        call expand_file_name(recent_files_save_file, file, st)
        if (.not. st%ok()) file = recent_files_save_file
        old = current_buffer()
        call get_or_make_buffer(generate_new_buffer_name(" *temp*"), h, st)
        call set_buffer(h, st)
        call insert("! Automatically generated by `recentf'."//achar(10)//namelist_text(group), st)
        coding_system_for_write = "utf-8-emacs"
        call write_region(file, .false., wst, message=wrote)
        call coding_system_for_write%clear()
        call close_buffer(h, st)
        call set_buffer(old, st)
        ! (The warning when it fails is Deferred: warnings.el is not ported.)
        if (.not. wst%ok()) return
        if (inter .or. recent_files_show_messages) call message(wrote)
        if (recent_files_save_file_modes >= 0) call set_file_modes(recent_files_save_file, recent_files_save_file_modes, st)
    end subroutine recent_files_save_list

    ! Not a port.
    recursive subroutine recent_files_save_list_adapter(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call recent_files_save_list()
    end subroutine recent_files_save_list_adapter

    recursive subroutine execute_recent_files_save_list(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call recent_files_save_list(.true.)
    end subroutine execute_recent_files_save_list

    !> Load a previously saved recent list.
    !> Read data from the file specified by `recent_files_save_file'.
    !> When `recent_files_initialize_file_name_history' is true, initialize an
    !> empty `file_name_history' with the recent list.  Port of
    !> recentf.el:recentf-load-list.
    recursive subroutine recent_files_load_list(status)
        type(status_type), intent(out) :: status
        type(namelist_group_type), allocatable :: groups(:)
        type(value_type) :: history
        type(value_type), allocatable :: names(:)
        type(status_type) :: st
        character(len=:), allocatable :: file, text
        integer :: g, k, j

        status = status_type()
        call expand_file_name(recent_files_save_file, file, st)
        if (.not. st%ok()) file = recent_files_save_file
        if (.not. is_file_readable(file)) return
        call file_text(file, text, status)
        if (.not. status%ok()) return
        call read_namelist(text, groups, status)
        if (.not. status%ok()) return

        do g = 1, size(groups)
            if (groups(g)%name /= "recentf" .or. .not. allocated(groups(g)%items)) cycle

            do k = 1, size(groups(g)%items)
                associate (it => groups(g)%items(k))
                    if (it%name /= "recent_files_files") cycle
                    if (allocated(recent_files_files)) deallocate (recent_files_files)
                    allocate (recent_files_files(0))
                    if (.not. it%subscripted) cycle
                    deallocate (recent_files_files)
                    allocate (recent_files_files(size(it%values)))

                    do j = 1, size(it%values)
                        recent_files_files(j)%name = ""
                        if (it%values(j)%kind == NAMELIST_STRING) recent_files_files(j)%name = it%values(j)%string
                    end do
                end associate
            end do
        end do

        if (.not. recent_files_initialize_file_name_history) return
        call symbol_value(value_type(name="file_name_history"), history, st)
        if (allocated(history%a)) return
        allocate (names(size(recent_files_files)))

        do k = 1, size(recent_files_files)
            names(k) = value_type(text=abbreviate_file_name(recent_files_files(k)%name))
        end do

        call set(value_type(name="file_name_history"), make_list(names), st)
    end subroutine recent_files_load_list

    recursive subroutine execute_recent_files_load_list(status)
        type(status_type), intent(out) :: status

        call recent_files_load_list(status)
    end subroutine execute_recent_files_load_list

    !> The text of FILE.  Not a port.
    recursive subroutine file_text(file, text, status)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: text
        type(status_type), intent(out) :: status
        type(handle_type) :: old, h
        type(status_type) :: st
        integer(int64) :: inserted

        text = ""
        old = current_buffer()
        call get_or_make_buffer(generate_new_buffer_name(" *temp*"), h, st)
        call set_buffer(h, st)
        call insert_file_contents(file, .false., inserted, status)
        if (status%ok()) text = buffer_string()
        call close_buffer(h, st)
        call set_buffer(old, st)
    end subroutine file_text

    !> Cleanup the recent list.
    !> That is, remove duplicates, non-kept, and excluded files.  Port of
    !> recentf.el:recentf-cleanup.
    recursive subroutine recent_files_cleanup()
        type(recent_files_file_type), allocatable :: newlist(:)
        character(len=:), allocatable :: f
        character(len=12) :: buffer
        integer :: n, k, m, j
        logical :: dup

        call message("Cleaning up the recentf list...")
        if (.not. allocated(recent_files_files)) allocate (recent_files_files(0))
        allocate (newlist(size(recent_files_files)))
        n = 0
        m = 0

        do k = 1, size(recent_files_files)
            f = recent_files_expand_file_name(recent_files_files(k)%name)
            dup = .false.

            do j = 1, m
                if (recent_files_string_equal(newlist(j)%name, f)) then
                    dup = .true.
                    exit
                end if
            end do

            if (is_recent_files_include(f) .and. is_recent_files_keep(f) .and. .not. dup) then
                m = m + 1
                newlist(m)%name = f
            else
                n = n + 1
                call message("File "//f//" removed from the recentf list")
            end if
        end do

        write (buffer, "(i0)") n
        call message("Cleaning up the recentf list...done ("//trim(buffer)//" removed)")
        if (allocated(recent_files_files)) deallocate (recent_files_files)
        allocate (recent_files_files(m))

        do k = 1, m
            call move_alloc(newlist(k)%name, recent_files_files(k)%name)
        end do
    end subroutine recent_files_cleanup

    ! Not a port.
    recursive subroutine recent_files_cleanup_adapter(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call recent_files_cleanup()
    end subroutine recent_files_cleanup_adapter

    !> Toggle keeping track of opened files (Recentf mode).
    !> This mode maintains a list of recently opened files and makes it
    !> easy to visit them.  The recent files list is automatically saved
    !> across Fortmacs sessions.
    !>
    !> You can use `recent_files_open' to visit files.
    !>
    !> By default, only operations like opening a file, writing a buffer
    !> to a file, and closing a buffer is counted as "operating" on the
    !> file.  Port of recentf.el:recentf-mode (no "Open Recent" menu).
    recursive subroutine recent_files_mode(status)
        type(status_type), intent(out) :: status

        call minor_modes%recent_files_mode%command(status)
    end subroutine recent_files_mode

    !> The body of Recentf mode.  Port of recentf.el:recentf-mode (its
    !> body).
    recursive subroutine recent_files_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status

        status = status_type()
        if (enable .and. is_recent_files_enabled()) return

        if (enable) then
            call recent_files_load_list(status)
        else
            call recent_files_save_list(mode_called_interactively)
        end if

        call execute_recent_files_auto_cleanup()
        call recent_files_manage_autosave_timer()

        if (enable) then
            call find_file_hook%prepend(recent_files_track_opened_file)
            call close_buffer_hook%prepend(recent_files_track_closed_file)
            call exit_fortmacs_hook%prepend(recent_files_save_list_adapter)
        else
            call find_file_hook%remove(recent_files_track_opened_file)
            call close_buffer_hook%remove(recent_files_track_closed_file)
            call exit_fortmacs_hook%remove(recent_files_save_list_adapter)
        end if
    end subroutine recent_files_mode_body

    !> Define the mode, the commands and the options' defaults.  Not a port.
    subroutine initialize_recent_files()
        if (.not. allocated(recent_files_save_file)) recent_files_save_file = state_directory()//"recentf"
        if (.not. allocated(recent_files_files)) allocate (recent_files_files(0))
        if (.not. allocated(recent_files_exclude)) allocate (recent_files_exclude(0))
        if (.not. allocated(recent_files_exclude_ignored_extensions)) allocate (recent_files_exclude_ignored_extensions(0))

        if (.not. allocated(recent_files_keep)) then
            allocate (recent_files_keep(1))
            recent_files_keep(1)%test => recent_files_keep_default_test
        end if

        if (.not. allocated(recent_files_filename_handlers)) then
            allocate (recent_files_filename_handlers(1))
            recent_files_filename_handlers(1)%handler => abbreviate_file_name_handler
        end if

        call minor_modes%recent_files_mode%define("recent_files_mode", global=.true., hook=recent_files_mode_hook, keymap=.true., &
                                                  body=recent_files_mode_body)
        recent_files_mode_map = minor_modes%recent_files_mode%map
        commands%recent_files_mode%execute => recent_files_mode
        commands%recent_files_open%execute => recent_files_open
        commands%recent_files%execute => recent_files_open
        commands%recent_files_open_most_recent_file%execute => execute_recent_files_open_most_recent_file
        commands%recent_files_save_list%execute => execute_recent_files_save_list
        commands%recent_files_load_list%execute => execute_recent_files_load_list
        commands%recent_files_cleanup%execute => recent_files_cleanup_adapter
    end subroutine initialize_recent_files

    !> Forget the seconds of `recent_files_auto_cleanup', as any assignment to it does.
    !> Not a port.
    subroutine recent_files_auto_cleanup_clear_kinds(self)
        class(recent_files_auto_cleanup_choice_setting_type), intent(inout) :: self

        self%seconds = -1
    end subroutine recent_files_auto_cleanup_clear_kinds

    !> The seconds of `recent_files_auto_cleanup' as help shows it (`%seconds = N'); "" when
    !> none is set.  Not a port.
    function recent_files_auto_cleanup_other_value(self) result(text)
        class(recent_files_auto_cleanup_choice_setting_type), intent(in) :: self
        character(len=:), allocatable :: text
        character(len=32) :: component_text

        text = ""

        if (self%seconds >= 0) then
            write (component_text, '("%seconds = ", i0)') self%seconds
            text = trim(component_text)
        end if
    end function recent_files_auto_cleanup_other_value

    !> The assignment that sets the number or procedure `recent_files_auto_cleanup' takes, for the error of a rejected
    !> value.  Not a port.
    function recent_files_auto_cleanup_kind_hint(self, number) result(text)
        class(recent_files_auto_cleanup_choice_setting_type), intent(in) :: self
        character(len=*), intent(in) :: number
        character(len=:), allocatable :: text

        text = "a number of seconds is `"//trim(self%name)//"%seconds = "//number//"'"
    end function recent_files_auto_cleanup_kind_hint
end module recent_files_mod
