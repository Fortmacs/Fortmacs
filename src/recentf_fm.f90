! recentf_fm.f90 -- keep track of recently opened files.
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
!> across Fortmacs sessions.  To enable it, turn on `recentf_mode'; visit
!> one of the files with `recentf_open'.
!>
!> The list is saved in `recentf_save_file' in namelist syntax
!> (namelist_fm):
!>
!>     &recentf
!>      recentf_list(1:2) = "~/notes.txt", "~/src/Fortmacs/src/rect_fm.f90",
!>     /
!>
!> `recentf_exclude' and `recentf_keep' are arrays of
!> `recentf_check_type' records, each a regexp or a predicate;
!> `recentf_filename_handlers' is an array of `recentf_filename_handler_type'.
!>
!> Deferred: the "Open Recent" menu with its filters (`recentf_menu_filter'
!> and the predefined and rule based filters; there is no menu bar), the
!> dialogs `recentf_edit_list', `recentf_open_files' and
!> `recentf_open_more_files' (widget.el is not ported) and with them
!> `recentf_filter_changer_current' in the saved file and the digit
!> shortcuts; remote files (`recentf_access_file'); tracking files as
!> they are written (`write_file_functions' is not ported); a time
!> string in `recentf_auto_cleanup'; the warning when the list can't be
!> saved (warnings.el); the obsolete `recentf_load_hook',
!> `recentf_trunc_list' and `recentf_elements'.
module recentf_fm
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, make_string, make_list, nilp
    use lread_fm, only: sym
    use data_fm, only: set, symbol_value
    use eval_fm, only: hook_type
    use modes_fm, only: minor_modes
    use commands_fm, only: commands
    use timer_fm, only: run_with_timer, run_with_idle_timer, free_timer, timerp
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, get_buffer_create, generate_new_buffer_name, &
                         kill_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: buffer_string
    use fileio_fm, only: file_readable_p, expand_file_name, write_region, insert_file_contents, set_file_modes
    use files_fm, only: find_file, find_file_hook, kill_buffer_hook, abbreviate_file_name, file_truename, &
                        file_name_extension
    use dired_fm, only: completion_ignored_extensions
    use config_fm, only: state_directory
    use coding_fm, only: coding_system_for_write
    use search_fm, only: string_match
    use keyboard_fm, only: kill_emacs_hook
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use minibuf_table_fm, only: completion_table_type, TABLE_LIST
    use minibuffer_fm, only: completing_read, REQUIRE_MATCH_T
    use easy_mmode_fm, only: mode_called_interactively
    use xdisp_fm, only: message
    use namelist_fm, only: namelist_group_type, namelist_item_type, read_namelist, namelist_text, NAMELIST_STRING
    implicit none(type, external)
    private

    public :: recentf_mode, recentf_enabled_p, recentf_exclude_file_by_extension_p, recentf_keep_default_predicate, &
              recentf_add_file, recentf_remove_if_non_kept, recentf_include_p, recentf_keep_p, recentf_push, &
              recentf_expand_file_name, recentf_apply_filename_handlers, recentf_open, recentf_open_most_recent_file, &
              recentf_save_list, recentf_load_list, recentf_cleanup, recentf_track_opened_file, &
              recentf_track_closed_file, abbreviate_file_name_handler, file_truename_handler, syms_of_recentf

    !> A recently opened file.
    type, public :: recentf_file_type
        character(len=:), allocatable :: name
    end type recentf_file_type

    !> List of recently opened files for `recentf_mode'.  Port of
    !> recentf.el recentf-list.
    type(recentf_file_type), allocatable, public :: recentf_list(:)

    abstract interface
        !> A predicate on a file name for `recentf_exclude' and `recentf_keep'.
        logical function recentf_predicate_proc(file)
            implicit none(type, external)
            character(len=*), intent(in) :: file
        end function recentf_predicate_proc

        !> A function of `recentf_filename_handlers': NAME transformed.
        subroutine recentf_filename_handler_proc(name, result, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: name
            character(len=:), allocatable, intent(out) :: result
            type(status_type), intent(out) :: status
        end subroutine recentf_filename_handler_proc

        !> `recentf_menu_action': open FILE.
        recursive subroutine recentf_action_proc(file, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: file
            type(status_type), intent(out) :: status
        end subroutine recentf_action_proc
    end interface

    !> An element of `recentf_exclude' or `recentf_keep': a REGEXP, or a
    !> PREDICATE (when REGEXP is blank).
    type, public :: recentf_check_type
        character(len=256) :: regexp = ""
        procedure(recentf_predicate_proc), pointer, nopass :: predicate => null()
    end type recentf_check_type

    !> An element of `recentf_filename_handlers'.
    type, public :: recentf_filename_handler_type
        procedure(recentf_filename_handler_proc), pointer, nopass :: handler => null()
    end type recentf_filename_handler_type

    !> `recentf_max_saved_items' for no limit (nil upstream).
    integer, parameter, public :: RECENTF_NO_LIMIT = huge(0)

    !> Maximum number of items of the recent list that will be saved.
    !> RECENTF_NO_LIMIT means to save the whole list.
    !> See the command `recentf_save_list'.  Port of recentf.el
    !> recentf-max-saved-items.
    integer, public :: recentf_max_saved_items = 25

    !> File to save the recent list into.  Port of recentf.el
    !> recentf-save-file (in the state directory,
    !> `~/.local/state/fortmacs/recentf').
    character(len=:), allocatable, public :: recentf_save_file

    !> Mode bits of recentf save file, as an integer, or -1.
    !> If not -1, after writing `recentf_save_file', set its mode bits to
    !> this value.  This is decimal, not octal.  The default is 384 (0600
    !> in octal), which gives R/W access only to the user who owns that
    !> file.  See also the function `set_file_modes'.  Port of recentf.el
    !> recentf-save-file-modes (-1 for nil).
    integer, public :: recentf_save_file_modes = 384

    !> List of regexps and predicates for filenames excluded from the
    !> recent list.  When a filename matches any of the regexps or
    !> satisfies any of the predicates it is excluded from the recent
    !> list.  A predicate is a function that is passed a filename to check
    !> and that must return true to exclude it.  Port of recentf.el
    !> recentf-exclude.
    type(recentf_check_type), allocatable, public :: recentf_exclude(:)

    !> List of file-name extensions that `recentf' should ignore.
    !> This has effect only if the function
    !> `recentf_exclude_file_by_extension_p' is in the list that is the
    !> value of `recentf_exclude'.  If this option is empty and
    !> `recentf_exclude_file_by_extension_p' is a member of
    !> `recentf_exclude', a file will be ignored if its extension is in
    !> `completion_ignored_extensions', which see.  Port of recentf.el
    !> recentf-exclude-ignored-extensions.
    character(len=32), allocatable, public :: recentf_exclude_ignored_extensions(:)

    !> List of regexps and predicates for filenames kept in the recent
    !> list.  Regexps and predicates are tried in the specified order.
    !> When empty all filenames are kept in the recent list.
    !> When a filename matches any of the regexps or satisfies any of the
    !> predicates it is kept in the recent list.
    !> The default is to keep readable files.
    !> A predicate is a function that is passed a filename to check and
    !> that must return true to keep it.  Port of recentf.el recentf-keep.
    type(recentf_check_type), allocatable, public :: recentf_keep(:)

    !> Function to invoke with a filename item of the recentf menu.
    !> The default is to call `find_file' to edit the selected file.  Port
    !> of recentf.el recentf-menu-action.
    procedure(recentf_action_proc), pointer, public :: recentf_menu_action => find_file

    !> Values of `recentf_auto_cleanup' other than a number of seconds.
    integer, parameter, public :: RECENTF_AUTO_CLEANUP_MODE = -1, RECENTF_AUTO_CLEANUP_NEVER = -2

    !> Define when to automatically cleanup the recent list.
    !> That is, remove duplicates, non-kept, and excluded files.
    !>
    !> The following values can be set:
    !>
    !> - RECENTF_AUTO_CLEANUP_MODE
    !>     Cleanup when turning the mode on (default).
    !> - RECENTF_AUTO_CLEANUP_NEVER
    !>     Never cleanup the list automatically.
    !> - A number
    !>     Cleanup each time Fortmacs has been idle that number of seconds.
    !>
    !> See also the command `recentf_cleanup', that can be used to
    !> manually cleanup the list.  Port of recentf.el recentf-auto-cleanup
    !> (a time string is Deferred).
    integer, public :: recentf_auto_cleanup = RECENTF_AUTO_CLEANUP_MODE

    !> True means to initialize `file_name_history' with the recent list.
    !> If `file_name_history' is not empty, do nothing.  Port of
    !> recentf.el recentf-initialize-file-name-history.
    logical, public :: recentf_initialize_file_name_history = .true.

    !> Functions to post process recent file names.
    !> They are successively passed a file name to transform it.  Port of
    !> recentf.el recentf-filename-handlers (the default holds
    !> `abbreviate_file_name_handler', for abbreviate-file-name).
    type(recentf_filename_handler_type), allocatable, public :: recentf_filename_handlers(:)

    !> Whether to show verbose messages about low-level recentf actions.
    !> False means to not show messages related to the recentf machinery.
    !> True means show messages that were printed by default on Emacs <=
    !> 31.1.  Port of recentf.el recentf-show-messages.
    logical, public :: recentf_show_messages = .true.

    !> The interval between auto saves of recently opened files, in
    !> seconds.  If set to 0, disables timer-based auto saving.  Port of
    !> recentf.el recentf-autosave-interval (0 for nil).
    integer, public :: recentf_autosave_interval = 0

    !> True if recentf searches and matches should ignore case (false on
    !> GNU/Linux).  Port of recentf.el recentf-case-fold-search.
    logical, parameter, public :: recentf_case_fold_search = .false.

    !> Hook run after entering or leaving `recentf_mode'.
    type(hook_type), target, public :: recentf_mode_hook = hook_type(name="recentf_mode_hook")

    !> Keymap to use in `recentf_mode'.  Port of recentf.el
    !> recentf-mode-map.
    type(handle_type), public :: recentf_mode_map

    !> Timer used to automatically cleanup the recent list.
    !> See also the option `recentf_auto_cleanup'.  Port of recentf.el
    !> recentf-auto-cleanup-timer.
    type(handle_type) :: recentf_auto_cleanup_timer
    type(handle_type) :: recentf_autosave_timer

contains

    !> Return true if `recentf_mode' is currently enabled.  Port of
    !> recentf.el:recentf-enabled-p.
    logical function recentf_enabled_p()
        recentf_enabled_p = kill_emacs_hook%member(recentf_save_list_proc)
    end function recentf_enabled_p

    !> Whether to ignore FILE_NAME due to its file-name extension.
    !> This predicate function is meant to be added to the list in
    !> `recentf_exclude'.  It returns true if FILE_NAME's extension is in
    !> the list that is the value of `recentf_exclude_ignored_extensions';
    !> if that variable is empty, this function consults
    !> `completion_ignored_extensions' instead.  Port of
    !> recentf.el:recentf-exclude-file-by-extension-p.
    logical function recentf_exclude_file_by_extension_p(file_name) result(ignored)
        character(len=*), intent(in) :: file_name
        character(len=:), allocatable :: extension, dotted
        logical :: found
        integer :: k
        ignored = .false.
        call file_name_extension(file_name, extension, found)
        if (.not. found) return
        dotted = "."//extension
        if (allocated(recentf_exclude_ignored_extensions)) then
            if (size(recentf_exclude_ignored_extensions) > 0) then
                do k = 1, size(recentf_exclude_ignored_extensions)
                    if (ends_with(dotted, trim(recentf_exclude_ignored_extensions(k)))) ignored = .true.
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
    end function recentf_exclude_file_by_extension_p

    !> Return true if FILE should be kept in the recent list.  Port of
    !> recentf.el:recentf-keep-default-predicate (no remote files).
    logical function recentf_keep_default_predicate(file) result(keep)
        character(len=*), intent(in) :: file
        keep = file_readable_p(file)
    end function recentf_keep_default_predicate

    !> Return true if strings S1 and S2 have identical contents.  Port of
    !> recentf.el:recentf-string-equal (`recentf_case_fold_search' is
    !> false).
    pure logical function recentf_string_equal(s1, s2) result(same)
        character(len=*), intent(in) :: s1, s2
        same = len(s1) == len(s2)
        if (same) same = s1 == s2
    end function recentf_string_equal

    !> The index in `recentf_list' of ELT, 0 if it is not there.  Port of
    !> recentf.el:recentf-string-member.
    integer function recentf_string_member(elt) result(k)
        character(len=*), intent(in) :: elt
        if (allocated(recentf_list)) then
            do k = 1, size(recentf_list)
                if (recentf_string_equal(elt, recentf_list(k)%name)) return
            end do
        end if
        k = 0
    end function recentf_string_member

    !> Remove element K of `recentf_list'.
    subroutine delete_entry(k)
        integer, intent(in) :: k
        type(recentf_file_type), allocatable :: kept(:)
        integer :: j
        allocate (kept(size(recentf_list) - 1))
        do j = 1, size(kept)
            call move_alloc(recentf_list(merge(j, j + 1, j < k))%name, kept(j)%name)
        end do
        call move_alloc(kept, recentf_list)
    end subroutine delete_entry

    !> Push FILENAME into the recent list, if it isn't there yet.
    !> If it is there yet, move it at the beginning of the list.  Port of
    !> recentf.el:recentf-push.
    subroutine recentf_push(filename)
        character(len=*), intent(in) :: filename
        type(recentf_file_type), allocatable :: grown(:)
        integer :: k
        if (.not. allocated(recentf_list)) allocate (recentf_list(0))
        k = recentf_string_member(filename)
        if (k > 0) call delete_entry(k)
        allocate (grown(size(recentf_list) + 1))
        grown(1)%name = filename
        do k = 1, size(recentf_list)
            call move_alloc(recentf_list(k)%name, grown(k + 1)%name)
        end do
        call move_alloc(grown, recentf_list)
    end subroutine recentf_push

    !> Apply `recentf_filename_handlers' to file NAME.
    !> Return the transformed file name, or NAME if any handler failed.
    !> Port of recentf.el:recentf-apply-filename-handlers.
    function recentf_apply_filename_handlers(name) result(filename)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: filename, next
        type(status_type) :: st
        integer :: k
        filename = name
        if (.not. allocated(recentf_filename_handlers)) return
        do k = 1, size(recentf_filename_handlers)
            if (.not. associated(recentf_filename_handlers(k)%handler)) cycle
            call recentf_filename_handlers(k)%handler(filename, next, st)
            if (.not. st%ok()) then
                filename = name
                return
            end if
            filename = next
        end do
    end function recentf_apply_filename_handlers

    !> Convert file NAME to absolute, and canonicalize it.
    !> NAME is first passed to the function `expand_file_name', then to
    !> `recentf_filename_handlers' to post process it.  Port of
    !> recentf.el:recentf-expand-file-name.
    function recentf_expand_file_name(name) result(filename)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: filename, expanded
        type(status_type) :: st
        call expand_file_name(name, expanded, st)
        if (.not. st%ok()) expanded = name
        filename = recentf_apply_filename_handlers(expanded)
    end function recentf_expand_file_name

    !> Whether FILENAME matches CHECK (a regexp, or a predicate).
    logical function check_matches(check, filename) result(matches)
        type(recentf_check_type), intent(in) :: check
        character(len=*), intent(in) :: filename
        type(status_type) :: st
        if (len_trim(check%regexp) > 0) then
            matches = string_match(trim(check%regexp), filename, st) > 0
            if (.not. st%ok()) matches = .false.
        else if (associated(check%predicate)) then
            matches = check%predicate(filename)
        else
            matches = .false.
        end if
    end function check_matches

    !> Return true if FILENAME should be included in the recent list.
    !> That is, if it doesn't match any of the `recentf_exclude' checks.
    !> Port of recentf.el:recentf-include-p.
    logical function recentf_include_p(filename) result(keepit)
        character(len=*), intent(in) :: filename
        integer :: k
        keepit = .true.
        if (.not. allocated(recentf_exclude)) return
        do k = 1, size(recentf_exclude)
            if (check_matches(recentf_exclude(k), filename)) then
                keepit = .false.
                return
            end if
        end do
    end function recentf_include_p

    !> Return true if FILENAME should be kept in the recent list.
    !> That is, if it matches any of the `recentf_keep' checks.  Port of
    !> recentf.el:recentf-keep-p.
    logical function recentf_keep_p(filename) result(keepit)
        character(len=*), intent(in) :: filename
        integer :: k
        keepit = .true.
        if (.not. allocated(recentf_keep)) return
        if (size(recentf_keep) == 0) return
        keepit = .false.
        do k = 1, size(recentf_keep)
            if (check_matches(recentf_keep(k), filename)) then
                keepit = .true.
                return
            end if
        end do
    end function recentf_keep_p

    !> Add or move FILENAME at the beginning of the recent list.
    !> Does nothing if the name satisfies any of the `recentf_exclude'
    !> regexps or predicates.  Port of recentf.el:recentf-add-file.
    subroutine recentf_add_file(filename)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: name
        name = recentf_expand_file_name(filename)
        if (recentf_include_p(name)) call recentf_push(name)
    end subroutine recentf_add_file

    !> Remove FILENAME from the recent list, if file is not kept.
    !> REMOVED says whether FILENAME has been removed.  Port of
    !> recentf.el:recentf-remove-if-non-kept.
    subroutine recentf_remove_if_non_kept(filename, removed)
        character(len=*), intent(in) :: filename
        logical, intent(out), optional :: removed
        integer :: k
        if (present(removed)) removed = .false.
        if (recentf_keep_p(filename)) return
        k = recentf_string_member(recentf_expand_file_name(filename))
        if (k == 0) return
        call delete_entry(k)
        if (present(removed)) removed = .true.
    end subroutine recentf_remove_if_non_kept

    !> abbreviate-file-name as a `recentf_filename_handlers' element.
    subroutine abbreviate_file_name_handler(name, result, status)
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        status = status_type()
        result = abbreviate_file_name(name)
    end subroutine abbreviate_file_name_handler

    !> file-truename as a `recentf_filename_handlers' element.
    subroutine file_truename_handler(name, result, status)
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        call file_truename(name, result, status)
    end subroutine file_truename_handler

    !> Automatic cleanup of the recent list.  Port of
    !> recentf.el:recentf-auto-cleanup.
    subroutine recentf_auto_cleanup_run()
        type(status_type) :: st
        if (timerp(recentf_auto_cleanup_timer)) call free_timer(recentf_auto_cleanup_timer)
        recentf_auto_cleanup_timer = handle_type()
        if (.not. minor_modes%recentf_mode%enabled()) return
        if (recentf_auto_cleanup == RECENTF_AUTO_CLEANUP_MODE) then
            call recentf_cleanup()
        else if (recentf_auto_cleanup > 0) then
            call run_with_idle_timer(recentf_auto_cleanup_timer, real(recentf_auto_cleanup, real64), .true., &
                                     recentf_cleanup_proc, st)
        end if
    end subroutine recentf_auto_cleanup_run

    !> Prompt for FILE in `recentf_list' and visit it.
    !> Enable `recentf_mode' if it isn't already.  Port of
    !> recentf.el:recentf-open (also `recentf').
    recursive subroutine recentf_open(status)
        type(status_type), intent(out) :: status
        type(completion_table_type) :: table
        type(value_type), allocatable :: names(:)
        character(len=:), allocatable :: file
        integer :: k
        if (.not. minor_modes%recentf_mode%enabled()) then
            call minor_modes%recentf_mode%enable(status)
            if (.not. status%ok()) return
        end if
        if (.not. allocated(recentf_list)) allocate (recentf_list(0))
        allocate (names(size(recentf_list)))
        do k = 1, size(recentf_list)
            names(k) = make_string(recentf_list(k)%name)
        end do
        table%kind = TABLE_LIST
        table%list = make_list(names)
        table%category = "recentf"
        call completing_read("Open recent file: ", table, file, status, require_match=REQUIRE_MATCH_T)
        if (.not. status%ok()) return
        call recentf_menu_action(file, status)
    end subroutine recentf_open

    !> Open the Nth most recent file.
    !> Optional argument N must be a valid digit number.  It defaults to 1.
    !> 1 opens the most recent file, 2 the second most recent one, etc..
    !> 0 opens the tenth most recent file.  Port of
    !> recentf.el:recentf-open-most-recent-file.
    recursive subroutine recentf_open_most_recent_file(n_in, status)
        integer(int64), intent(in) :: n_in
        type(status_type), intent(out) :: status
        integer(int64) :: n
        character(len=24) :: buf
        n = n_in
        if (n == 0) then
            n = 10
        else if (.not. (n > 0 .and. n < 10)) then
            write (buf, "(i0)") n
            call set_error(status, FM_GENERIC_ERROR, "Recent file number out of range [0-9], "//trim(buf))
            return
        end if
        if (.not. allocated(recentf_list)) allocate (recentf_list(0))
        if (n > size(recentf_list)) then
            call set_error(status, FM_GENERIC_ERROR, "Not that many recent files")
            return
        end if
        call recentf_menu_action(recentf_list(n)%name, status)
    end subroutine recentf_open_most_recent_file

    recursive subroutine cmd_recentf_open_most_recent_file(status)
        type(status_type), intent(out) :: status
        call recentf_open_most_recent_file(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_recentf_open_most_recent_file

    !> Insert the name of the file just opened or written into the recent
    !> list.  Port of recentf.el:recentf-track-opened-file.
    recursive subroutine recentf_track_opened_file(status)
        type(status_type), intent(out) :: status
        status = status_type()
        if (allocated(buffers(cur)%filename)) call recentf_add_file(buffers(cur)%filename)
    end subroutine recentf_track_opened_file

    !> Update the recent list when a buffer is killed.
    !> That is, remove a non kept file from the recent list.  Port of
    !> recentf.el:recentf-track-closed-file.
    recursive subroutine recentf_track_closed_file(status)
        type(status_type), intent(out) :: status
        status = status_type()
        if (allocated(buffers(cur)%filename)) call recentf_remove_if_non_kept(buffers(cur)%filename)
    end subroutine recentf_track_closed_file

    !> Cancel `recentf_autosave_timer', if set.  Port of
    !> recentf.el:recentf--cancel-autosave-timer.
    subroutine recentf_cancel_autosave_timer()
        if (timerp(recentf_autosave_timer)) call free_timer(recentf_autosave_timer)
        recentf_autosave_timer = handle_type()
    end subroutine recentf_cancel_autosave_timer

    !> Set or cancel an invocation of `recentf_save_list' on a timer.
    !> If `recentf_mode' is enabled, set the timer, otherwise cancel the
    !> timer.  Port of recentf.el:recentf--manage-autosave-timer.
    subroutine recentf_manage_autosave_timer()
        type(status_type) :: st
        if (minor_modes%recentf_mode%enabled() .and. recentf_autosave_interval > 0 .and. &
            .not. timerp(recentf_autosave_timer)) then
            call run_with_timer(recentf_autosave_timer, real(recentf_autosave_interval, real64), &
                                recentf_save_list_proc, st, real(recentf_autosave_interval, real64))
        else
            call recentf_cancel_autosave_timer()
        end if
    end subroutine recentf_manage_autosave_timer

    !> Save the recent list.
    !> Write data into the file specified by `recentf_save_file'.  Port of
    !> recentf.el:recentf-save-list (INTERACTIVE says it was called as a
    !> command).
    recursive subroutine recentf_save_list(interactive)
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
        if (.not. allocated(recentf_list)) allocate (recentf_list(0))
        group%name = "recentf"
        n = min(size(recentf_list), max(recentf_max_saved_items, 0))
        if (n == 0) then
            item = namelist_item_type(name="recentf_list")
            call item%add_logical(.false.)
        else
            item = namelist_item_type(name="recentf_list", subscripted=.true., lower=1, upper=n)
            do k = 1, n
                call item%add_string(recentf_list(k)%name)
            end do
        end if
        call group%add(item)
        call expand_file_name(recentf_save_file, file, st)
        if (.not. st%ok()) file = recentf_save_file
        old = current_buffer()
        call get_buffer_create(generate_new_buffer_name(" *temp*"), h, st)
        call set_buffer(h, st)
        call insert("! Automatically generated by `recentf'."//achar(10)//namelist_text(group), st)
        coding_system_for_write = "utf-8-emacs"
        call write_region(file, .false., wst, message=wrote)
        call coding_system_for_write%clear()
        call kill_buffer(h, st)
        call set_buffer(old, st)
        ! (The warning when it fails is Deferred: warnings.el is not ported.)
        if (.not. wst%ok()) return
        if (inter .or. recentf_show_messages) call message(wrote)
        if (recentf_save_file_modes >= 0) call set_file_modes(recentf_save_file, recentf_save_file_modes, st)
    end subroutine recentf_save_list

    recursive subroutine recentf_save_list_proc(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call recentf_save_list()
    end subroutine recentf_save_list_proc

    recursive subroutine cmd_recentf_save_list(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call recentf_save_list(.true.)
    end subroutine cmd_recentf_save_list

    !> Load a previously saved recent list.
    !> Read data from the file specified by `recentf_save_file'.
    !> When `recentf_initialize_file_name_history' is true, initialize an
    !> empty `file_name_history' with the recent list.  Port of
    !> recentf.el:recentf-load-list.
    recursive subroutine recentf_load_list(status)
        type(status_type), intent(out) :: status
        type(namelist_group_type), allocatable :: groups(:)
        type(value_type) :: history
        type(value_type), allocatable :: names(:)
        type(status_type) :: st
        character(len=:), allocatable :: file, text
        integer :: g, k, j
        status = status_type()
        call expand_file_name(recentf_save_file, file, st)
        if (.not. st%ok()) file = recentf_save_file
        if (.not. file_readable_p(file)) return
        call file_text(file, text, status)
        if (.not. status%ok()) return
        call read_namelist(text, groups, status)
        if (.not. status%ok()) return
        do g = 1, size(groups)
            if (groups(g)%name /= "recentf" .or. .not. allocated(groups(g)%items)) cycle
            do k = 1, size(groups(g)%items)
                associate (it => groups(g)%items(k))
                    if (it%name /= "recentf_list") cycle
                    if (allocated(recentf_list)) deallocate (recentf_list)
                    allocate (recentf_list(0))
                    if (.not. it%subscripted) cycle
                    deallocate (recentf_list)
                    allocate (recentf_list(size(it%values)))
                    do j = 1, size(it%values)
                        recentf_list(j)%name = ""
                        if (it%values(j)%kind == NAMELIST_STRING) recentf_list(j)%name = it%values(j)%string
                    end do
                end associate
            end do
        end do
        if (.not. recentf_initialize_file_name_history) return
        call symbol_value(sym("file_name_history"), history, st)
        if (.not. nilp(history)) return
        allocate (names(size(recentf_list)))
        do k = 1, size(recentf_list)
            names(k) = make_string(abbreviate_file_name(recentf_list(k)%name))
        end do
        call set(sym("file_name_history"), make_list(names), st)
    end subroutine recentf_load_list

    recursive subroutine cmd_recentf_load_list(status)
        type(status_type), intent(out) :: status
        call recentf_load_list(status)
    end subroutine cmd_recentf_load_list

    !> The text of FILE.
    recursive subroutine file_text(file, text, status)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: text
        type(status_type), intent(out) :: status
        type(handle_type) :: old, h
        type(status_type) :: st
        integer(int64) :: inserted
        text = ""
        old = current_buffer()
        call get_buffer_create(generate_new_buffer_name(" *temp*"), h, st)
        call set_buffer(h, st)
        call insert_file_contents(file, .false., inserted, status)
        if (status%ok()) text = buffer_string()
        call kill_buffer(h, st)
        call set_buffer(old, st)
    end subroutine file_text

    !> Cleanup the recent list.
    !> That is, remove duplicates, non-kept, and excluded files.  Port of
    !> recentf.el:recentf-cleanup.
    recursive subroutine recentf_cleanup()
        type(recentf_file_type), allocatable :: newlist(:)
        character(len=:), allocatable :: f
        character(len=12) :: buf
        integer :: n, k, m, j
        logical :: dup
        call message("Cleaning up the recentf list...")
        if (.not. allocated(recentf_list)) allocate (recentf_list(0))
        allocate (newlist(size(recentf_list)))
        n = 0
        m = 0
        do k = 1, size(recentf_list)
            f = recentf_expand_file_name(recentf_list(k)%name)
            dup = .false.
            do j = 1, m
                if (recentf_string_equal(newlist(j)%name, f)) then
                    dup = .true.
                    exit
                end if
            end do
            if (recentf_include_p(f) .and. recentf_keep_p(f) .and. .not. dup) then
                m = m + 1
                newlist(m)%name = f
            else
                n = n + 1
                call message("File "//f//" removed from the recentf list")
            end if
        end do
        write (buf, "(i0)") n
        call message("Cleaning up the recentf list...done ("//trim(buf)//" removed)")
        if (allocated(recentf_list)) deallocate (recentf_list)
        allocate (recentf_list(m))
        do k = 1, m
            call move_alloc(newlist(k)%name, recentf_list(k)%name)
        end do
    end subroutine recentf_cleanup

    recursive subroutine recentf_cleanup_proc(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call recentf_cleanup()
    end subroutine recentf_cleanup_proc

    !> Toggle keeping track of opened files (Recentf mode).
    !> This mode maintains a list of recently opened files and makes it
    !> easy to visit them.  The recent files list is automatically saved
    !> across Fortmacs sessions.
    !>
    !> You can use `recentf_open' to visit files.
    !>
    !> By default, only operations like opening a file, writing a buffer
    !> to a file, and killing a buffer is counted as "operating" on the
    !> file.  Port of recentf.el:recentf-mode (no "Open Recent" menu).
    recursive subroutine recentf_mode(status)
        type(status_type), intent(out) :: status
        call minor_modes%recentf_mode%command(status)
    end subroutine recentf_mode

    !> The body of Recentf mode.
    recursive subroutine recentf_mode_body(enable, status)
        logical, intent(in) :: enable
        type(status_type), intent(out) :: status
        status = status_type()
        if (enable .and. recentf_enabled_p()) return
        if (enable) then
            call recentf_load_list(status)
        else
            call recentf_save_list(mode_called_interactively)
        end if
        call recentf_auto_cleanup_run()
        call recentf_manage_autosave_timer()
        if (enable) then
            call find_file_hook%add(recentf_track_opened_file)
            call kill_buffer_hook%add(recentf_track_closed_file)
            call kill_emacs_hook%add(recentf_save_list_proc)
        else
            call find_file_hook%remove(recentf_track_opened_file)
            call kill_buffer_hook%remove(recentf_track_closed_file)
            call kill_emacs_hook%remove(recentf_save_list_proc)
        end if
    end subroutine recentf_mode_body

    !> Define the mode, the commands and the options' defaults.
    subroutine syms_of_recentf()
        if (.not. allocated(recentf_save_file)) recentf_save_file = state_directory()//"recentf"
        if (.not. allocated(recentf_list)) allocate (recentf_list(0))
        if (.not. allocated(recentf_exclude)) allocate (recentf_exclude(0))
        if (.not. allocated(recentf_exclude_ignored_extensions)) allocate (recentf_exclude_ignored_extensions(0))
        if (.not. allocated(recentf_keep)) then
            allocate (recentf_keep(1))
            recentf_keep(1)%predicate => recentf_keep_default_predicate
        end if
        if (.not. allocated(recentf_filename_handlers)) then
            allocate (recentf_filename_handlers(1))
            recentf_filename_handlers(1)%handler => abbreviate_file_name_handler
        end if
        call minor_modes%recentf_mode%define("recentf_mode", global=.true., hook=recentf_mode_hook, keymap=.true., &
                                             body=recentf_mode_body)
        recentf_mode_map = minor_modes%recentf_mode%map
        commands%recentf_mode%run => recentf_mode
        commands%recentf_open%run => recentf_open
        commands%recentf%run => recentf_open
        commands%recentf_open_most_recent_file%run => cmd_recentf_open_most_recent_file
        commands%recentf_save_list%run => cmd_recentf_save_list
        commands%recentf_load_list%run => cmd_recentf_load_list
        commands%recentf_cleanup%run => recentf_cleanup_proc
    end subroutine syms_of_recentf
end module recentf_fm
