! bookmark_fm.f90 -- set bookmarks, maybe annotate them, jump to them later.
! Ported from GNU Emacs 31.1 lisp/bookmark.el.
!
! Copyright (C) 1993-1997, 2001-2026 Free Software Foundation, Inc.
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

!> This package is for setting "bookmarks" in files.  A bookmark
!> associates a string with a location in a certain file.  Thus, you
!> can navigate your way to that location by providing the string.
!>
!> \[bookmark_set] (C-x r m) sets a bookmark, \[bookmark_jump] (C-x r b)
!> jumps to one; `bookmark_map' holds all the bookmark commands, for a
!> prefix key of your choice.
!>
!> `bookmark_table' is an array of `bookmark_type' records, saved in
!> `bookmark_default_file' in namelist syntax (namelist_fm):
!>
!>     ! Fortmacs bookmarks -*- coding: utf-8-emacs -*-
!>     &bookmark
!>      bookmark(1)%name = "notes",
!>      bookmark(1)%filename = "~/notes.txt",
!>      bookmark(1)%front_context_string = "Fortran namelis",
!>      bookmark(1)%rear_context_string = "ved state is in ",
!>      bookmark(1)%position = 1234,
!>      bookmark(1)%last_modified = 1761700000, 123456789,
!>     /
!>
!> The last-modified time is seconds and nanoseconds since the epoch.
!>
!> \[bookmark_bmenu_list] (C-x r l) lists the bookmarks in Bookmark Menu mode
!> (a Tabulated List mode).
!>
!> Deferred: the menu and the mouse command of the bookmark list
!> (`bookmark_bmenu_other_window_with_mouse'), and its sorting in the locale's
!> collation order (`is_string_collate_less': names sort case-insensitively
!> by character code, as in the POSIX locale); `truncate_lines' in it;
!> bookmarks with a `handler' (Help, VC-Dir and Shell buffers set
!> `bookmark_make_record_function' to make them; that function is not
!> buffer-local yet), and with them the `buffer', `location' and other
!> properties of a record, the bookmark type and the `bookmark_inhibit'
!> property; `bookmark_jump_other_frame' (frames are not ported); the
!> popup menu `bookmark_completing_read' shows for a mouse event and the
!> menu bar's Bookmarks menu (`menu_bar_bookmark_map',
!> `bookmark_menu_length'); fringe bitmaps (the fringe mark is an overlay
!> whose text a terminal does not show); numbered backups of the
!> bookmark file (`bookmark_version_control': version-control is not
!> ported); `select_safe_coding_system' when saving; the obsolete
!> variables, format upgrades and `bookmark_load_hook'.
module bookmark_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use error_fm, only: status_type, set_error, condition_matches, FM_GENERIC_ERROR, FM_USER_ERROR, &
                        FM_FILE_ERROR, FM_BOOKMARK_ERROR_NO_FILENAME, format_message
    use handle_fm, only: handle_type
    use lisp_fm, only: value_type, make_list, vtag, V_INTEGER, V_ARRAY
    use data_fm, only: add_local_variables_killer, history_setting, forward_setting
    use eval_fm, only: hook_type
    use modes_fm, only: major_modes
    use commands_fm, only: commands
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, get_buffer, get_buffer_create, is_buffer_live, &
                         buffer_list, erase_buffer, kill_buffer, buffer_serial_live, generate_new_buffer_name, &
                         restore_buffer_modified
    use insdel_fm, only: insert
    use editfns_fm, only: point, goto_char, point_min, point_max, buffer_substring, buffer_string, delete_region, &
                          following_char, pos_bol, pos_eol, widen, is_bol, is_eob, user_full_name, user_login_name, &
                          system_name, current_time_string
    use cmds_fm, only: forward_line, beginning_of_line, end_of_line, delete_char, forward_char
    use syntax_fm, only: forward_word
    use simple_fm, only: push_mark
    use search_fm, only: search_forward, search_backward, match_beginning, match_end
    use fileio_fm, only: is_file_readable, expand_file_name, file_name_nondirectory, file_name_directory, &
                         directory_file_name, file_attributes
    use sysdep_fm, only: file_stat_type, emacs_clock_realtime
    use files_fm, only: find_file_noselect, abbreviate_file_name, write_file, revert_buffer_function
    use frame_fm, only: selected_frame, selected_window
    use tabulated_list_fm, only: tabulated_list_column_type, tabulated_list_entry_type, set_tabulated_list_format, &
                                 set_tabulated_list_entries, set_tabulated_list_padding, set_tabulated_list_sort_key, &
                                 clear_tabulated_list_sort_key, tabulated_list_init_header, tabulated_list_print, &
                                 tabulated_list_get_id, tabulated_list_put_tag, tabulated_list_revert_hook
    use dired_mode_fm, only: dired_directory_of
    use derived_fm, only: is_derived_mode
    use config_fm, only: state_directory
    use coding_fm, only: coding_system_for_write, buffer_file_coding_system, coding_system_base
    use itree_fm, only: make_overlay, overlay_put, overlay_get, overlays_in, free_overlay
    use keymap_fm, only: make_sparse_keymap, set_keymap_parent, is_keymap_live, free_keymap, keymap_set
    use keyboard_fm, only: kill_emacs_hook
    use callint_fm, only: current_prefix_arg
    use minibuf_fm, only: read_from_minibuffer, minibuffer_local_map, minibuffer_string_type, completion_ignore_case
    use minibuf_table_fm, only: completion_table_type, TABLE_LIST
    use minibuffer_fm, only: completing_read, read_file_name, REQUIRE_MATCH_OTHER, REQUIRE_MATCH_CONFIRM
    use fns_fm, only: read_yes_or_no
    use subr_fm, only: add_to_history, read_y_or_n, make_progress_reporter, progress_reporter_done, &
                       progress_reporter_type
    use dispnew_fm, only: ding
    use help_fm, only: substitute_command_keys
    use window_fm, only: windows, get_buffer_window, set_window_point, pop_to_buffer, pop_to_buffer_same_window, &
                         switch_to_buffer_other_window, quit_window, window_configuration_type, &
                         current_window_configuration, set_window_configuration, display_action_table_type, switch_to_buffer
    use faces_fm, only: named_face_type, face_attrs, display_when, FACE_CLASS_COLOR, FACE_CLASS_GRAYSCALE, &
                        FACE_BACKGROUND_LIGHT, FACE_BACKGROUND_DARK
    use xdisp_fm, only: message
    use namelist_fm, only: namelist_group_type, namelist_item_type, read_namelist, namelist_text, &
                           NAMELIST_STRING, NAMELIST_INTEGER
    implicit none(type, external)
    private

    public :: bookmark_name_from_full_record, bookmark_all_names, bookmark_get_bookmark, bookmark_set_name, &
              bookmark_get_annotation, bookmark_set_annotation, bookmark_get_filename, bookmark_set_filename, &
              bookmark_get_position, bookmark_set_position, bookmark_get_front_context_string, &
              bookmark_set_front_context_string, bookmark_get_rear_context_string, &
              bookmark_set_rear_context_string, bookmark_update_last_modified, bookmark_maybe_sort_table, &
              bookmark_completing_read, bookmark_maybe_historicize_string, bookmark_make_record, bookmark_store, &
              bookmark_make_record_default, bookmark_table_from_buffer, bookmark_set_internal, bookmark_set, &
              bookmark_set_no_overwrite, bookmark_kill_line, bookmark_default_annotation_text, &
              bookmark_insert_annotation, bookmark_edit_annotation_mode, bookmark_edit_annotation_cancel, &
              bookmark_edit_annotation_confirm, bookmark_edit_annotation, bookmark_buffer_name, bookmark_yank_word, &
              bookmark_buffer_file_name, bookmark_maybe_load_default_file, bookmark_jump, &
              bookmark_jump_other_window, bookmark_handle_bookmark, bookmark_default_handler, bookmark_relocate, &
              bookmark_insert_location, bookmark_location, bookmark_rename, bookmark_insert, bookmark_delete, &
              bookmark_delete_all, is_bookmark_time_to_save, bookmark_write, bookmark_save, bookmark_write_file, &
              bookmark_import_new_list, bookmark_maybe_rename, bookmark_load, bookmark_show_annotation, &
              bookmark_show_all_annotations, bookmark_exit_hook_internal, bookmark_current_bookmark, &
              set_bookmark_current_bookmark, bookmark_bmenu_surreptitiously_rebuild_list, bookmark_bmenu_get_buffer, &
              bookmark_bmenu_list, bookmark_bmenu_mode, bookmark_bmenu_name_predicate, bookmark_bmenu_type_predicate, &
              bookmark_bmenu_file_predicate, bookmark_bmenu_toggle_filenames_command, bookmark_bmenu_show_filenames, &
              bookmark_bmenu_hide_filenames, bookmark_bmenu_ensure_position, bookmark_bmenu_bookmark, &
              bookmark_bmenu_mark, bookmark_bmenu_mark_all, bookmark_bmenu_select, bookmark_bmenu_any_marks, &
              bookmark_bmenu_save, bookmark_bmenu_load, bookmark_bmenu_1_window, bookmark_bmenu_2_window, &
              bookmark_bmenu_this_window, bookmark_bmenu_other_window, bookmark_bmenu_switch_other_window, &
              bookmark_bmenu_show_annotation, bookmark_bmenu_show_all_annotations, bookmark_bmenu_edit_annotation, &
              bookmark_bmenu_unmark, bookmark_bmenu_backup_unmark, bookmark_bmenu_unmark_all, bookmark_bmenu_delete, &
              bookmark_bmenu_delete_backwards, bookmark_bmenu_delete_all, bookmark_bmenu_execute_deletions, &
              bookmark_bmenu_rename, bookmark_bmenu_locate, bookmark_bmenu_relocate, &
              bookmark_bmenu_filter_table_by_regexp, bookmark_bmenu_search, bookmark_bmenu_goto_bookmark, &
              syms_of_bookmark

    !> A bookmark: its NAME and where it is.  FILENAME names the bookmarked
    !> file; POSITION is the bookmarked buffer position (0 for none);
    !> FRONT_CONTEXT_STRING is buffer text that immediately follows
    !> POSITION and REAR_CONTEXT_STRING buffer text that immediately
    !> precedes it; ANNOTATION is a string that describes the bookmark (see
    !> the options `bookmark_use_annotations' and
    !> `bookmark_automatically_show_annotations'); LAST_MODIFIED is when it
    !> was set or last changed, as seconds and nanoseconds since the epoch.
    !> Unallocated components are not used.  DEFAULTS is only in a new
    !> record: a list of default names for it.  Port of the (NAME .
    !> PARAM-ALIST) elements of bookmark.el bookmark-alist.
    type, public :: bookmark_type
        character(len=:), allocatable :: name
        character(len=:), allocatable :: filename
        integer(int64) :: position = 0
        character(len=:), allocatable :: front_context_string, rear_context_string
        character(len=:), allocatable :: annotation
        integer(int64), allocatable :: last_modified(:)
        type(minibuffer_string_type), allocatable :: defaults(:)
    end type bookmark_type

    !> A function of `bookmark_inhibit_context_functions': whether not to
    !> record context strings of a buffer visiting FILENAME.
    abstract interface
        ! Not a port.
        logical function bookmark_file_predicate(filename)
            implicit none(type, external)
            character(len=*), intent(in) :: filename
        end function bookmark_file_predicate

        !> A `bookmark_make_record_function': make RECORD for the current
        !> location (its name may be left unallocated).  Not a port.
        recursive subroutine bookmark_make_record_proc(record, status)
            import :: bookmark_type, status_type
            implicit none(type, external)
            type(bookmark_type), intent(out) :: record
            type(status_type), intent(out) :: status
        end subroutine bookmark_make_record_proc

        !> A `bookmark_edit_annotation_text_func': the default annotation
        !> TEXT for bookmark NAME.  Not a port.
        subroutine bookmark_annotation_text_proc(name, text)
            implicit none(type, external)
            character(len=*), intent(in) :: name
            character(len=:), allocatable, intent(out) :: text
        end subroutine bookmark_annotation_text_proc

        !> A display function of `bookmark_jump': show BUFFER.  Not a port.
        recursive subroutine bookmark_display_proc(buffer, status)
            import :: handle_type, status_type
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
            type(status_type), intent(out) :: status
        end subroutine bookmark_display_proc
    end interface

    !> An element of `bookmark_inhibit_context_functions'.
    type, public :: bookmark_inhibit_context_function_type
        procedure(bookmark_file_predicate), pointer, nopass :: function => null()
    end type bookmark_inhibit_context_function_type

    !> The current default bookmark file with its modification time.
    type, public :: bookmark_timestamp_type
        !> Unallocated when no bookmark file has been loaded or saved.
        character(len=:), allocatable :: file
        !> False if the file did not exist.
        logical :: mtime_known = .false.
        integer(int64) :: mtime_sec = 0
        integer(int32) :: mtime_nsec = 0
    end type bookmark_timestamp_type

    !> If true, setting a bookmark queries for an annotation in a buffer.
    !> Port of bookmark.el bookmark-use-annotations.
    logical, public :: bookmark_use_annotations = .false.

    !> Values of `bookmark_save_flag' other than a number.
    integer, parameter, public :: BOOKMARK_SAVE_FLAG_NEVER = -1, BOOKMARK_SAVE_FLAG_ON_EXIT = -2

    !> Controls when Fortmacs saves bookmarks to a file.
    !> --> BOOKMARK_SAVE_FLAG_NEVER means never save bookmarks, except when
    !>     `bookmark_save' is explicitly called (\[bookmark_save]).
    !> --> BOOKMARK_SAVE_FLAG_ON_EXIT means save bookmarks when Fortmacs is
    !>     killed.
    !> --> Otherwise, it should be a number that is the frequency with which
    !>     the bookmark list is saved (i.e.: the number of times which
    !>     Fortmacs's bookmark list may be modified before it is
    !>     automatically saved.).  If it is a number, Fortmacs will also
    !>     automatically save bookmarks when it is killed.
    !>
    !> Therefore, the way to get it to save every time you make or delete a
    !> bookmark is to set this variable to 1 (or 0, which produces the same
    !> behavior.)
    !>
    !> To specify the file in which to save them, modify the variable
    !> `bookmark_default_file'.  Port of bookmark.el bookmark-save-flag.
    integer, public :: bookmark_save_flag = BOOKMARK_SAVE_FLAG_ON_EXIT

    !> File in which to save bookmarks by default.  Port of bookmark.el
    !> bookmark-default-file (in the state directory,
    !> `~/.local/state/fortmacs/bookmarks').
    character(len=:), allocatable, public :: bookmark_default_file

    !> Values of `bookmark_watch_bookmark_file'.
    integer, parameter, public :: BOOKMARK_WATCH_BOOKMARK_FILE_NO = 0, BOOKMARK_WATCH_BOOKMARK_FILE_ASK = 1, &
                                  BOOKMARK_WATCH_BOOKMARK_FILE_SILENT = 2

    !> If BOOKMARK_WATCH_BOOKMARK_FILE_ASK, reload the default bookmark file
    !> if it was changed.  If this file has changed on disk since it was
    !> last loaded, query the user whether to load it again.  If the value
    !> is BOOKMARK_WATCH_BOOKMARK_FILE_SILENT reload without querying.
    !> This file defaults to `bookmark_default_file'.  But during a
    !> Fortmacs session, `bookmark_load' and `bookmark_save' can redefine
    !> the current default file.  Port of bookmark.el
    !> bookmark-watch-bookmark-file.
    integer, public :: bookmark_watch_bookmark_file = BOOKMARK_WATCH_BOOKMARK_FILE_ASK

    !> Values of `bookmark_version_control': BOOKMARK_VERSION_CONTROL_NUMBERED, BOOKMARK_VERSION_CONTROL_EXISTING,
    !> BOOKMARK_VERSION_CONTROL_NEVER and BOOKMARK_VERSION_CONTROL_NOSPECIAL.
    integer, parameter, public :: BOOKMARK_VERSION_CONTROL_NUMBERED = 1, BOOKMARK_VERSION_CONTROL_EXISTING = 0, &
                                  BOOKMARK_VERSION_CONTROL_NEVER = 2, BOOKMARK_VERSION_CONTROL_NOSPECIAL = 3

    !> Whether or not to make numbered backups of the bookmark file.
    !> It can have four values: BOOKMARK_VERSION_CONTROL_NUMBERED, _NIL, _NEVER,
    !> or _NOSPECIAL.  The first three have the same meaning that they do
    !> for the variable `version_control'; the value _NOSPECIAL (the
    !> default) means just use the value of `version_control'.  Port of
    !> bookmark.el bookmark-version-control (numbered backups are not
    !> ported: saving makes a single backup, as with `version_control' .false.).
    integer, public :: bookmark_version_control = BOOKMARK_VERSION_CONTROL_NOSPECIAL

    !> True means bookmark functions ignore case in completion.  Port of
    !> bookmark.el bookmark-completion-ignore-case.
    logical, public :: bookmark_completion_ignore_case = .true.

    !> Values of `bookmark_sort_flag'.
    integer, parameter, public :: BOOKMARK_SORT_FLAG_CREATED = 0, BOOKMARK_SORT_FLAG_ALPHABETICAL = 1, &
                                  BOOKMARK_SORT_FLAG_LAST_MODIFIED = 2

    !> This controls the bookmark display sorting.
    !> BOOKMARK_SORT_FLAG_CREATED means they will be displayed in LIFO order
    !> (that is, most recently created ones come first, oldest ones come
    !> last).
    !>
    !> BOOKMARK_SORT_FLAG_LAST_MODIFIED means that bookmarks will be
    !> displayed sorted from most recently modified to least recently
    !> modified.
    !>
    !> Other values means that bookmarks will be displayed sorted by
    !> bookmark name.  Port of bookmark.el bookmark-sort-flag.
    integer, public :: bookmark_sort_flag = BOOKMARK_SORT_FLAG_ALPHABETICAL

    !> True means confirm before deleting bookmarks in a bookmark menu
    !> buffer.  False means don't prompt for confirmation.  Port of
    !> bookmark.el bookmark-menu-confirm-deletion.
    logical, public :: bookmark_menu_confirm_deletion = .false.

    !> True means show annotations when jumping to a bookmark.  Port of
    !> bookmark.el bookmark-automatically-show-annotations.
    logical, public :: bookmark_automatically_show_annotations = .true.

    !> The fringe bitmap to mark bookmarked lines with.
    !> If unallocated, don't display a mark on the fringe.  Port of
    !> bookmark.el bookmark-fringe-mark (a terminal shows no fringes).
    character(len=:), allocatable, public :: bookmark_fringe_mark

    !> Face used to highlight current line.  Port of bookmark.el
    !> bookmark-face.
    type(named_face_type), target, public :: bookmark_face

    !> Keymap containing bindings to bookmark functions.
    !> It is not bound to any key by default: to bind it so that you have a
    !> bookmark prefix, just use `keymap_global_set' and bind a key of your
    !> choice to variable `bookmark_map'.  All interactive bookmark
    !> functions have a binding in this keymap.  Port of bookmark.el
    !> bookmark-map.
    type(handle_type), public :: bookmark_map

    !> List of bookmarks and their parameters.
    !> Bookmark functions update the value automatically.
    !> You probably do NOT want to change the value yourself.
    !>
    !> The list is ordered from most recently created bookmark at the front
    !> to least recently created bookmark at the end.  See `bookmark_type'
    !> for the parameters of a bookmark.  Port of bookmark.el
    !> bookmark-alist.
    type(bookmark_type), allocatable, public :: bookmark_table(:)

    !> Timestamp of current default bookmark file.
    !> The value is actually the FILE, a bookmark file that defaults to
    !> `bookmark_default_file', and its modification time.  Port of
    !> bookmark.el bookmark-bookmarks-timestamp.
    type(bookmark_timestamp_type), public :: bookmark_bookmarks_timestamp

    !> The coding-system of the last loaded or saved bookmark file
    !> (unallocated for none yet).  Port of bookmark.el
    !> bookmark-file-coding-system.
    character(len=:), allocatable, public :: bookmark_file_coding_system

    !> Number of modifications to bookmark list since it was last saved.
    !> Port of bookmark.el bookmark-alist-modification-count.
    integer, public :: bookmark_table_modification_count = 0

    !> Length of the context strings recorded on either side of a
    !> bookmark.  Port of bookmark.el bookmark-search-size.
    integer, public :: bookmark_search_size = 16

    !> The buffer in which a bookmark is currently being set or renamed.
    !> Functions that insert strings into the minibuffer use this to know
    !> the source buffer for that information; see `bookmark_yank_word'
    !> for example.  Port of bookmark.el bookmark-current-buffer.
    type(handle_type), public :: bookmark_current_buffer

    !> The next point from which to pull source text for
    !> `bookmark_yank_word' (0 for none).  This point is in
    !> `bookmark_current_buffer'.  Port of bookmark.el bookmark-yank-point.
    integer(int64), public :: bookmark_yank_point = 0

    !> A function that should be called to create a bookmark record.
    !> Modes may set this variable to enable bookmarking of locations that
    !> should be treated specially, such as Info nodes, news posts,
    !> images, pdf documents, etc.
    !>
    !> A `user_error' error condition should occur if it is unable to construct a record
    !> for the current location.
    !>
    !> The returned record is a `bookmark_type'.  Its name is a suggested
    !> name for the constructed bookmark.  It can be unallocated in which
    !> case a default heuristic will be used.  Port of bookmark.el
    !> bookmark-make-record-function (default
    !> `bookmark_make_record_default').
    procedure(bookmark_make_record_proc), pointer, public :: bookmark_make_record_function => null()

    !> List of functions to call before making a bookmark record.
    !> The functions take `buffer_file_name' as argument.  If any of these
    !> functions returns true, the bookmark does not record context strings
    !> from the current buffer.  Port of bookmark.el
    !> bookmark-inhibit-context-functions.
    type(bookmark_inhibit_context_function_type), allocatable, public :: bookmark_inhibit_context_functions(:)

    !> Keymap of the minibuffer while reading a bookmark name: C-w is
    !> `bookmark_yank_word'.  Port of bookmark.el
    !> bookmark-minibuffer-read-name-map.
    type(handle_type), public :: bookmark_minibuffer_read_name_map

    !> Function to return default text to use for a bookmark annotation.
    !> It takes the name of the bookmark, as a string, and returns the text.  Port
    !> of bookmark.el bookmark-edit-annotation-text-func.
    procedure(bookmark_annotation_text_proc), pointer, public :: bookmark_edit_annotation_text_func => null()

    !> Keymap for editing an annotation of a bookmark.  Port of bookmark.el
    !> bookmark-edit-annotation-mode-map.
    type(handle_type), public :: bookmark_edit_annotation_mode_map

    !> Hook run when entering `bookmark_edit_annotation_mode'.
    type(hook_type), target, public :: bookmark_edit_annotation_mode_hook = &
                                       hook_type(name="bookmark_edit_annotation_mode_hook")

    !> Hook run after `bookmark_jump' jumps to a bookmark.
    !> Useful for example to unhide text in `outline_mode'.  Port of
    !> bookmark.el bookmark-after-jump-hook.
    type(hook_type), target, public :: bookmark_after_jump_hook = hook_type(name="bookmark_after_jump_hook")

    !> Hook run after a bookmark file is loaded by `bookmark_load'.
    !> This hook can be used, for example, to reconcile `bookmark_table'
    !> against bookmark state that you, or a package that you use,
    !> maintains.  Port of bookmark.el bookmark-after-load-file-hook.
    type(hook_type), target, public :: bookmark_after_load_file_hook = &
                                       hook_type(name="bookmark_after_load_file_hook")

    !> Hook run when Fortmacs exits.  Port of bookmark.el
    !> bookmark-exit-hook.
    type(hook_type), target, public :: bookmark_exit_hook = hook_type(name="bookmark_exit_hook")

    !> Name of buffer used for Bookmark List.  Port of bookmark.el
    !> bookmark-bmenu-buffer.
    character(len=:), allocatable, public :: bookmark_bmenu_buffer

    !> Number of columns (chars) used for the *Bookmark List* marks column.
    !> This includes the annotations column.  Port of bookmark.el
    !> bookmark-bmenu-marks-width.
    integer, parameter, public :: BOOKMARK_BMENU_MARKS_WIDTH = 2

    !> Column at which to display filenames in a buffer listing bookmarks.
    !> You can toggle whether files are shown with
    !> \<bookmark_bmenu_mode_map>\[bookmark_bmenu_toggle_filenames].  Port
    !> of bookmark.el bookmark-bmenu-file-column.
    integer, public :: bookmark_bmenu_file_column = 30

    !> Column width for bookmark type in a buffer listing bookmarks.  Port
    !> of bookmark.el bookmark-bmenu-type-column-width.
    integer, public :: bookmark_bmenu_type_column_width = 8

    !> True means show filenames when listing bookmarks.
    !> A true value may result in truncated bookmark names.  Port of
    !> bookmark.el bookmark-bmenu-toggle-filenames.
    logical, public :: bookmark_bmenu_toggle_filenames = .true.

    !> Face used to highlight bookmark names in bookmark menu buffers.
    !> Port of bookmark.el bookmark-menu-bookmark.
    type(named_face_type), target, public :: bookmark_menu_bookmark

    !> Time before `bookmark_bmenu_search' updates the display.  Port of
    !> bookmark.el bookmark-search-delay.
    real, public :: bookmark_search_delay = 0.2

    !> Keymap for `bookmark_bmenu_mode'.  Port of bookmark.el
    !> bookmark-bmenu-mode-map.
    type(handle_type), public :: bookmark_bmenu_mode_map

    !> Hook run when entering `bookmark_bmenu_mode'.
    type(hook_type), target, public :: bookmark_bmenu_mode_hook = hook_type(name="bookmark_bmenu_mode_hook")

    !> The idle timer of `bookmark_bmenu_search' and its minibuffer.
    type(handle_type) :: search_timer, search_minibuffer

    !> Values of OVERWRITE_OR_PUSH for `bookmark_set_internal' (BOOKMARK_SET_ERROR,
    !> BOOKMARK_SET_OVERWRITE and BOOKMARK_SET_PUSH).
    integer, parameter, public :: BOOKMARK_SET_ERROR = 0, BOOKMARK_SET_OVERWRITE = 1, BOOKMARK_SET_PUSH = 2

    !> The buffer-local variables: `bookmark_current_bookmark' (name of
    !> bookmark most recently used in the current file),
    !> `bookmark_annotation_name' (name of bookmark under edit in
    !> `bookmark_edit_annotation_mode') and
    !> `bookmark__annotation_from_bookmark_list'.
    type :: local_type
        integer(int64) :: serial = 0
        character(len=:), allocatable :: current_bookmark
        character(len=:), allocatable :: annotation_name
        logical :: annotation_from_bookmark_list = .false.
    end type local_type

    type(local_type), allocatable :: locals(:)

    !> Mtime for which we already queried about reloading (bookmark.el
    !> bookmark--watch-already-asked-mtime).
    logical :: watch_asked_known = .false., watch_asked_nil = .false.
    integer(int64) :: watch_asked_sec = 0
    integer(int32) :: watch_asked_nsec = 0

    !> The history list for bookmark functions.  Port of bookmark.el
    !> bookmark-history.
    type(history_setting), target, public :: bookmark_history = history_setting(name="bookmark_history")

contains

    ! ------------------------------------------------------------------
    ! Buffer-local variables
    ! ------------------------------------------------------------------

    ! The index of the current buffer's entry in `locals', made if MAKE
    ! (0 if there is none).  Not a port.
    integer function local_index(make) result(k)
        logical, intent(in) :: make
        type(local_type), allocatable :: grown(:)
        integer :: i, n

        k = 0
        if (cur == 0) return
        if (.not. allocated(locals)) allocate (locals(0))

        do k = 1, size(locals)
            if (locals(k)%serial == buffers(cur)%serial) return
        end do

        k = 0
        if (.not. make) return

        do k = 1, size(locals)
            if (.not. buffer_serial_live(locals(k)%serial)) then
                locals(k) = local_type(serial=buffers(cur)%serial)
                return
            end if
        end do

        n = size(locals)
        allocate (grown(n + 1))

        do i = 1, n
            grown(i) = locals(i)
        end do

        grown(n + 1)%serial = buffers(cur)%serial
        call move_alloc(grown, locals)
        k = n + 1
    end function local_index

    ! kill-all-local-variables: forget the current buffer's values.
    ! Not a port.
    subroutine kill_bookmark_locals()
        integer :: k

        k = local_index(.false.)
        if (k > 0) locals(k) = local_type()
    end subroutine kill_bookmark_locals

    !> Name of bookmark most recently used in the current file ("" for
    !> none).  It is buffer local, used to make moving a bookmark forward
    !> through a file easier.  Port of bookmark.el bookmark-current-bookmark.
    function bookmark_current_bookmark() result(name)
        character(len=:), allocatable :: name
        integer :: k

        name = ""
        k = local_index(.false.)
        if (k == 0) return
        if (allocated(locals(k)%current_bookmark)) name = locals(k)%current_bookmark
    end function bookmark_current_bookmark

    !> Set the current buffer's `bookmark_current_bookmark' to NAME ("" if
    !> NAME is absent).  Not a port.
    subroutine set_bookmark_current_bookmark(name)
        character(len=*), intent(in), optional :: name
        integer :: k

        k = local_index(present(name))
        if (k == 0) return

        if (present(name)) then
            locals(k)%current_bookmark = name
        else if (allocated(locals(k)%current_bookmark)) then
            deallocate (locals(k)%current_bookmark)
        end if
    end subroutine set_bookmark_current_bookmark

    ! Whether the current buffer's `bookmark_current_bookmark' is allocated.
    ! Not a port.
    logical function have_current_bookmark()
        integer :: k

        have_current_bookmark = .false.
        k = local_index(.false.)
        if (k > 0) have_current_bookmark = allocated(locals(k)%current_bookmark)
    end function have_current_bookmark

    ! ------------------------------------------------------------------
    ! Records
    ! ------------------------------------------------------------------

    ! Not a port.
    pure logical function same(a, b)
        character(len=*), intent(in) :: a, b

        same = len(a) == len(b)
        if (same) same = a == b
    end function same

    !> Return the name of BOOKMARK_RECORD.
    !> BOOKMARK_RECORD is, e.g., one element from `bookmark_table'.
    !> Port of bookmark.el:bookmark-name-from-full-record.
    function bookmark_name_from_full_record(bookmark_record) result(name)
        type(bookmark_type), intent(in) :: bookmark_record
        character(len=:), allocatable :: name

        name = ""
        if (allocated(bookmark_record%name)) name = bookmark_record%name
    end function bookmark_name_from_full_record

    !> Return a list of all current bookmark names.  Port of
    !> bookmark.el:bookmark-all-names.
    recursive function bookmark_all_names() result(names)
        type(minibuffer_string_type), allocatable :: names(:)
        type(status_type) :: st
        integer :: k

        call bookmark_maybe_load_default_file(st)
        allocate (names(size(bookmark_table)))

        do k = 1, size(bookmark_table)
            names(k)%s = bookmark_name_from_full_record(bookmark_table(k))
        end do
    end function bookmark_all_names

    !> Return the index in `bookmark_table' of the bookmark named
    !> BOOKMARK_NAME; error if there is none.  If optional argument NOERROR
    !> is true, return 0 instead of making an error condition occur.  Port of
    !> bookmark.el:bookmark-get-bookmark (for a name).
    integer function bookmark_get_bookmark(bookmark_name, status, noerror) result(k)
        character(len=*), intent(in) :: bookmark_name
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: noerror

        status = status_type()
        if (.not. allocated(bookmark_table)) allocate (bookmark_table(0))

        do k = 1, size(bookmark_table)
            if (name_matches(bookmark_table(k), bookmark_name)) return
        end do

        k = 0

        if (present(noerror)) then
            if (noerror) return
        end if

        call set_error(status, FM_GENERIC_ERROR, "Invalid bookmark "//bookmark_name)
    end function bookmark_get_bookmark

    ! Whether RECORD is named NAME, as assoc-string compares them (ignoring
    ! case if `bookmark_completion_ignore_case').  Not a port.
    logical function name_matches(record, name) result(match)
        use fns_fm, only: compare_strings, COMPARE_EQUAL
        type(bookmark_type), intent(in) :: record
        character(len=*), intent(in) :: name

        match = .false.
        if (.not. allocated(record%name)) return

        if (bookmark_completion_ignore_case) then
            match = compare_strings(record%name, name, ignore_case=.true.) == COMPARE_EQUAL
        else
            match = same(record%name, name)
        end if
    end function name_matches

    !> Set BOOKMARK_NAME's name to NEWNAME.  Port of
    !> bookmark.el:bookmark-set-name.
    subroutine bookmark_set_name(bookmark_name, newname, status)
        character(len=*), intent(in) :: bookmark_name, newname
        type(status_type), intent(out) :: status
        integer :: k

        k = bookmark_get_bookmark(bookmark_name, status)
        if (k > 0) bookmark_table(k)%name = newname
    end subroutine bookmark_set_name

    !> Return the annotation of BOOKMARK_NAME, or "" if none.  Port of
    !> bookmark.el:bookmark-get-annotation.
    function bookmark_get_annotation(bookmark_name) result(annotation)
        character(len=*), intent(in) :: bookmark_name
        character(len=:), allocatable :: annotation
        type(status_type) :: st
        integer :: k

        annotation = ""
        k = bookmark_get_bookmark(bookmark_name, st, noerror=.true.)
        if (k == 0) return
        if (allocated(bookmark_table(k)%annotation)) annotation = bookmark_table(k)%annotation
    end function bookmark_get_annotation

    !> Set the annotation of BOOKMARK_NAME to ANN.  Port of
    !> bookmark.el:bookmark-set-annotation.
    subroutine bookmark_set_annotation(bookmark_name, ann, status)
        character(len=*), intent(in) :: bookmark_name, ann
        type(status_type), intent(out) :: status
        integer :: k

        k = bookmark_get_bookmark(bookmark_name, status)
        if (k > 0) bookmark_table(k)%annotation = ann
    end subroutine bookmark_set_annotation

    !> Return the full filename of BOOKMARK_NAME, or "" if none.  Port of
    !> bookmark.el:bookmark-get-filename.
    function bookmark_get_filename(bookmark_name) result(filename)
        character(len=*), intent(in) :: bookmark_name
        character(len=:), allocatable :: filename
        type(status_type) :: st
        integer :: k

        filename = ""
        k = bookmark_get_bookmark(bookmark_name, st, noerror=.true.)
        if (k == 0) return
        if (allocated(bookmark_table(k)%filename)) filename = bookmark_table(k)%filename
    end function bookmark_get_filename

    !> Set the full filename of BOOKMARK_NAME to FILENAME.  Port of
    !> bookmark.el:bookmark-set-filename.
    subroutine bookmark_set_filename(bookmark_name, filename, status)
        character(len=*), intent(in) :: bookmark_name, filename
        type(status_type), intent(out) :: status
        integer :: k

        k = bookmark_get_bookmark(bookmark_name, status)
        if (k > 0) bookmark_table(k)%filename = filename
    end subroutine bookmark_set_filename

    !> Return the position (i.e.: point) of BOOKMARK_NAME, or 0 if none.
    !> Port of bookmark.el:bookmark-get-position.
    integer(int64) function bookmark_get_position(bookmark_name) result(position)
        character(len=*), intent(in) :: bookmark_name
        type(status_type) :: st
        integer :: k

        position = 0
        k = bookmark_get_bookmark(bookmark_name, st, noerror=.true.)
        if (k > 0) position = bookmark_table(k)%position
    end function bookmark_get_position

    !> Set the position (i.e.: point) of BOOKMARK_NAME to POSITION.
    !> Port of bookmark.el:bookmark-set-position.
    subroutine bookmark_set_position(bookmark_name, position, status)
        character(len=*), intent(in) :: bookmark_name
        integer(int64), intent(in) :: position
        type(status_type), intent(out) :: status
        integer :: k

        k = bookmark_get_bookmark(bookmark_name, status)
        if (k > 0) bookmark_table(k)%position = position
    end subroutine bookmark_set_position

    !> Return the front-context-string of BOOKMARK_NAME, or "" if none.
    !> Port of bookmark.el:bookmark-get-front-context-string.
    function bookmark_get_front_context_string(bookmark_name) result(string)
        character(len=*), intent(in) :: bookmark_name
        character(len=:), allocatable :: string
        type(status_type) :: st
        integer :: k

        string = ""
        k = bookmark_get_bookmark(bookmark_name, st, noerror=.true.)
        if (k == 0) return
        if (allocated(bookmark_table(k)%front_context_string)) string = bookmark_table(k)%front_context_string
    end function bookmark_get_front_context_string

    !> Set the front-context-string of BOOKMARK_NAME to STRING.  Port of
    !> bookmark.el:bookmark-set-front-context-string.
    subroutine bookmark_set_front_context_string(bookmark_name, string, status)
        character(len=*), intent(in) :: bookmark_name, string
        type(status_type), intent(out) :: status
        integer :: k

        k = bookmark_get_bookmark(bookmark_name, status)
        if (k > 0) bookmark_table(k)%front_context_string = string
    end subroutine bookmark_set_front_context_string

    !> Return the rear-context-string of BOOKMARK_NAME, or "" if none.
    !> Port of bookmark.el:bookmark-get-rear-context-string.
    function bookmark_get_rear_context_string(bookmark_name) result(string)
        character(len=*), intent(in) :: bookmark_name
        character(len=:), allocatable :: string
        type(status_type) :: st
        integer :: k

        string = ""
        k = bookmark_get_bookmark(bookmark_name, st, noerror=.true.)
        if (k == 0) return
        if (allocated(bookmark_table(k)%rear_context_string)) string = bookmark_table(k)%rear_context_string
    end function bookmark_get_rear_context_string

    !> Set the rear-context-string of BOOKMARK_NAME to STRING.  Port of
    !> bookmark.el:bookmark-set-rear-context-string.
    subroutine bookmark_set_rear_context_string(bookmark_name, string, status)
        character(len=*), intent(in) :: bookmark_name, string
        type(status_type), intent(out) :: status
        integer :: k

        k = bookmark_get_bookmark(bookmark_name, status)
        if (k > 0) bookmark_table(k)%rear_context_string = string
    end subroutine bookmark_set_rear_context_string

    ! The current time as a last-modified time.  Not a port.
    function current_time() result(time)
        integer(int64), allocatable :: time(:)
        integer(int64) :: sec
        integer(int32) :: nsec

        call emacs_clock_realtime(sec, nsec)
        allocate (time(2))
        time(1) = sec
        time(2) = int(nsec, int64)
    end function current_time

    !> Update the last-modified date of BOOKMARK_NAME to the current time.
    !> Port of bookmark.el:bookmark-update-last-modified.
    subroutine bookmark_update_last_modified(bookmark_name, status)
        character(len=*), intent(in) :: bookmark_name
        type(status_type), intent(out) :: status
        integer :: k

        k = bookmark_get_bookmark(bookmark_name, status)
        if (k > 0) bookmark_table(k)%last_modified = current_time()
    end subroutine bookmark_update_last_modified

    ! Copy record FROM into TO, moving its allocations.  Not a port.
    subroutine move_record(from, to)
        type(bookmark_type), intent(inout) :: from
        type(bookmark_type), intent(out) :: to

        if (allocated(from%name)) call move_alloc(from%name, to%name)
        if (allocated(from%filename)) call move_alloc(from%filename, to%filename)
        to%position = from%position
        if (allocated(from%front_context_string)) call move_alloc(from%front_context_string, to%front_context_string)
        if (allocated(from%rear_context_string)) call move_alloc(from%rear_context_string, to%rear_context_string)
        if (allocated(from%annotation)) call move_alloc(from%annotation, to%annotation)
        if (allocated(from%last_modified)) call move_alloc(from%last_modified, to%last_modified)
        if (allocated(from%defaults)) call move_alloc(from%defaults, to%defaults)
    end subroutine move_record

    ! Put RECORD at the front of `bookmark_table'.  Not a port.
    subroutine push_record(record)
        type(bookmark_type), intent(in) :: record
        type(bookmark_type), allocatable :: grown(:)
        integer :: k, n

        if (.not. allocated(bookmark_table)) allocate (bookmark_table(0))
        n = size(bookmark_table)
        allocate (grown(n + 1))
        grown(1) = record

        do k = 1, n
            call move_record(bookmark_table(k), grown(k + 1))
        end do

        call move_alloc(grown, bookmark_table)
    end subroutine push_record

    ! Put RECORD at the end of `bookmark_table'.  Not a port.
    subroutine append_record(record)
        type(bookmark_type), intent(in) :: record
        type(bookmark_type), allocatable :: grown(:)
        integer :: k, n

        if (.not. allocated(bookmark_table)) allocate (bookmark_table(0))
        n = size(bookmark_table)
        allocate (grown(n + 1))

        do k = 1, n
            call move_record(bookmark_table(k), grown(k))
        end do

        grown(n + 1) = record
        call move_alloc(grown, bookmark_table)
    end subroutine append_record

    ! Remove element K of `bookmark_table'.  Not a port.
    subroutine delete_record(k)
        integer, intent(in) :: k
        type(bookmark_type), allocatable :: kept(:)
        integer :: j

        allocate (kept(size(bookmark_table) - 1))

        do j = 1, size(kept)
            call move_record(bookmark_table(merge(j, j + 1, j < k)), kept(j))
        end do

        call move_alloc(kept, bookmark_table)
    end subroutine delete_record

    ! Make `bookmark_table' empty.  Not a port.
    subroutine clear_table()
        if (allocated(bookmark_table)) deallocate (bookmark_table)
        allocate (bookmark_table(0))
    end subroutine clear_table

    ! Whether time A is before time B (time-less-p).  Not a port.
    pure logical function is_time_less(a, b)
        integer(int64), intent(in) :: a(:), b(:)

        if (a(1) /= b(1)) then
            is_time_less = a(1) < b(1)
        else
            is_time_less = a(2) < b(2)
        end if
    end function is_time_less

    ! Whether name A sorts before name B (string-lessp: by character code,
    ! which is the order of the UTF-8 bytes).  Not a port.
    pure logical function is_name_less(a, b)
        character(len=*), intent(in) :: a, b
        integer :: k

        do k = 1, min(len(a), len(b))
            if (a(k:k) /= b(k:k)) then
                is_name_less = iachar(a(k:k)) < iachar(b(k:k))
                return
            end if
        end do

        is_name_less = len(a) < len(b)
    end function is_name_less

    !> Return `bookmark_table' for display.
    !> If `bookmark_sort_flag' is BOOKMARK_SORT_FLAG_ALPHABETICAL, then return a
    !> sorted by name copy of the table.  If `bookmark_sort_flag' is
    !> BOOKMARK_SORT_FLAG_LAST_MODIFIED, then return a sorted by last
    !> modified copy of the table.  Otherwise, just return `bookmark_table',
    !> which by default is ordered from most recently created to least
    !> recently created bookmark.  Port of
    !> bookmark.el:bookmark-maybe-sort-alist.
    function bookmark_maybe_sort_table() result(copy)
        type(bookmark_type), allocatable :: copy(:)
        type(bookmark_type) :: t
        integer :: i, j

        if (.not. allocated(bookmark_table)) allocate (bookmark_table(0))
        allocate (copy(size(bookmark_table)))

        do i = 1, size(bookmark_table)
            copy(i) = bookmark_table(i)
        end do

        if (bookmark_sort_flag == BOOKMARK_SORT_FLAG_CREATED) return

        ! A stable insertion sort (`sort' is stable).
        do i = 2, size(copy)
            call move_record(copy(i), t)
            j = i - 1

            do while (j >= 1)
                if (.not. before(t, copy(j))) exit
                call move_record(copy(j), copy(j + 1))
                j = j - 1
            end do

            call move_record(t, copy(j + 1))
        end do

    contains

        logical function before(x, y)
            type(bookmark_type), intent(in) :: x, y

            if (bookmark_sort_flag == BOOKMARK_SORT_FLAG_LAST_MODIFIED) then
                if (.not. allocated(x%last_modified)) then
                    before = .false.
                else if (.not. allocated(y%last_modified)) then
                    before = .true.
                else
                    before = is_time_less(y%last_modified, x%last_modified)
                end if
            else
                before = is_name_less(bookmark_name_from_full_record(x), bookmark_name_from_full_record(y))
            end if
        end function before
    end function bookmark_maybe_sort_table

    !> Prompting with PROMPT, read a bookmark name in completion.
    !> PROMPT will get a ": " stuck on the end no matter what, so you
    !> probably don't want to include one yourself.
    !> Optional arg DEFAULT is a string to return if the user input is
    !> empty.  If DEFAULT is absent then return empty string for empty
    !> input.  Port of bookmark.el:bookmark-completing-read (no popup menu
    !> for a mouse event).
    recursive subroutine bookmark_completing_read(prompt, result, status, default)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: default
        type(completion_table_type) :: table
        type(value_type), allocatable :: names(:)
        character(len=:), allocatable :: def
        logical :: saved_case
        integer :: k

        result = ""
        call bookmark_maybe_load_default_file(status)
        if (.not. status%ok()) return
        def = ""
        if (present(default)) def = default
        allocate (names(size(bookmark_table)))

        do k = 1, size(bookmark_table)
            names(k) = value_type(text=bookmark_name_from_full_record(bookmark_table(k)))
        end do

        table%kind = TABLE_LIST
        table%list = make_list(names)
        table%category = "bookmark"
        saved_case = completion_ignore_case%value
        completion_ignore_case%value = bookmark_completion_ignore_case

        if (len(def) > 0) then
            call completing_read(format_prompt(prompt, def), table, result, status, &
                                 require_match=REQUIRE_MATCH_OTHER, history="bookmark_history", default=def)
        else
            call completing_read(format_prompt(prompt, def), table, result, status, &
                                 require_match=REQUIRE_MATCH_OTHER, history="bookmark_history")
        end if

        completion_ignore_case%value = saved_case
    end subroutine bookmark_completing_read

    ! PROMPT with DEFAULT, as format-prompt makes it.  Port of
    ! minibuffer.el:format-prompt (a string DEFAULT only).
    function format_prompt(prompt, default) result(full)
        character(len=*), intent(in) :: prompt, default
        character(len=:), allocatable :: full

        full = prompt
        if (len(default) > 0) full = full//" (default "//default//")"
        full = full//": "
    end function format_prompt

    !> Put STRING into the bookmark prompt history, if caller
    !> non-interactive (CALLED_INTERACTIVELY false).  We need this because
    !> sometimes bookmark functions are invoked from other commands that
    !> pass in the bookmark name, so `completing_read' never gets a chance
    !> to set `bookmark_history'.  Port of
    !> bookmark.el:bookmark-maybe-historicize-string.
    subroutine bookmark_maybe_historicize_string(string, called_interactively)
        character(len=*), intent(in) :: string
        logical, intent(in), optional :: called_interactively

        if (present(called_interactively)) then
            if (called_interactively) return
        end if

        call add_to_history("bookmark_history", string)
    end subroutine bookmark_maybe_historicize_string

    !> Return a new bookmark RECORD for the current location.  Port of
    !> bookmark.el:bookmark-make-record.
    recursive subroutine bookmark_make_record(record, status)
        type(bookmark_type), intent(out) :: record
        type(status_type), intent(out) :: status
        type(minibuffer_string_type), allocatable :: defaults(:)
        integer :: saved_size, k
        logical :: inhibit

        saved_size = bookmark_search_size

        ! If we're in a buffer that's visiting an encrypted file, don't
        ! include any context in the bookmark file, because that would leak
        ! (possibly secret) data.
        if (allocated(buffers(cur)%filename) .and. allocated(bookmark_inhibit_context_functions)) then
            inhibit = .false.

            do k = 1, size(bookmark_inhibit_context_functions)
                if (.not. associated(bookmark_inhibit_context_functions(k)%function)) cycle

                if (bookmark_inhibit_context_functions(k)%function(buffers(cur)%filename)) then
                    inhibit = .true.
                    exit
                end if
            end do

            if (inhibit) bookmark_search_size = 0
        end if

        if (associated(bookmark_make_record_function)) then
            call bookmark_make_record_function(record, status)
        else
            call bookmark_make_record_default(record, status)
        end if

        bookmark_search_size = saved_size
        if (.not. status%ok()) return

        ! Set up default name if the function does not provide one.
        if (.not. allocated(record%name)) then
            if (have_current_bookmark()) then
                record%name = bookmark_current_bookmark()
            else
                record%name = bookmark_buffer_name()
            end if
        end if

        ! Set up defaults.  (Upstream adds them to the record's table, which
        ! is lost when a record has nothing but its name.)
        if (.not. (allocated(record%filename) .or. record%position /= 0 .or. &
                   allocated(record%front_context_string) .or. allocated(record%rear_context_string) .or. &
                   allocated(record%annotation) .or. allocated(record%last_modified) .or. &
                   allocated(record%defaults))) return
        allocate (defaults(0))

        if (allocated(record%defaults)) then
            do k = 1, size(record%defaults)
                call add_default(record%defaults(k)%s)
            end do
        end if

        if (have_current_bookmark()) call add_default(bookmark_current_bookmark())
        call add_default(record%name)
        call add_default(bookmark_buffer_name())
        call move_alloc(defaults, record%defaults)

    contains

        ! (delete-dups (append ...)): keep the first of equal strings.
        subroutine add_default(s)
            character(len=*), intent(in) :: s
            type(minibuffer_string_type), allocatable :: grown(:)
            integer :: j, n

            n = size(defaults)

            do j = 1, n
                if (same(defaults(j)%s, s)) return
            end do

            allocate (grown(n + 1))

            do j = 1, n
                call move_alloc(defaults(j)%s, grown(j)%s)
            end do

            grown(n + 1)%s = s
            call move_alloc(grown, defaults)
        end subroutine add_default
    end subroutine bookmark_make_record

    !> Store the bookmark NAME with data RECORD.
    !> If NO_OVERWRITE is true and another bookmark of the same name already
    !> exists in `bookmark_table', record the new bookmark without discarding
    !> the old one.  Port of bookmark.el:bookmark-store.
    recursive subroutine bookmark_store(name, record, no_overwrite, status)
        character(len=*), intent(in) :: name
        type(bookmark_type), intent(in) :: record
        logical, intent(in) :: no_overwrite
        type(status_type), intent(out) :: status
        type(bookmark_type) :: bm
        type(status_type) :: st
        integer :: k

        call bookmark_maybe_load_default_file(status)
        if (.not. status%ok()) return
        bm = record
        bm%name = name
        if (allocated(bm%defaults)) deallocate (bm%defaults)
        k = 0
        if (.not. no_overwrite) k = bookmark_get_bookmark(name, st, noerror=.true.)

        if (k > 0) then
            ! Already existing bookmark under that name and no prefix arg
            ! means just overwrite old bookmark.  First clean up if
            ! previously location was fontified.
            if (allocated(bookmark_fringe_mark)) call bookmark_remove_fringe_mark(bookmark_table(k))
            ! (Its name stays as it was.)
            call move_alloc(bookmark_table(k)%name, bm%name)
            bookmark_table(k) = bm
        else
            ! Otherwise just put it onto the front of the list.  Either the
            ! bookmark doesn't exist already, or there is no prefix arg.  In
            ! either case, we want the new bookmark on the front of the
            ! list, since the list is kept in reverse order of creation.
            call push_record(bm)
        end if

        call set_bookmark_current_bookmark(name)
        bookmark_table_modification_count = bookmark_table_modification_count + 1
        if (is_bookmark_time_to_save()) call bookmark_save(status)
        call set_bookmark_current_bookmark(name)
        call bookmark_bmenu_surreptitiously_rebuild_list()
    end subroutine bookmark_store

    !> Return the record describing the location of a new bookmark.
    !> Point should be at the buffer in which the bookmark is being set,
    !> and normally should be at the position where the bookmark is
    !> desired, but see the optional arguments for other possibilities.
    !>
    !> If NO_FILE is true, then only return the subset of the record that
    !> pertains to the location within the buffer, leaving off the part
    !> that records the filename.
    !>
    !> If NO_CONTEXT is true, do not include the front- and rear-context
    !> strings in the record -- the position is enough.
    !>
    !> If POSN is present, record POSN as the point instead of `point'.
    !> Port of bookmark.el:bookmark-make-record-default.
    recursive subroutine bookmark_make_record_default(record, status, no_file, no_context, posn)
        type(bookmark_type), intent(out) :: record
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_file, no_context
        integer(int64), intent(in), optional :: posn
        character(len=:), allocatable :: text
        logical :: file, context

        file = .true.
        if (present(no_file)) file = .not. no_file
        context = .true.
        if (present(no_context)) context = .not. no_context
        status = status_type()

        if (file) then
            call bookmark_buffer_file_name(record%filename, status)
            if (.not. status%ok()) return
        end if

        if (context) then
            if (point_max() - point() >= bookmark_search_size) then
                call buffer_substring(point(), point() + bookmark_search_size, text, status)
                record%front_context_string = text
            end if

            if (point() - point_min() >= bookmark_search_size) then
                call buffer_substring(point(), point() - bookmark_search_size, text, status)
                record%rear_context_string = text
            end if
        end if

        record%position = point()
        if (present(posn)) record%position = posn
        record%last_modified = current_time()
    end subroutine bookmark_make_record_default

    ! Not a port.
    recursive subroutine make_record_default(record, status)
        type(bookmark_type), intent(out) :: record
        type(status_type), intent(out) :: status

        call bookmark_make_record_default(record, status)
    end subroutine make_record_default

    ! ------------------------------------------------------------------
    ! File format
    ! ------------------------------------------------------------------

    !> Return a `bookmark_table' LIST from the current buffer.
    !> The buffer must of course contain bookmark format information.
    !> Does not care from where in the buffer it is called, and does not
    !> affect point.  Port of bookmark.el:bookmark-alist-from-buffer (a
    !> `bookmark' namelist group where upstream looks for the end of the
    !> version stamp).
    recursive subroutine bookmark_table_from_buffer(list, status)
        type(bookmark_type), allocatable, intent(out) :: list(:)
        type(status_type), intent(out) :: status
        type(namelist_group_type), allocatable :: groups(:)
        type(bookmark_type), allocatable :: records(:)
        logical :: found
        integer :: g, k, n, idx, m

        call read_namelist(buffer_string(), groups, status)
        if (.not. status%ok()) return
        found = .false.
        n = 0

        do g = 1, size(groups)
            if (groups(g)%name /= "bookmark") cycle
            found = .true.
            if (.not. allocated(groups(g)%items)) cycle

            do k = 1, size(groups(g)%items)
                if (groups(g)%items(k)%name == "bookmark") n = max(n, int(groups(g)%items(k)%lower))
            end do
        end do

        if (.not. found) then
            if (allocated(buffers(cur)%filename)) then
                call set_error(status, FM_GENERIC_ERROR, "File not in bookmark format: "//buffers(cur)%filename)
            else
                call set_error(status, FM_GENERIC_ERROR, "Buffer not in bookmark format: "//buffers(cur)%name)
            end if

            return
        end if

        allocate (records(n))

        do g = 1, size(groups)
            if (groups(g)%name /= "bookmark" .or. .not. allocated(groups(g)%items)) cycle

            do k = 1, size(groups(g)%items)
                associate (it => groups(g)%items(k))
                    if (it%name /= "bookmark" .or. .not. it%subscripted) cycle
                    if (size(it%values) == 0) cycle
                    idx = int(it%lower)
                    if (idx < 1) cycle

                    select case (it%component)
                    case ("name")
                        if (it%values(1)%kind == NAMELIST_STRING) records(idx)%name = it%values(1)%string
                    case ("filename")
                        if (it%values(1)%kind == NAMELIST_STRING) records(idx)%filename = it%values(1)%string
                    case ("position")
                        if (it%values(1)%kind == NAMELIST_INTEGER) records(idx)%position = it%values(1)%integer
                    case ("front_context_string")
                        if (it%values(1)%kind == NAMELIST_STRING) then
                            records(idx)%front_context_string = it%values(1)%string
                        end if
                    case ("rear_context_string")
                        if (it%values(1)%kind == NAMELIST_STRING) then
                            records(idx)%rear_context_string = it%values(1)%string
                        end if
                    case ("annotation")
                        if (it%values(1)%kind == NAMELIST_STRING) records(idx)%annotation = it%values(1)%string
                    case ("last_modified")
                        if (size(it%values) == 2) then
                            if (it%values(1)%kind == NAMELIST_INTEGER .and. it%values(2)%kind == NAMELIST_INTEGER) then
                                allocate (records(idx)%last_modified(2))
                                records(idx)%last_modified(1) = it%values(1)%integer
                                records(idx)%last_modified(2) = it%values(2)%integer
                            end if
                        end if
                    case default
                        ! Not a component of a bookmark.
                        continue
                    end select
                end associate
            end do
        end do

        ! The records with a name.
        m = 0

        do k = 1, n
            if (allocated(records(k)%name)) m = m + 1
        end do

        allocate (list(m))
        m = 0

        do k = 1, n
            if (.not. allocated(records(k)%name)) cycle
            m = m + 1
            call move_record(records(k), list(m))
        end do
    end subroutine bookmark_table_from_buffer

    ! The text of the bookmark file for `bookmark_table', its coding
    ! system named CODING.  Not a port.
    function bookmark_file_text(coding) result(text)
        character(len=*), intent(in) :: coding
        character(len=:), allocatable :: text
        type(namelist_group_type) :: group
        integer :: k

        group%name = "bookmark"

        do k = 1, size(bookmark_table)
            associate (b => bookmark_table(k))
                call add_string("name", bookmark_name_from_full_record(b))
                if (allocated(b%filename)) call add_string("filename", b%filename)
                if (allocated(b%front_context_string)) call add_string("front_context_string", b%front_context_string)
                if (allocated(b%rear_context_string)) call add_string("rear_context_string", b%rear_context_string)
                if (b%position /= 0) call add_integers("position", [b%position])
                if (allocated(b%annotation)) call add_string("annotation", b%annotation)
                if (allocated(b%last_modified)) call add_integers("last_modified", b%last_modified)
            end associate
        end do

        text = "! Fortmacs bookmarks -*- coding: "//coding//" -*-"//achar(10)// &
               "! This format is meant to be slightly human-readable;"//achar(10)// &
               "! nevertheless, you probably don't want to edit it."//achar(10)//namelist_text(group)

    contains

        subroutine add_string(component, value)
            character(len=*), intent(in) :: component, value
            type(namelist_item_type) :: item

            item = namelist_item_type(name="bookmark", component=component, subscripted=.true., lower=k, upper=k)
            call item%add_string(value)
            call group%add(item)
        end subroutine add_string

        subroutine add_integers(component, values)
            character(len=*), intent(in) :: component
            integer(int64), intent(in) :: values(:)
            type(namelist_item_type) :: item
            integer :: j

            item = namelist_item_type(name="bookmark", component=component, subscripted=.true., lower=k, upper=k)

            do j = 1, size(values)
                call item%add_integer(values(j))
            end do

            call group%add(item)
        end subroutine add_integers
    end function bookmark_file_text

    ! ------------------------------------------------------------------
    ! Core code
    ! ------------------------------------------------------------------

    !> Set a bookmark using specified NAME or prompting with PROMPT.
    !> The bookmark is set at the current location.
    !>
    !> If NAME is present, use it as the name of the new bookmark.  In this
    !> case, the value of PROMPT is ignored.
    !>
    !> Otherwise, prompt the user for the bookmark name.  Begin the
    !> interactive prompt with PROMPT, followed by a space, a generated
    !> default name in parentheses, a colon and a space.
    !>
    !> OVERWRITE_OR_PUSH controls what happens if there is already a
    !> bookmark with the same name: BOOKMARK_SET_ERROR means an error condition
    !> occurs; BOOKMARK_SET_OVERWRITE means replace any existing bookmark;
    !> BOOKMARK_SET_PUSH means push the new bookmark onto the bookmark
    !> table.  The push behavior means that among bookmarks with the same
    !> name, this most recently set one becomes the one in effect, but the
    !> others are still there, in order, if the topmost one is ever
    !> deleted.  Port of bookmark.el:bookmark-set-internal.
    recursive subroutine bookmark_set_internal(prompt, overwrite_or_push, status, name)
        character(len=*), intent(in) :: prompt
        integer, intent(in) :: overwrite_or_push
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: name
        type(bookmark_type) :: record
        type(minibuffer_string_type), allocatable :: defaults(:)
        character(len=:), allocatable :: default, str
        type(status_type) :: st

        call set_internal()
        bookmark_yank_point = 0
        bookmark_current_buffer = handle_type()

    contains

        recursive subroutine set_internal()
            character(len=24) :: buf

            call bookmark_make_record(record, status)
            if (.not. status%ok()) return
            ! `defaults' is a transient element of the record.  Bookmark
            ! record functions can use it to specify a list of default
            ! values accessible via M-n while reading a bookmark name.
            call move_alloc(record%defaults, defaults)
            default = ""

            if (allocated(defaults)) then
                if (size(defaults) > 0) default = defaults(1)%s
            end if

            if (.not. allocated(defaults)) then
                ! When no defaults in the record, use its name.
                allocate (defaults(1))
                defaults(1)%s = record%name
                default = record%name
            else if (size(defaults) == 0) then
                deallocate (defaults)
                allocate (defaults(1))
                defaults(1)%s = record%name
                default = record%name
            end if

            call bookmark_maybe_load_default_file(status)
            if (.not. status%ok()) return

            ! Don't set `bookmark_yank_point' and `bookmark_current_buffer'
            ! if they have been already set in another buffer.
            if (.not. (bookmark_yank_point /= 0 .and. is_buffer_live(bookmark_current_buffer))) then
                bookmark_yank_point = point()
                bookmark_current_buffer = current_buffer()
            end if

            if (present(name)) then
                str = name
            else
                call read_from_minibuffer(format_prompt(prompt, default), str, status, &
                                          keymap=bookmark_minibuffer_read_name_map, defaults=defaults)
                if (.not. status%ok()) return
            end if

            if (len(str) == 0) str = default

            select case (overwrite_or_push)
            case (BOOKMARK_SET_ERROR)
                if (bookmark_get_bookmark(str, st, noerror=.true.) > 0) then
                    call set_error(status, FM_GENERIC_ERROR, "A bookmark named """//str//""" already exists")
                    return
                end if

                call bookmark_store(str, record, .false., status)
            case (BOOKMARK_SET_OVERWRITE)
                call bookmark_store(str, record, .false., status)
            case (BOOKMARK_SET_PUSH)
                call bookmark_store(str, record, .true., status)
            case default
                write (buf, "(i0)") overwrite_or_push
                call set_error(status, FM_GENERIC_ERROR, &
                               format_message("Unrecognized value for `overwrite_or_push': ")//trim(buf))
                return
            end select

            if (.not. status%ok()) return

            ! Ask for an annotation buffer for this bookmark
            if (bookmark_use_annotations) then
                call bookmark_edit_annotation(str, status)
                if (.not. status%ok()) return
            end if

            if (allocated(bookmark_fringe_mark)) call bookmark_set_fringe_mark()
        end subroutine set_internal
    end subroutine bookmark_set_internal

    !> Set a bookmark named NAME at the current location.
    !> If NAME is absent, then prompt the user.
    !>
    !> With a prefix arg (true NO_OVERWRITE), do not overwrite any existing
    !> bookmark that has the same name as NAME, but instead push the new
    !> bookmark onto the bookmark table.  The most recently set bookmark
    !> with name NAME is thus the one in effect at any given time, but the
    !> others are still there, should the user decide to delete the most
    !> recent one.
    !>
    !> To yank words from the text of the buffer and use them as part of
    !> the bookmark name, type \<bookmark_minibuffer_read_name_map>\[bookmark_yank_word]
    !> while setting a bookmark.  Successive \[bookmark_yank_word]'s yank
    !> successive words.
    !>
    !> Typing \[universal_argument] inserts (at the bookmark name prompt)
    !> the name of the last bookmark used in the document where the new
    !> bookmark is being set; this helps you use a single bookmark name to
    !> track progress through a large document.  If there is no prior
    !> bookmark for this document, then \[universal_argument] inserts an
    !> appropriate name based on the buffer or file.
    !>
    !> Use \[bookmark_delete] to remove bookmarks (you give it a name and
    !> it removes only the first instance of a bookmark with that name from
    !> the list of bookmarks.)  Port of bookmark.el:bookmark-set.
    recursive subroutine bookmark_set(status, name, no_overwrite)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: name
        logical, intent(in), optional :: no_overwrite
        logical :: push

        push = .false.
        if (present(no_overwrite)) push = no_overwrite

        if (push) then
            call bookmark_set_internal("Add bookmark named", BOOKMARK_SET_PUSH, status, name)
        else
            call bookmark_set_internal("Set bookmark named", BOOKMARK_SET_OVERWRITE, status, name)
        end if
    end subroutine bookmark_set

    recursive subroutine cmd_bookmark_set(status)
        type(status_type), intent(out) :: status

        call bookmark_set(status, no_overwrite=allocated(current_prefix_arg%a))
    end subroutine cmd_bookmark_set

    !> Set a bookmark named NAME at the current location.
    !> If NAME is absent, then prompt the user.
    !>
    !> If a bookmark named NAME already exists and prefix argument
    !> PUSH_BOOKMARK is true, then push the new bookmark onto the bookmark
    !> table.  Pushing it means that among bookmarks named NAME, this one
    !> becomes the one in effect, but the others are still there, in order,
    !> and become effective again if the user ever deletes the most recent
    !> one.
    !>
    !> Otherwise, if a bookmark named NAME already exists but
    !> PUSH_BOOKMARK is false, raise an error.
    !>
    !> To yank words from the text of the buffer and use them as part of
    !> the bookmark name, type \<bookmark_minibuffer_read_name_map>\[bookmark_yank_word]
    !> while setting a bookmark.  Successive \[bookmark_yank_word]'s yank
    !> successive words.
    !>
    !> Typing \[universal_argument] inserts (at the bookmark name prompt)
    !> the name of the last bookmark used in the document where the new
    !> bookmark is being set; this helps you use a single bookmark name to
    !> track progress through a large document.  If there is no prior
    !> bookmark for this document, then \[universal_argument] inserts an
    !> appropriate name based on the buffer or file.
    !>
    !> Use \[bookmark_delete] to remove bookmarks (you give it a name and
    !> it removes only the first instance of a bookmark with that name from
    !> the list of bookmarks.)  Port of
    !> bookmark.el:bookmark-set-no-overwrite.
    recursive subroutine bookmark_set_no_overwrite(status, name, push_bookmark)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: name
        logical, intent(in), optional :: push_bookmark
        logical :: push

        push = .false.
        if (present(push_bookmark)) push = push_bookmark

        if (push) then
            call bookmark_set_internal("Set bookmark", BOOKMARK_SET_PUSH, status, name)
        else
            call bookmark_set_internal("Set bookmark", BOOKMARK_SET_ERROR, status, name)
        end if
    end subroutine bookmark_set_no_overwrite

    recursive subroutine cmd_bookmark_set_no_overwrite(status)
        type(status_type), intent(out) :: status

        call bookmark_set_no_overwrite(status, push_bookmark=allocated(current_prefix_arg%a))
    end subroutine cmd_bookmark_set_no_overwrite

    !> Kill from point to end of line.
    !> If optional arg NEWLINE_TOO is true, delete the newline too.
    !> Does not affect the kill ring.  Port of bookmark.el:bookmark-kill-line.
    subroutine bookmark_kill_line(newline_too)
        logical, intent(in), optional :: newline_too
        type(status_type) :: st

        call delete_region(point(), pos_eol(), st)

        if (present(newline_too)) then
            if (newline_too .and. following_char() == 10) call delete_char(1_int64, st)
        end if
    end subroutine bookmark_kill_line

    !> Return default annotation text for BOOKMARK_NAME.
    !> The default annotation text is simply some text explaining how to
    !> use annotations.  Port of
    !> bookmark.el:bookmark-default-annotation-text.
    function bookmark_default_annotation_text(bookmark_name) result(text)
        character(len=*), intent(in) :: bookmark_name
        character(len=:), allocatable :: text
        character(len=*), parameter :: LF = achar(10)

        text = format_message("#  Type the annotation for bookmark `")//bookmark_name//format_message("' here.")//LF// &
               format_message("#  All lines which start with a `#' will be deleted.")//LF// &
               substitute_command_keys("#  Type \[bookmark_edit_annotation_confirm] when done.  Type "// &
                                       "\[bookmark_edit_annotation_cancel] to cancel."//LF//"#"//LF)// &
               "#  Author: "//user_full_name()//" <"//user_login_name()//"@"//system_name()//">"//LF// &
               "#  Date:   "//current_time_string()//LF
    end function bookmark_default_annotation_text

    ! Not a port.
    subroutine default_annotation_text(name, text)
        character(len=*), intent(in) :: name
        character(len=:), allocatable, intent(out) :: text

        text = bookmark_default_annotation_text(name)
    end subroutine default_annotation_text

    !> Insert annotation for BOOKMARK_NAME at point.  Port of
    !> bookmark.el:bookmark-insert-annotation.
    recursive subroutine bookmark_insert_annotation(bookmark_name, status)
        character(len=*), intent(in) :: bookmark_name
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: annotation, text
        type(status_type) :: st

        if (bookmark_get_bookmark(bookmark_name, st, noerror=.true.) == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Invalid bookmark: "//bookmark_name)
            return
        end if

        if (associated(bookmark_edit_annotation_text_func)) then
            call bookmark_edit_annotation_text_func(bookmark_name, text)
            call insert(text, status)
        else
            call insert(bookmark_default_annotation_text(bookmark_name), status)
        end if

        if (.not. status%ok()) return
        annotation = bookmark_get_annotation(bookmark_name)
        if (len(annotation) > 0) call insert(annotation, status)
    end subroutine bookmark_insert_annotation

    !> Mode for editing the annotation of bookmarks.
    !> \<bookmark_edit_annotation_mode_map>When you have finished
    !> composing, type \[bookmark_edit_annotation_confirm] or
    !> \[bookmark_edit_annotation_cancel] to cancel.
    !>
    !> \{bookmark_edit_annotation_mode_map}
    !>
    !> Port of bookmark.el:bookmark-edit-annotation-mode.
    recursive subroutine bookmark_edit_annotation_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        call major_modes%bookmark_edit_annotation_mode%begin(saved, status)
        if (.not. status%ok()) return
        call major_modes%bookmark_edit_annotation_mode%finish(saved, status)
    end subroutine bookmark_edit_annotation_mode

    ! Display bookmark list after editing if appropriate, OLD_BUFFER being
    ! the annotation buffer.  Port of
    ! bookmark.el:bookmark-edit-annotation--maybe-display-list (the part
    ! after BODY).
    recursive subroutine maybe_display_list(old_buffer, status)
        type(handle_type), intent(in) :: old_buffer
        type(status_type), intent(out) :: status
        type(handle_type) :: list
        character(len=:), allocatable :: name
        logical :: from_bookmark_list
        integer :: k

        from_bookmark_list = .false.
        k = local_index(.false.)
        if (k > 0) from_bookmark_list = locals(k)%annotation_from_bookmark_list
        call quit_window(.false., status)
        if (.not. status%ok()) return
        call bookmark_bmenu_surreptitiously_rebuild_list()

        if (from_bookmark_list) then
            list = get_buffer(bookmark_bmenu_buffer)
            call pop_to_buffer(list, status)
            if (.not. status%ok()) return
            call goto_char(point_min())
            name = bookmark_bmenu_bookmark()
        end if

        call kill_buffer(old_buffer, status)
    end subroutine maybe_display_list

    !> Cancel the current annotation edit.  Port of
    !> bookmark.el:bookmark-edit-annotation-cancel.
    recursive subroutine bookmark_edit_annotation_cancel(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: old_buffer

        old_buffer = current_buffer()
        call message("Canceled by user")
        call maybe_display_list(old_buffer, status)
    end subroutine bookmark_edit_annotation_cancel

    !> Use buffer contents as annotation for a bookmark.
    !> Lines beginning with `#' are ignored.  Port of
    !> bookmark.el:bookmark-edit-annotation-confirm.
    recursive subroutine bookmark_edit_annotation_confirm(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: old_buffer
        character(len=:), allocatable :: annotation, bookmark_name
        integer(int64) :: shortage
        integer :: k

        if (.not. is_derived_mode("bookmark_edit_annotation_mode")) then
            call set_error(status, FM_GENERIC_ERROR, "Not in bookmark_edit_annotation_mode")
            return
        end if

        call goto_char(point_min())

        do while (point() < point_max())
            if (following_char() == iachar("#")) then
                call bookmark_kill_line(.true.)
            else
                shortage = forward_line(1_int64)
            end if
        end do

        ! Take no chances with text properties.
        old_buffer = current_buffer()
        call buffer_substring(point_min(), point_max(), annotation, status)
        if (.not. status%ok()) return
        bookmark_name = ""
        k = local_index(.false.)

        if (k > 0) then
            if (allocated(locals(k)%annotation_name)) bookmark_name = locals(k)%annotation_name
        end if

        call bookmark_set_annotation(bookmark_name, annotation, status)
        if (.not. status%ok()) return
        call bookmark_update_last_modified(bookmark_name, status)
        bookmark_table_modification_count = bookmark_table_modification_count + 1
        call message("Annotation updated for """//bookmark_name//"""")
        call maybe_display_list(old_buffer, status)
    end subroutine bookmark_edit_annotation_confirm

    !> Pop up a buffer for editing bookmark BOOKMARK_NAME's annotation.
    !> If optional argument FROM_BOOKMARK_LIST is true, return to the
    !> bookmark list when editing is done.  Port of
    !> bookmark.el:bookmark-edit-annotation.
    recursive subroutine bookmark_edit_annotation(bookmark_name, status, from_bookmark_list)
        character(len=*), intent(in) :: bookmark_name
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: from_bookmark_list
        type(handle_type) :: buf
        integer :: k

        call get_buffer_create(generate_new_buffer_name("*Bookmark Annotation Compose*"), buf, status)
        if (.not. status%ok()) return
        call pop_to_buffer(buf, status)
        if (.not. status%ok()) return
        call bookmark_edit_annotation_mode(status)
        if (.not. status%ok()) return
        call bookmark_insert_annotation(bookmark_name, status)
        if (.not. status%ok()) return
        k = local_index(.true.)
        locals(k)%annotation_from_bookmark_list = .false.
        if (present(from_bookmark_list)) locals(k)%annotation_from_bookmark_list = from_bookmark_list
        locals(k)%annotation_name = bookmark_name
    end subroutine bookmark_edit_annotation

    !> Return the name of the current buffer in a form usable as a
    !> bookmark name.  If the buffer is associated with a file or
    !> directory, use that name.  Port of bookmark.el:bookmark-buffer-name.
    function bookmark_buffer_name() result(name)
        character(len=:), allocatable :: name
        character(len=:), allocatable :: dirname
        integer :: idx

        if (allocated(buffers(cur)%filename)) then
            ! Or are we a file?
            name = file_name_nondirectory(buffers(cur)%filename)
            return
        end if

        dirname = dired_directory_of()

        if (len(dirname) > 0) then
            ! Or are we a directory?
            idx = len(dirname)

            ! Strip the trailing slash.
            if (dirname(idx:idx) == "/") then
                name = file_name_nondirectory(dirname(1:idx - 1))
            else
                ! Else return the current-buffer
                name = buffers(cur)%name
            end if

            return
        end if

        ! If all else fails, use the buffer's name.
        name = buffers(cur)%name
    end function bookmark_buffer_name

    !> Get the next word from buffer `bookmark_current_buffer' and append
    !> it to the name of the bookmark currently being set, advancing
    !> `bookmark_yank_point' by one word.  Port of
    !> bookmark.el:bookmark-yank-word.
    recursive subroutine bookmark_yank_word(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: old
        character(len=:), allocatable :: string
        type(status_type) :: st
        integer(int64) :: start

        old = current_buffer()
        call set_buffer(bookmark_current_buffer, status)
        if (.not. status%ok()) return
        call goto_char(bookmark_yank_point)
        start = point()
        call forward_word(1_int64)
        bookmark_yank_point = point()
        call buffer_substring(start, point(), string, st)
        call set_buffer(old, status)
        if (.not. status%ok()) return
        call insert(string, status)
    end subroutine bookmark_yank_word

    !> Return the current buffer's file in a way useful for bookmarks.
    !> Port of bookmark.el:bookmark-buffer-file-name.
    recursive subroutine bookmark_buffer_file_name(name, status)
        character(len=:), allocatable, intent(out) :: name
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: dir

        status = status_type()

        ! Abbreviate the path, both so it's shorter and so it's more
        ! portable.  E.g., the user's home dir might be a different path on
        ! different machines, but "~/" will still reach it.
        if (allocated(buffers(cur)%filename)) then
            name = abbreviate_file_name(buffers(cur)%filename)
            return
        end if

        dir = dired_directory_of()

        if (len(dir) > 0) then
            name = abbreviate_file_name(dir)
            return
        end if

        name = ""
        call set_error(status, FM_GENERIC_ERROR, "Buffer not visiting a file or directory")
    end subroutine bookmark_buffer_file_name

    ! Don't ask repeatedly if user already said "no" to reloading a file
    ! with this mtime (NEW_KNOWN false for none, which `time_equal_p' takes
    ! as the current time).  Port of
    ! bookmark.el:bookmark--watch-file-already-queried-p.
    logical function is_watch_file_already_queried(new_known, new_sec, new_nsec) result(queried)
        logical, intent(in) :: new_known
        integer(int64), intent(in) :: new_sec
        integer(int32), intent(in) :: new_nsec

        if (.not. watch_asked_known) then
            ! (Both nil: both the current time.)
            queried = .not. new_known .and. watch_asked_nil
        else
            queried = new_known .and. new_sec == watch_asked_sec .and. new_nsec == watch_asked_nsec
        end if

        watch_asked_known = new_known
        watch_asked_nil = .not. new_known
        watch_asked_sec = new_sec
        watch_asked_nsec = new_nsec
    end function is_watch_file_already_queried

    !> If bookmarks have not been loaded from the default place, load them.
    !> Port of bookmark.el:bookmark-maybe-load-default-file.
    recursive subroutine bookmark_maybe_load_default_file(status)
        type(status_type), intent(out) :: status
        type(file_stat_type) :: stat
        logical :: found, ok

        status = status_type()
        if (.not. allocated(bookmark_table)) allocate (bookmark_table(0))

        if (.not. allocated(bookmark_bookmarks_timestamp%file) .and. size(bookmark_table) == 0 .and. &
            is_file_readable(expand(bookmark_default_file))) then
            call bookmark_load(bookmark_default_file, status, overwrite=.true., no_msg=.true.)
            return
        end if

        if (bookmark_watch_bookmark_file == BOOKMARK_WATCH_BOOKMARK_FILE_NO) return
        found = .false.

        if (allocated(bookmark_bookmarks_timestamp%file)) then
            call file_attributes(bookmark_bookmarks_timestamp%file, stat, found)
        end if

        ! (time-equal-p NEW-MTIME OLD-MTIME): nil is the current time.
        if (found .and. bookmark_bookmarks_timestamp%mtime_known) then
            if (stat%mtime_sec == bookmark_bookmarks_timestamp%mtime_sec .and. &
                stat%mtime_nsec == bookmark_bookmarks_timestamp%mtime_nsec) return
        else if (.not. found .and. .not. bookmark_bookmarks_timestamp%mtime_known) then
            return
        end if

        if (is_watch_file_already_queried(found, stat%mtime_sec, stat%mtime_nsec)) return

        if (bookmark_watch_bookmark_file /= BOOKMARK_WATCH_BOOKMARK_FILE_SILENT) then
            call read_yes_or_no("Bookmarks "//bookmark_bookmarks_timestamp%file//" changed on disk.  Reload? ", &
                                ok, status)
            if (.not. status%ok() .or. .not. ok) return
        end if

        call bookmark_load(bookmark_bookmarks_timestamp%file, status, overwrite=.true., no_msg=.true.)
    end subroutine bookmark_maybe_load_default_file

    ! FILE expanded (expand_file_name), or FILE itself if that fails.
    ! Not a port.
    recursive function expand(file) result(name)
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: name
        type(status_type) :: st

        call expand_file_name(file, name, st)
        if (.not. st%ok()) name = file
    end function expand

    !> Handle BOOKMARK_NAME, then call DISPLAY_FUNCTION.
    !> DISPLAY_FUNCTION is called with the new buffer as argument.
    !>
    !> After calling DISPLAY_FUNCTION, set window point to the point
    !> specified by BOOKMARK_NAME, if necessary, run
    !> `bookmark_after_jump_hook', and then show any annotations for this
    !> bookmark.  Port of bookmark.el:bookmark--jump-via.
    recursive subroutine bookmark_jump_via(bookmark_name, display_function, status)
        character(len=*), intent(in) :: bookmark_name
        procedure(bookmark_display_proc) :: display_function
        type(status_type), intent(out) :: status
        type(window_configuration_type) :: config
        type(handle_type) :: buf, win, ov
        type(handle_type), allocatable :: overlays(:)
        type(status_type) :: st
        integer(int64) :: pt
        logical :: found
        integer :: k

        call current_window_configuration(config)
        call bookmark_handle_bookmark(bookmark_name, status)
        buf = current_buffer()
        pt = point()
        call set_window_configuration(config)
        if (.not. status%ok()) return
        call display_function(buf, status)
        if (.not. status%ok()) return
        win = get_buffer_window(buf)
        if (win%index /= 0) call set_window_point(windows(win%index), pt, st)

        if (allocated(bookmark_fringe_mark)) then
            call overlays_in(pos_bol(), pos_bol() + 1, overlays)
            found = .false.

            do k = 1, size(overlays)
                ov = overlays(k)

                if (is_bookmark_overlay(ov)) then
                    found = .true.
                    exit
                end if
            end do

            if (.not. found) call bookmark_set_fringe_mark()
        end if

        call bookmark_after_jump_hook%execute(status)
        if (.not. status%ok()) return
        ! If there is an annotation for this bookmark, show it in a buffer.
        if (bookmark_automatically_show_annotations) call bookmark_show_annotation(bookmark_name, status)
    end subroutine bookmark_jump_via

    ! Not a port.
    recursive subroutine display_same_window(buffer, status)
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status

        call pop_to_buffer_same_window(buffer, status)
    end subroutine display_same_window

    ! Not a port.
    recursive subroutine display_other_window(buffer, status)
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status

        call switch_to_buffer_other_window(buffer, status)
    end subroutine display_other_window

    !> Jump to bookmark BOOKMARK (a point in some file).
    !> You may have a problem using this function if the value of variable
    !> `bookmark_table' is empty.  If that happens, you need to load in
    !> some bookmarks.  See help on function `bookmark_load' for more about
    !> this.
    !>
    !> If the file pointed to by BOOKMARK no longer exists, you will be
    !> asked if you wish to give the bookmark a new location, and
    !> `bookmark_jump' will then jump to the new location, as well as
    !> recording it in place of the old one in the permanent bookmark
    !> record.
    !>
    !> BOOKMARK is a bookmark name (a string); absent is an error.
    !>
    !> If DISPLAY_FUNC is present, it is a function to invoke to display
    !> the bookmark.  It defaults to `pop_to_buffer_same_window'.  A
    !> typical value for DISPLAY_FUNC would be
    !> `switch_to_buffer_other_window'.  Port of bookmark.el:bookmark-jump.
    recursive subroutine bookmark_jump(bookmark, status, display_func, called_interactively)
        character(len=*), intent(in), optional :: bookmark
        type(status_type), intent(out) :: status
        procedure(bookmark_display_proc), optional :: display_func
        logical, intent(in), optional :: called_interactively

        if (.not. present(bookmark)) then
            call set_error(status, FM_GENERIC_ERROR, "No bookmark specified")
            return
        end if

        call bookmark_maybe_historicize_string(bookmark, called_interactively)

        ! Don't use `switch_to_buffer' because it would let the
        ! window-point override the bookmark's point when
        ! `switch_to_buffer_preserve_window_point' is true.
        if (present(display_func)) then
            call bookmark_jump_via(bookmark, display_func, status)
        else
            call bookmark_jump_via(bookmark, display_same_window, status)
        end if
    end subroutine bookmark_jump

    recursive subroutine cmd_bookmark_jump(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: bookmark

        call bookmark_completing_read("Jump to bookmark", bookmark, status, bookmark_current_bookmark())
        if (.not. status%ok()) return
        call bookmark_jump(bookmark, status, called_interactively=.true.)
    end subroutine cmd_bookmark_jump

    !> Jump to BOOKMARK in another window.  See `bookmark_jump' for more.
    !> Port of bookmark.el:bookmark-jump-other-window.
    recursive subroutine bookmark_jump_other_window(bookmark, status, called_interactively)
        character(len=*), intent(in) :: bookmark
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: called_interactively

        call bookmark_jump(bookmark, status, display_other_window, called_interactively)
    end subroutine bookmark_jump_other_window

    recursive subroutine cmd_bookmark_jump_other_window(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: bookmark

        call bookmark_completing_read("Jump to bookmark (in another window)", bookmark, status, &
                                      bookmark_current_bookmark())
        if (.not. status%ok()) return
        call bookmark_jump_other_window(bookmark, status, called_interactively=.true.)
    end subroutine cmd_bookmark_jump_other_window

    !> Call BOOKMARK_NAME's handler, `bookmark_default_handler'.  This
    !> changes current buffer and point, or a `file_error' error condition occurs.
    !>
    !> If BOOKMARK_NAME has a file, but that file no longer exists, then
    !> offer interactively to relocate BOOKMARK_NAME.  Port of
    !> bookmark.el:bookmark-handle-bookmark (no handlers).
    recursive subroutine bookmark_handle_bookmark(bookmark_name, status)
        character(len=*), intent(in) :: bookmark_name
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file, display_name
        type(status_type) :: err
        logical :: ok
        integer :: k

        k = bookmark_get_bookmark(bookmark_name, status)
        if (.not. status%ok()) return
        call bookmark_default_handler(bookmark_table(k), status)

        if (condition_matches(status%code, FM_BOOKMARK_ERROR_NO_FILENAME)) then
            ! We were unable to find the marked file, so ask if user wants
            ! to relocate the bookmark, else remind them to consider
            ! deletion.
            file = bookmark_get_filename(bookmark_name)

            ! Don't know how to relocate if there's no `file'.
            if (len(file) > 0) then
                err = status
                ! If file is not a dir, directory_file_name just returns
                ! file.
                display_name = directory_file_name(file)
                call ding()
                call read_y_or_n(display_name//" nonexistent.  Relocate """//bookmark_name//"""? ", ok, status)
                if (.not. status%ok()) return

                if (ok) then
                    call bookmark_relocate(bookmark_name, status)
                    if (.not. status%ok()) return
                    ! Try again.
                    k = bookmark_get_bookmark(bookmark_name, status)
                    if (.not. status%ok()) return
                    call bookmark_default_handler(bookmark_table(k), status)
                else
                    call message("Bookmark not relocated; consider removing it ("//bookmark_name//").")
                    status = err
                end if
            end if
        end if

        if (.not. status%ok()) return
        call set_bookmark_current_bookmark(bookmark_name)
    end subroutine bookmark_handle_bookmark

    !> Default handler to jump to a particular bookmark location.
    !> BMK_RECORD is a bookmark record, not a bookmark name (i.e., not a
    !> string).  Changes current buffer and point, or a
    !> `file_error' error condition occurs.  Port of bookmark.el:bookmark-default-handler (no
    !> `buffer' property).
    recursive subroutine bookmark_default_handler(bmk_record, status)
        type(bookmark_type), intent(in) :: bmk_record
        type(status_type), intent(out) :: status
        type(handle_type) :: buf
        type(status_type) :: st
        integer(int64) :: found

        if (allocated(bmk_record%filename)) then
            if (is_file_readable(expand(bmk_record%filename))) then
                call find_file_noselect(bmk_record%filename, buf, status)
                if (.not. status%ok()) return
                call set_buffer(buf, status)
                if (.not. status%ok()) return
            else
                call set_error(status, FM_BOOKMARK_ERROR_NO_FILENAME, "stringp, "//bmk_record%filename)
                return
            end if
        else
            ! No file found.
            call set_error(status, FM_BOOKMARK_ERROR_NO_FILENAME, "expected character(len=*), got unallocated")
            return
        end if

        if (bmk_record%position /= 0) call goto_char(bmk_record%position)

        ! Go searching forward first.  Then, if forward-str exists and was
        ! found in the file, we can search backward for behind-str.
        ! Rationale is that if text was inserted between the two in the
        ! file, it's better to be put before it so you can read it, rather
        ! than after and remain perhaps unaware of the changes.
        if (allocated(bmk_record%front_context_string)) then
            call search_forward(bmk_record%front_context_string, found, st, bound=point_max(), noerror=1)
            if (found /= 0) call goto_char(match_beginning(0, st))
        end if

        if (allocated(bmk_record%rear_context_string)) then
            call search_backward(bmk_record%rear_context_string, found, st, bound=point_min(), noerror=1)
            if (found /= 0) call goto_char(match_end(0, st))
        end if
    end subroutine bookmark_default_handler

    !> Relocate BOOKMARK_NAME to another file, reading file name with
    !> minibuffer.
    !>
    !> This makes an already existing bookmark point to that file, instead
    !> of the one it used to point at.  Useful when a file has been renamed
    !> after a bookmark was set in it.  Port of
    !> bookmark.el:bookmark-relocate.
    recursive subroutine bookmark_relocate(bookmark_name, status, called_interactively)
        character(len=*), intent(in) :: bookmark_name
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: called_interactively
        character(len=:), allocatable :: bmrk_filename, newloc, expanded

        call bookmark_maybe_historicize_string(bookmark_name, called_interactively)
        call bookmark_maybe_load_default_file(status)
        if (.not. status%ok()) return
        bmrk_filename = bookmark_get_filename(bookmark_name)

        if (len(bmrk_filename) == 0) then
            call set_error(status, FM_USER_ERROR, "Cannot relocate bookmark of type """"")
            return
        end if

        call read_file_name("Relocate "//bookmark_name//" to: ", newloc, status, dir=file_name_directory(bmrk_filename))
        if (.not. status%ok()) return
        call expand_file_name(newloc, expanded, status)
        if (.not. status%ok()) return
        newloc = abbreviate_file_name(expanded)
        call bookmark_set_filename(bookmark_name, newloc, status)
        if (.not. status%ok()) return
        call bookmark_update_last_modified(bookmark_name, status)
        bookmark_table_modification_count = bookmark_table_modification_count + 1
        if (is_bookmark_time_to_save()) call bookmark_save(status)
        call bookmark_bmenu_surreptitiously_rebuild_list()
    end subroutine bookmark_relocate

    recursive subroutine cmd_bookmark_relocate(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        call bookmark_completing_read("Bookmark to relocate", name, status)
        if (.not. status%ok()) return
        call bookmark_relocate(name, status, called_interactively=.true.)
    end subroutine cmd_bookmark_relocate

    !> Insert the name of the file associated with BOOKMARK_NAME.
    !>
    !> Optional second arg NO_HISTORY means don't record this in the
    !> minibuffer history list `bookmark_history'.  Port of
    !> bookmark.el:bookmark-insert-location.
    recursive subroutine bookmark_insert_location(bookmark_name, status, no_history, called_interactively)
        character(len=*), intent(in) :: bookmark_name
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_history, called_interactively
        logical :: history

        history = .true.
        if (present(no_history)) history = .not. no_history
        if (history) call bookmark_maybe_historicize_string(bookmark_name, called_interactively)
        call insert(bookmark_location(bookmark_name), status)
    end subroutine bookmark_insert_location

    recursive subroutine cmd_bookmark_insert_location(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        call bookmark_completing_read("Insert bookmark location", name, status)
        if (.not. status%ok()) return
        call bookmark_insert_location(name, status, called_interactively=.true.)
    end subroutine cmd_bookmark_insert_location

    !> Return a description of the location of BOOKMARK_NAME.  Port of
    !> bookmark.el:bookmark-location (no `location' property).
    recursive function bookmark_location(bookmark_name) result(location)
        character(len=*), intent(in) :: bookmark_name
        character(len=:), allocatable :: location
        type(status_type) :: st

        call bookmark_maybe_load_default_file(st)
        location = bookmark_get_filename(bookmark_name)
        if (len(location) == 0) location = "-- Unknown location --"
    end function bookmark_location

    !> Change the name of OLD_NAME bookmark to NEW_NAME name.
    !> If called from keyboard, prompt for OLD_NAME and NEW_NAME.
    !>
    !> If NEW_NAME is absent, prompt for it.  If called with two strings,
    !> then no prompting is done.
    !>
    !> While you are entering the new name, consecutive
    !> \<bookmark_minibuffer_read_name_map>\[bookmark_yank_word]'s insert
    !> consecutive words from the text of the buffer into the new bookmark
    !> name.  Port of bookmark.el:bookmark-rename.
    recursive subroutine bookmark_rename(old_name, status, new_name, called_interactively)
        character(len=*), intent(in) :: old_name
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: new_name
        logical, intent(in), optional :: called_interactively
        character(len=:), allocatable :: final_new_name

        call bookmark_maybe_historicize_string(old_name, called_interactively)
        call bookmark_maybe_load_default_file(status)
        if (.not. status%ok()) return
        bookmark_yank_point = point()
        bookmark_current_buffer = current_buffer()

        if (present(new_name)) then
            final_new_name = new_name
        else
            call read_from_minibuffer(format_message("Rename """//old_name//""" to: "), final_new_name, status, &
                                      keymap=bookmark_minibuffer_read_name_map, history="bookmark_history")
            if (.not. status%ok()) return
        end if

        call bookmark_set_name(old_name, final_new_name, status)
        if (.not. status%ok()) return
        call bookmark_update_last_modified(final_new_name, status)
        call set_bookmark_current_bookmark(final_new_name)
        call bookmark_bmenu_surreptitiously_rebuild_list()
        bookmark_table_modification_count = bookmark_table_modification_count + 1
        if (is_bookmark_time_to_save()) call bookmark_save(status)
    end subroutine bookmark_rename

    recursive subroutine cmd_bookmark_rename(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        call bookmark_completing_read("Old bookmark name", name, status)
        if (.not. status%ok()) return
        call bookmark_rename(name, status, called_interactively=.true.)
    end subroutine cmd_bookmark_rename

    !> Insert the text of the file pointed to by bookmark BOOKMARK_NAME.
    !> BOOKMARK_NAME is a bookmark name (a string), not a bookmark record.
    !>
    !> You may have a problem using this function if the value of variable
    !> `bookmark_table' is empty.  If that happens, you need to load in
    !> some bookmarks.  See help on function `bookmark_load' for more about
    !> this.  Port of bookmark.el:bookmark-insert.
    recursive subroutine bookmark_insert(bookmark_name, status, called_interactively)
        character(len=*), intent(in) :: bookmark_name
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: called_interactively
        type(handle_type) :: old
        character(len=:), allocatable :: str_to_insert
        type(status_type) :: st
        integer(int64) :: orig_point

        call bookmark_maybe_historicize_string(bookmark_name, called_interactively)
        call bookmark_maybe_load_default_file(status)
        if (.not. status%ok()) return
        orig_point = point()
        old = current_buffer()
        call bookmark_handle_bookmark(bookmark_name, status)
        if (status%ok()) str_to_insert = buffer_string()
        call set_buffer(old, st)
        if (.not. status%ok()) return
        call insert(str_to_insert, status)
        if (.not. status%ok()) return
        call push_mark()
        call goto_char(orig_point)
    end subroutine bookmark_insert

    recursive subroutine cmd_bookmark_insert(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        call bookmark_completing_read("Insert bookmark contents", name, status)
        if (.not. status%ok()) return
        call bookmark_insert(name, status, called_interactively=.true.)
    end subroutine cmd_bookmark_insert

    !> Delete BOOKMARK_NAME from the bookmark list.
    !>
    !> Removes only the first instance of a bookmark with that name.  If
    !> there are one or more other bookmarks with the same name, they will
    !> not be deleted.  Defaults to the "current" bookmark (that is, the one
    !> most recently used in this file, if any).
    !> Optional second arg BATCH means don't update the bookmark list
    !> buffer, probably because we were called from there.  Port of
    !> bookmark.el:bookmark-delete.
    recursive subroutine bookmark_delete(bookmark_name, status, batch, called_interactively)
        character(len=*), intent(in) :: bookmark_name
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: batch, called_interactively
        logical :: rebuild
        type(status_type) :: st
        integer :: k

        call bookmark_maybe_historicize_string(bookmark_name, called_interactively)
        call bookmark_maybe_load_default_file(status)
        if (.not. status%ok()) return
        k = bookmark_get_bookmark(bookmark_name, st, noerror=.true.)

        if (k > 0) then
            call bookmark_remove_fringe_mark(bookmark_table(k))
            call delete_record(k)
        end if

        ! Nil bookmark-current-bookmark if the last occurrence has been
        ! deleted.
        if (bookmark_get_bookmark(bookmark_current_bookmark(), st, noerror=.true.) == 0) then
            call set_bookmark_current_bookmark()
        end if

        rebuild = .true.
        if (present(batch)) rebuild = .not. batch
        if (rebuild) call bookmark_bmenu_surreptitiously_rebuild_list()
        bookmark_table_modification_count = bookmark_table_modification_count + 1
        if (is_bookmark_time_to_save()) call bookmark_save(status)
    end subroutine bookmark_delete

    recursive subroutine cmd_bookmark_delete(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        call bookmark_completing_read("Delete bookmark", name, status, bookmark_current_bookmark())
        if (.not. status%ok()) return
        call bookmark_delete(name, status, called_interactively=.true.)
    end subroutine cmd_bookmark_delete

    !> Permanently delete all bookmarks.
    !> If optional argument NO_CONFIRM is true, don't ask for
    !> confirmation.  Port of bookmark.el:bookmark-delete-all.
    recursive subroutine bookmark_delete_all(status, no_confirm)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_confirm
        logical :: ok
        integer :: k

        ! We don't use `bookmark_menu_confirm_deletion' here because that
        ! variable is specifically to control confirmation prompting in a
        ! bookmark menu buffer, where the user has the marked-for-deletion
        ! bookmarks arrayed in front of them and might have accidentally
        ! hit the key that executes the deletions.  The UI situation here
        ! is quite different, by contrast: the user got to this point by a
        ! sequence of keystrokes unlikely to be typed by chance.
        ok = .false.
        if (present(no_confirm)) ok = no_confirm
        status = status_type()

        if (.not. ok) then
            call read_yes_or_no("Permanently delete all bookmarks? ", ok, status)
            if (.not. status%ok()) return
        end if

        if (.not. ok) return
        call bookmark_maybe_load_default_file(status)
        if (.not. status%ok()) return

        do k = 1, size(bookmark_table)
            call bookmark_remove_fringe_mark(bookmark_table(k))
        end do

        bookmark_table_modification_count = bookmark_table_modification_count + size(bookmark_table)
        call clear_table()
        call bookmark_bmenu_surreptitiously_rebuild_list()
        if (is_bookmark_time_to_save()) call bookmark_save(status)
    end subroutine bookmark_delete_all

    recursive subroutine cmd_bookmark_delete_all(status)
        type(status_type), intent(out) :: status

        call bookmark_delete_all(status, no_confirm=allocated(current_prefix_arg%a))
    end subroutine cmd_bookmark_delete_all

    !> Return true if it is time to save bookmarks to disk, false
    !> otherwise.  Optional argument FINAL_TIME means this is being called
    !> when Fortmacs is being killed, so save even if
    !> `bookmark_save_flag' is a number and is greater than
    !> `bookmark_table_modification_count'.  Port of
    !> bookmark.el:bookmark-time-to-save-p.
    logical function is_bookmark_time_to_save(final_time) result(save)
        logical, intent(in), optional :: final_time
        logical :: final

        final = .false.
        if (present(final_time)) final = final_time

        if (final) then
            save = bookmark_table_modification_count > 0 .and. bookmark_save_flag /= BOOKMARK_SAVE_FLAG_NEVER
        else if (bookmark_save_flag >= 0) then
            save = bookmark_table_modification_count >= bookmark_save_flag
        else
            save = .false.
        end if
    end function is_bookmark_time_to_save

    !> Write bookmarks to a file (reading the file name with the
    !> minibuffer).  Port of bookmark.el:bookmark-write.
    recursive subroutine bookmark_write(status)
        type(status_type), intent(out) :: status

        call bookmark_maybe_load_default_file(status)
        if (.not. status%ok()) return
        call bookmark_save(status, parg=.true.)
    end subroutine bookmark_write

    !> Save currently defined bookmarks in FILE.
    !> FILE defaults to `bookmark_default_file'.
    !> With prefix PARG, query user for a file to save in.
    !> If MAKE_DEFAULT is true (interactively with prefix
    !> \[universal_argument] \[universal_argument]) the file we save in
    !> becomes the new default in the current Fortmacs session (without
    !> affecting the value of `bookmark_default_file'.).
    !>
    !> When you want to load in the bookmarks from a file, use
    !> `bookmark_load', \[bookmark_load].  That function will prompt you
    !> for a file, defaulting to the file defined by variable
    !> `bookmark_default_file'.  Port of bookmark.el:bookmark-save.
    recursive subroutine bookmark_save(status, parg, file, make_default)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: parg, make_default
        character(len=*), intent(in), optional :: file
        character(len=:), allocatable :: f, default
        logical :: ask, make

        call bookmark_maybe_load_default_file(status)
        if (.not. status%ok()) return
        ask = .false.
        if (present(parg)) ask = parg
        make = .false.
        if (present(make_default)) make = make_default

        if (present(file)) then
            f = file
        else
            if (allocated(bookmark_bookmarks_timestamp%file)) then
                default = bookmark_bookmarks_timestamp%file
            else
                default = bookmark_default_file
            end if

            if (ask) then
                ! This should be part of the `interactive' spec.
                call read_file_name("File to save bookmarks in: ("//default//") ", f, status, &
                                    dir=file_name_directory(default), default_filename=default)
                if (.not. status%ok()) return
            else
                f = default
            end if
        end if

        call bookmark_write_file(f, status)
        if (.not. status%ok()) return
        ! Signal that we have synced the bookmark file by setting this to 0.
        ! If there was an error at any point before, it will not get set,
        ! which is what we want.
        bookmark_table_modification_count = 0

        if (make) then
            call set_timestamp(expand(f))
        else if (allocated(bookmark_bookmarks_timestamp%file)) then
            if (same(bookmark_bookmarks_timestamp%file, expand(f))) call set_timestamp(expand(f))
        end if
    end subroutine bookmark_save

    recursive subroutine cmd_bookmark_save(status)
        type(status_type), intent(out) :: status
        logical :: c_u_c_u
        type(value_type) :: arg

        c_u_c_u = .false.
        arg = current_prefix_arg
        if (allocated(arg%a)) c_u_c_u = prefix_is_16(arg)
        call bookmark_save(status, parg=allocated(arg%a), make_default=c_u_c_u)
    end subroutine cmd_bookmark_save

    ! Whether prefix argument ARG is (16), as C-u C-u makes it.  Not a port.
    logical function prefix_is_16(arg)
        use lisp_fm, only: xfixnum, list_elements, value_type
        type(value_type), intent(in) :: arg
        type(value_type), allocatable :: elts(:)

        prefix_is_16 = .false.
        if (vtag(arg) /= V_ARRAY) return
        call list_elements(arg, elts)
        if (size(elts) /= 1) return
        if (vtag(elts(1)) /= V_INTEGER) return
        prefix_is_16 = xfixnum(elts(1)) == 16
    end function prefix_is_16

    ! `bookmark_bookmarks_timestamp' becomes FILE with its modification
    ! time.  Not a port.
    subroutine set_timestamp(file)
        character(len=*), intent(in) :: file
        type(file_stat_type) :: stat
        logical :: found

        call file_attributes(file, stat, found)
        bookmark_bookmarks_timestamp%file = file
        bookmark_bookmarks_timestamp%mtime_known = found
        bookmark_bookmarks_timestamp%mtime_sec = stat%mtime_sec
        bookmark_bookmarks_timestamp%mtime_nsec = stat%mtime_nsec
    end subroutine set_timestamp

    !> Write `bookmark_table' to FILE.  Port of
    !> bookmark.el:bookmark-write-file.
    recursive subroutine bookmark_write_file(file, status)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        type(progress_reporter_type) :: reporter
        type(handle_type) :: old, h
        type(status_type) :: st, wst
        character(len=:), allocatable :: coding, saved, base
        logical :: had_coding

        call make_progress_reporter(reporter, "Saving bookmarks to file "//file//"...")
        old = current_buffer()
        call get_buffer_create(" *Bookmarks*", h, status)
        if (.not. status%ok()) return
        call set_buffer(h, status)
        if (.not. status%ok()) return
        call goto_char(point_min())
        call erase_buffer(st)
        had_coding = allocated(coding_system_for_write%value)

        if (had_coding) then
            saved = coding_system_for_write%value
            coding = saved
        else if (allocated(bookmark_file_coding_system)) then
            coding = bookmark_file_coding_system
        else
            coding = "utf-8-emacs"
        end if

        ! (bookmark-insert-file-format-version-stamp)
        base = coding_system_base(coding)
        if (same(base, "undecided") .or. same(base, "prefer-utf-8")) base = "utf-8-emacs"
        call insert(bookmark_file_text(base), status)
        if (.not. status%ok()) return
        call goto_char(point_min())
        ! (The format makes no numbered backups: `bookmark_version_control'
        ! has no effect.)
        coding_system_for_write = coding
        call write_file(file, .false., wst)

        if (had_coding) then
            coding_system_for_write = saved
        else
            call coding_system_for_write%clear()
        end if

        if (.not. wst%ok()) then
            if (.not. condition_matches(wst%code, FM_FILE_ERROR)) then
                status = wst
                call set_buffer(old, st)
                return
            end if

            call message("Can't write "//file)
        end if

        bookmark_file_coding_system = coding
        call kill_buffer(current_buffer(), st)
        call set_buffer(old, st)
        call progress_reporter_done(reporter)
    end subroutine bookmark_write_file

    !> Add NEW_LIST of bookmarks to `bookmark_table'.
    !> Rename new bookmarks as needed using suffix "<N>" (N=1,2,3...), when
    !> they conflict with existing bookmark names.  Port of
    !> bookmark.el:bookmark-import-new-list.
    recursive subroutine bookmark_import_new_list(new_list)
        type(bookmark_type), intent(inout) :: new_list(:)
        type(minibuffer_string_type), allocatable :: names(:), grown(:)
        integer :: k, j, n

        names = bookmark_all_names()

        do k = 1, size(new_list)
            call bookmark_maybe_rename(new_list(k), names)
            call append_record(new_list(k))
            n = size(names)
            allocate (grown(n + 1))
            grown(1)%s = bookmark_name_from_full_record(new_list(k))

            do j = 1, n
                call move_alloc(names(j)%s, grown(j + 1)%s)
            end do

            call move_alloc(grown, names)
        end do
    end subroutine bookmark_import_new_list

    !> Rename bookmark FULL_RECORD if its current name is already used.
    !> This is a helper for `bookmark_import_new_list'.  Port of
    !> bookmark.el:bookmark-maybe-rename.
    subroutine bookmark_maybe_rename(full_record, names)
        type(bookmark_type), intent(inout) :: full_record
        type(minibuffer_string_type), intent(in) :: names(:)
        character(len=:), allocatable :: found_name, new_name
        character(len=24) :: buf
        integer :: count

        found_name = bookmark_name_from_full_record(full_record)
        if (.not. is_name_taken(found_name)) return
        ! We've got a conflict, so generate a new name
        count = 2
        new_name = found_name

        do while (is_name_taken(new_name))
            write (buf, "(i0)") count
            new_name = found_name//"<"//trim(buf)//">"
            count = count + 1
        end do

        full_record%name = new_name

    contains

        logical function is_name_taken(s)
            character(len=*), intent(in) :: s
            integer :: k

            is_name_taken = .false.

            do k = 1, size(names)
                if (same(names(k)%s, s)) then
                    is_name_taken = .true.
                    return
                end if
            end do
        end function is_name_taken
    end subroutine bookmark_maybe_rename

    !> Load bookmarks from FILE (which must be in bookmark format).
    !> Appends loaded bookmarks to the front of the list of bookmarks.
    !> If argument OVERWRITE is true, existing bookmarks are destroyed.
    !> Optional third arg NO_MSG means don't display any messages while
    !> loading.  If DEFAULT is true make FILE the new bookmark file to
    !> watch.  Interactively, a prefix arg makes OVERWRITE and DEFAULT
    !> true.
    !>
    !> If you load a file that doesn't contain a proper bookmark list, you
    !> will corrupt Fortmacs's bookmark list.  Generally, you should only
    !> load in files that were created with the bookmark functions in the
    !> first place.  Your own personal bookmark file, specified by the
    !> variable `bookmark_default_file', is maintained automatically by
    !> Fortmacs; you shouldn't need to load it explicitly.
    !>
    !> If you load a file containing bookmarks with the same names as
    !> bookmarks already present in your Fortmacs, the new bookmarks will
    !> get unique numeric suffixes "<2>", "<3>", etc.  Port of
    !> bookmark.el:bookmark-load.
    recursive subroutine bookmark_load(file, status, overwrite, no_msg, default)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: overwrite, no_msg, default
        character(len=:), allocatable :: f, afile, current_default
        type(progress_reporter_type) :: reporter
        type(bookmark_type), allocatable :: blist(:)
        type(handle_type) :: old, buf
        type(status_type) :: st
        logical :: over, quiet, make
        integer :: k

        over = .false.
        if (present(overwrite)) over = overwrite
        quiet = .false.
        if (present(no_msg)) quiet = no_msg
        make = .false.
        if (present(default)) make = default
        call expand_file_name(file, f, status)
        if (.not. status%ok()) return
        afile = abbreviate_file_name(f)

        if (.not. is_file_readable(f)) then
            call set_error(status, FM_USER_ERROR, "Cannot read bookmark file "//afile)
            return
        end if

        if (.not. quiet) call make_progress_reporter(reporter, "Loading bookmarks from "//f//"...")
        old = current_buffer()
        call find_file_noselect(f, buf, status)
        if (.not. status%ok()) return
        call set_buffer(buf, status)
        if (.not. status%ok()) return
        call goto_char(point_min())
        call bookmark_table_from_buffer(blist, status)

        if (status%ok()) then
            if (.not. allocated(bookmark_table)) allocate (bookmark_table(0))

            if (over) then
                call clear_table()

                do k = 1, size(blist)
                    call append_record(blist(k))
                end do

                bookmark_table_modification_count = 0
            else
                call bookmark_import_new_list(blist)
                bookmark_table_modification_count = bookmark_table_modification_count + 1
            end if

            if (allocated(bookmark_bookmarks_timestamp%file)) then
                current_default = bookmark_bookmarks_timestamp%file
            else
                current_default = expand(bookmark_default_file)
            end if

            if (make .or. same(f, current_default)) call set_timestamp(f)
            call bookmark_bmenu_surreptitiously_rebuild_list()

            if (allocated(buffer_file_coding_system%value)) then
                bookmark_file_coding_system = buffer_file_coding_system%value
            end if

            call kill_buffer(buf, st)
        end if

        ! (On an error the file's buffer stays, as upstream.)
        call set_buffer(old, st)
        if (.not. status%ok()) return
        call bookmark_after_load_file_hook%execute(status)
        if (.not. status%ok()) return
        if (.not. quiet) call progress_reporter_done(reporter)
    end subroutine bookmark_load

    recursive subroutine cmd_bookmark_load(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: default, file
        logical :: prefix

        if (allocated(bookmark_bookmarks_timestamp%file)) then
            default = abbreviate_file_name(bookmark_bookmarks_timestamp%file)
        else
            default = abbreviate_file_name(expand(bookmark_default_file))
        end if

        prefix = allocated(current_prefix_arg%a)
        call read_file_name("Load bookmarks from: ("//default//") ", file, status, dir=file_name_directory(default), &
                            default_filename=default, mustmatch=REQUIRE_MATCH_CONFIRM)
        if (.not. status%ok()) return
        call bookmark_load(file, status, overwrite=prefix, default=prefix)
    end subroutine cmd_bookmark_load

    !> Display the annotation for BOOKMARK_NAME in a buffer.
    !> If the annotation does not exist, do nothing.  Port of
    !> bookmark.el:bookmark-show-annotation.
    recursive subroutine bookmark_show_annotation(bookmark_name, status)
        character(len=*), intent(in) :: bookmark_name
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: annotation
        type(handle_type) :: old_buf, buf, saved
        integer(int64) :: saved_point
        type(status_type) :: st

        status = status_type()
        annotation = bookmark_get_annotation(bookmark_name)
        if (len(annotation) == 0) return
        saved = current_buffer()
        saved_point = point()
        old_buf = current_buffer()
        call get_buffer_create("*Bookmark Annotation*", buf, status)
        if (.not. status%ok()) return
        call pop_to_buffer(buf, status, table=display_action_table_type(inhibit_same_window=.true.))
        if (.not. status%ok()) return
        call set_buffer(buf, st)
        buffers(cur)%read_only = .false.
        call erase_buffer(st)
        call insert(annotation, st)
        call goto_char(point_min())
        call restore_buffer_modified(.false.)
        buffers(cur)%read_only = .true.
        call switch_to_buffer_other_window(old_buf, status)
        ! (save-excursion)
        call set_buffer(saved, st)
        call goto_char(saved_point)
    end subroutine bookmark_show_annotation

    !> Display the annotations for all bookmarks in a buffer.  Port of
    !> bookmark.el:bookmark-show-all-annotations.
    recursive subroutine bookmark_show_all_annotations(status)
        use window_fm, only: select_window, is_window_live
        type(status_type), intent(out) :: status
        type(bookmark_type), allocatable :: sorted(:)
        type(handle_type) :: buf, old_window, old_buffer
        type(status_type) :: st
        character(len=:), allocatable :: name, ann
        integer(int64) :: shortage
        integer :: k

        old_window = selected_window()
        old_buffer = current_buffer()
        call get_buffer_create("*Bookmark Annotation*", buf, status)
        if (.not. status%ok()) return
        call pop_to_buffer(buf, status, table=display_action_table_type(inhibit_same_window=.true.))
        if (.not. status%ok()) return
        call set_buffer(buf, st)
        buffers(cur)%read_only = .false.
        call erase_buffer(st)
        sorted = bookmark_maybe_sort_table()

        do k = 1, size(sorted)
            name = bookmark_name_from_full_record(sorted(k))
            ann = ""
            if (allocated(sorted(k)%annotation)) ann = sorted(k)%annotation
            call insert(name//":"//achar(10), st)

            if (len(ann) > 0) then
                ! insert the annotation, indented by 4 spaces.
                block
                    integer(int64) :: here
                    here = point()
                    call insert(ann, st)
                    if (.not. is_bol()) call insert(achar(10), st)
                    call goto_char(here)
                end block

                do while (point() < point_max())
                    ! paranoia
                    call beginning_of_line()
                    call insert("    ", st)
                    shortage = forward_line()
                    call end_of_line()
                end do
            end if
        end do

        call goto_char(point_min())
        call restore_buffer_modified(.false.)
        buffers(cur)%read_only = .true.
        ! (save-selected-window)
        if (is_window_live(old_window)) call select_window(old_window, st, norecord=.true.)
        call set_buffer(old_buffer, st)
    end subroutine bookmark_show_all_annotations

    ! ------------------------------------------------------------------
    ! The bookmark list
    ! ------------------------------------------------------------------

    !> Rebuild the Bookmark List if it exists.
    !> Don't affect the buffer ring order.  Port of
    !> bookmark.el:bookmark-bmenu-surreptitiously-rebuild-list.
    recursive subroutine bookmark_bmenu_surreptitiously_rebuild_list()
        type(window_configuration_type) :: config
        type(handle_type) :: old
        type(status_type) :: st
        integer(int64) :: pt

        if (.not. is_buffer_live(get_buffer(bookmark_bmenu_buffer))) return
        old = current_buffer()
        pt = point()
        call current_window_configuration(config)
        call bookmark_bmenu_list(st)
        call set_window_configuration(config)
        call set_buffer(old, st)
        call goto_char(pt)
    end subroutine bookmark_bmenu_surreptitiously_rebuild_list

    !> Re-populate `tabulated_list_entries'.  Port of
    !> bookmark.el:bookmark-bmenu--revert.
    recursive subroutine bookmark_bmenu_revert(status)
        type(status_type), intent(out) :: status
        type(bookmark_type), allocatable :: sorted(:)
        type(tabulated_list_entry_type), allocatable :: entries(:)
        integer :: k, n, ncols

        sorted = bookmark_maybe_sort_table()
        n = size(sorted)
        ncols = 3
        if (bookmark_bmenu_toggle_filenames) ncols = 4
        allocate (entries(n))

        ! (Pushed onto a list upstream: the last record first.)
        do k = 1, n
            associate (e => entries(n - k + 1), r => sorted(k))
                e%id = bookmark_name_from_full_record(r)
                allocate (e%cols(ncols))
                e%cols(1)%face = ""
                e%cols(1)%text = ""

                if (allocated(r%annotation)) then
                    if (len(r%annotation) > 0) e%cols(1)%text = "*"
                end if

                e%cols(2)%text = bookmark_name_from_full_record(r)
                e%cols(2)%face = ""
                ! (display-mouse-p: a window system.)
                if (associated(selected_frame%rif)) e%cols(2)%face = "bookmark_menu_bookmark"
                ! (No handlers: no types.)
                e%cols(3)%text = ""
                e%cols(3)%face = ""

                if (bookmark_bmenu_toggle_filenames) then
                    e%cols(4)%text = bookmark_location(e%id)
                    e%cols(4)%face = ""
                end if
            end associate
        end do

        ! The value of `bookmark_sort_flag' might have changed since the
        ! last time the buffer contents were generated, so re-check it.
        if (bookmark_sort_flag == BOOKMARK_SORT_FLAG_ALPHABETICAL) then
            call set_tabulated_list_sort_key("Bookmark Name")
            call set_tabulated_list_entries(entries)
        else
            call clear_tabulated_list_sort_key()
            ! And since we're not sorting by bookmark name, show bookmarks
            ! according to order of creation, with the most recently
            ! created bookmarks at the top and the least recently created
            ! at the bottom.
            call set_tabulated_list_entries(entries(n:1:-1))
        end if

        ! Generate the header only after `tabulated_list_sort_key' is
        ! settled, because if that's true then the sort-direction indicator
        ! will be shown in the named column, but if it's nil then the
        ! indicator will not be shown.
        call tabulated_list_init_header()
        call tabulated_list_print(status, remember_pos=.true.)
    end subroutine bookmark_bmenu_revert

    ! Not a port.
    recursive subroutine bmenu_revert_function(ignore_auto, noconfirm, status)
        logical, intent(in) :: ignore_auto, noconfirm
        type(status_type), intent(out) :: status

        if (ignore_auto .and. noconfirm) continue
        call bookmark_bmenu_revert(status)
    end subroutine bmenu_revert_function

    ! Not a port.
    recursive subroutine bmenu_revert_hook(status)
        type(status_type), intent(out) :: status

        call bookmark_bmenu_revert(status)
    end subroutine bmenu_revert_hook

    !> Return the Bookmark List, building it if it doesn't exists.
    !> Don't affect the buffer ring order.  Port of
    !> bookmark.el:bookmark-bmenu-get-buffer.
    recursive function bookmark_bmenu_get_buffer() result(buf)
        type(handle_type) :: buf
        type(window_configuration_type) :: config
        type(handle_type) :: old
        type(status_type) :: st
        integer(int64) :: pt

        buf = get_buffer(bookmark_bmenu_buffer)
        if (buf%index /= 0) return
        old = current_buffer()
        pt = point()
        call current_window_configuration(config)
        call bookmark_bmenu_list(st)
        buf = get_buffer(bookmark_bmenu_buffer)
        call set_window_configuration(config)
        call set_buffer(old, st)
        call goto_char(pt)
    end function bookmark_bmenu_get_buffer

    !> Display a list of existing bookmarks.
    !> The list is displayed in a buffer named `*Bookmark List*'.
    !> The leftmost column displays a D if the bookmark is flagged for
    !> deletion, or > if it is flagged for displaying.  Port of
    !> bookmark.el:bookmark-bmenu-list.
    recursive subroutine bookmark_bmenu_list(status, called_interactively)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: called_interactively
        type(handle_type) :: buf
        logical :: interactive

        call bookmark_maybe_load_default_file(status)
        if (.not. status%ok()) return
        call get_buffer_create(bookmark_bmenu_buffer, buf, status)
        if (.not. status%ok()) return
        interactive = .false.
        if (present(called_interactively)) interactive = called_interactively

        if (interactive) then
            call switch_to_buffer(buffers(buf%index)%name, status)
        else
            call set_buffer(buf, status)
        end if

        if (.not. status%ok()) return
        call bookmark_bmenu_mode(status)
        if (.not. status%ok()) return
        call bookmark_bmenu_revert(status)
    end subroutine bookmark_bmenu_list

    recursive subroutine cmd_bookmark_bmenu_list(status)
        type(status_type), intent(out) :: status

        call bookmark_bmenu_list(status, called_interactively=.true.)
    end subroutine cmd_bookmark_bmenu_list

    !> Major mode for editing a list of bookmarks.
    !> Each line describes one of the bookmarks in Fortmacs.
    !> Letters do not insert themselves; instead, they are commands.
    !> Bookmark names preceded by a "*" have annotations.
    !>
    !> If `bookmark_sort_flag' is true, then sort the list by bookmark name
    !> (case-insensitively, in collation order); the direction of that sort
    !> can be reversed by using the column sort toggle for the bookmark
    !> name column.
    !>
    !> If `bookmark_sort_flag' is false, then sort the list by bookmark
    !> creation order, with most recently created bookmarks on top.
    !> However, the column sort toggle will still activate (and thereafter
    !> toggle the direction of) lexical sorting by bookmark name.  At any
    !> time you may use \[revert_buffer] to go back to sorting by creation
    !> order.
    !>
    !> \<bookmark_bmenu_mode_map>
    !> \[bookmark_bmenu_mark] -- mark bookmark to be displayed.
    !> \[bookmark_bmenu_mark_all] -- mark all listed bookmarks to be displayed.
    !> \[bookmark_bmenu_select] -- select bookmark of line point is on.
    !>   Also show bookmarks marked using m in other windows.
    !> \[bookmark_bmenu_toggle_filenames] -- toggle displaying of filenames (they may obscure long bookmark names).
    !> \[bookmark_bmenu_locate] -- display (in minibuffer) location of this bookmark.
    !> \[bookmark_bmenu_1_window] -- select this bookmark in full-frame window.
    !> \[bookmark_bmenu_2_window] -- select this bookmark in one window,
    !>   together with bookmark selected before this one in another window.
    !> \[bookmark_bmenu_this_window] -- select this bookmark in place of the bookmark menu buffer.
    !> \[bookmark_bmenu_other_window] -- select this bookmark in another window,
    !>   so the bookmark menu bookmark remains visible in its window.
    !> \[bookmark_bmenu_switch_other_window] -- switch the other window to this bookmark.
    !> \[bookmark_bmenu_rename] -- rename this bookmark (prompts for new name).
    !> \[bookmark_bmenu_relocate] -- relocate this bookmark's file (prompts for new file).
    !> \[bookmark_bmenu_delete] -- mark this bookmark to be deleted, and move down.
    !> \[bookmark_bmenu_delete_backwards] -- mark this bookmark to be deleted, and move up.
    !> \[bookmark_bmenu_delete_all] -- mark all listed bookmarks as to be deleted.
    !> \[bookmark_bmenu_execute_deletions] -- delete bookmarks marked with `\[bookmark_bmenu_delete]'
    !>   or `\[bookmark_bmenu_delete_all]'.
    !> \[bookmark_bmenu_save] -- save the current bookmark list in the default file.
    !>   With a prefix arg, prompts for a file to save in.
    !> \[bookmark_bmenu_load] -- load in a file of bookmarks (prompts for file.)
    !> \[bookmark_bmenu_unmark] -- remove all kinds of marks from current line.
    !>   With prefix argument, also move up one line.
    !> \[bookmark_bmenu_backup_unmark] -- back up a line and remove marks.
    !> \[bookmark_bmenu_unmark_all] -- remove all kinds of marks from all listed bookmarks.
    !> \[bookmark_bmenu_show_annotation] -- show the annotation, if it exists, for the current bookmark
    !>   in another buffer.
    !> \[bookmark_bmenu_show_all_annotations] -- show the annotations of all bookmarks in another buffer.
    !> \[bookmark_bmenu_edit_annotation] -- edit the annotation for the current bookmark.
    !> \[bookmark_bmenu_search] -- incrementally search for bookmarks.
    !> \[revert_buffer] -- refresh the buffer, and thus refresh the sort order (useful
    !>   if `bookmark_sort_flag' is false).
    !>
    !> Port of bookmark.el:bookmark-bmenu-mode.
    recursive subroutine bookmark_bmenu_mode(status)
        type(status_type), intent(out) :: status
        type(tabulated_list_column_type), allocatable :: format(:)
        logical :: saved

        call major_modes%bookmark_bmenu_mode%begin(saved, status)
        if (.not. status%ok()) return
        ! (truncate-lines: not ported.)
        buffers(cur)%read_only = .true.

        if (bookmark_bmenu_toggle_filenames) then
            allocate (format(4))
        else
            allocate (format(3))
        end if

        ! Space to add "*" for bookmark with annotation
        format(1)%name = ""
        format(1)%width = 1
        format(2)%name = "Bookmark Name"
        format(2)%width = bookmark_bmenu_file_column
        format(2)%sort = .true.
        format(2)%predicate => bookmark_bmenu_name_predicate
        format(3)%name = "Type"
        format(3)%width = bookmark_bmenu_type_column_width
        format(3)%sort = .true.
        format(3)%predicate => bookmark_bmenu_type_predicate

        if (bookmark_bmenu_toggle_filenames) then
            format(4)%name = "File"
            format(4)%width = 0
            format(4)%sort = .true.
            format(4)%predicate => bookmark_bmenu_file_predicate
        end if

        call set_tabulated_list_format(format)
        call set_tabulated_list_padding(BOOKMARK_BMENU_MARKS_WIDTH)
        if (bookmark_sort_flag == BOOKMARK_SORT_FLAG_ALPHABETICAL) call set_tabulated_list_sort_key("Bookmark Name")
        call tabulated_list_revert_hook%add(bmenu_revert_hook, local=.true.)
        call revert_buffer_function%set_local(bmenu_revert_function)
        call tabulated_list_init_header()
        call major_modes%bookmark_bmenu_mode%finish(saved, status)
    end subroutine bookmark_bmenu_mode

    ! string-collate-lessp with IGNORE-CASE: case-insensitive, in the
    ! POSIX locale's order (by character code).  Not a port.
    logical function is_collate_less(a, b)
        use fns_fm, only: compare_strings
        character(len=*), intent(in) :: a, b

        is_collate_less = compare_strings(a, b, ignore_case=.true.) < 0
    end function is_collate_less

    !> Predicate to sort "*Bookmark List*" buffer by the name column.
    !> This is used for `tabulated_list_format' in `bookmark_bmenu_mode'.
    !> Port of bookmark.el:bookmark-bmenu--name-predicate.
    logical function bookmark_bmenu_name_predicate(a, b)
        type(tabulated_list_entry_type), intent(in) :: a, b

        bookmark_bmenu_name_predicate = is_collate_less(a%id, b%id)
    end function bookmark_bmenu_name_predicate

    !> Predicate to sort "*Bookmark List*" buffer by the type column.
    !> This is used for `tabulated_list_format' in `bookmark_bmenu_mode'.
    !> Port of bookmark.el:bookmark-bmenu--type-predicate.
    logical function bookmark_bmenu_type_predicate(a, b)
        type(tabulated_list_entry_type), intent(in) :: a, b

        bookmark_bmenu_type_predicate = is_collate_less(a%cols(3)%text, b%cols(3)%text)
    end function bookmark_bmenu_type_predicate

    !> Predicate to sort "*Bookmark List*" buffer by the file column.
    !> This is used for `tabulated_list_format' in `bookmark_bmenu_mode'.
    !> Port of bookmark.el:bookmark-bmenu--file-predicate.
    logical function bookmark_bmenu_file_predicate(a, b)
        type(tabulated_list_entry_type), intent(in) :: a, b

        bookmark_bmenu_file_predicate = is_collate_less(bookmark_location(a%id), bookmark_location(b%id))
    end function bookmark_bmenu_file_predicate

    !> Toggle whether filenames are shown in the bookmark list.
    !> Optional argument SHOW means show them unconditionally.  Port of
    !> bookmark.el:bookmark-bmenu-toggle-filenames.
    recursive subroutine bookmark_bmenu_toggle_filenames_command(status, show)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: show
        logical :: s

        status = status_type()
        s = .false.
        if (present(show)) s = show

        if (s) then
            bookmark_bmenu_toggle_filenames = .true.
        else
            bookmark_bmenu_toggle_filenames = .not. bookmark_bmenu_toggle_filenames
        end if

        call bookmark_bmenu_surreptitiously_rebuild_list()
    end subroutine bookmark_bmenu_toggle_filenames_command

    recursive subroutine cmd_bookmark_bmenu_toggle_filenames(status)
        type(status_type), intent(out) :: status

        call bookmark_bmenu_toggle_filenames_command(status)
    end subroutine cmd_bookmark_bmenu_toggle_filenames

    !> In an interactive bookmark list, show filenames along with
    !> bookmarks.  Port of bookmark.el:bookmark-bmenu-show-filenames.
    recursive subroutine bookmark_bmenu_show_filenames()
        bookmark_bmenu_toggle_filenames = .true.
        call bookmark_bmenu_surreptitiously_rebuild_list()
    end subroutine bookmark_bmenu_show_filenames

    !> In an interactive bookmark list, hide the filenames of the
    !> bookmarks.  Port of bookmark.el:bookmark-bmenu-hide-filenames.
    recursive subroutine bookmark_bmenu_hide_filenames()
        bookmark_bmenu_toggle_filenames = .false.
        call bookmark_bmenu_surreptitiously_rebuild_list()
    end subroutine bookmark_bmenu_hide_filenames

    !> If point is not on a bookmark line, move it to one.
    !> If after the last full line, move to the last full line.  The return
    !> value is undefined.  Port of
    !> bookmark.el:bookmark-bmenu-ensure-position.
    subroutine bookmark_bmenu_ensure_position()
        if (is_bol() .and. is_eob()) call beginning_of_line(0_int64)
    end subroutine bookmark_bmenu_ensure_position

    !> Return the bookmark for this line in an interactive bookmark list
    !> buffer ("" for none).  Port of bookmark.el:bookmark-bmenu-bookmark.
    function bookmark_bmenu_bookmark() result(name)
        character(len=:), allocatable :: name

        call bookmark_bmenu_ensure_position()
        name = tabulated_list_get_id()
    end function bookmark_bmenu_bookmark

    !> Mark bookmark on this line to be displayed by
    !> \<bookmark_bmenu_mode_map>\[bookmark_bmenu_select].  Port of
    !> bookmark.el:bookmark-bmenu-mark.
    recursive subroutine bookmark_bmenu_mark(status)
        type(status_type), intent(out) :: status

        call bookmark_bmenu_ensure_position()
        call tabulated_list_put_tag(">", status, .true.)
    end subroutine bookmark_bmenu_mark

    ! Put TAG on every listed bookmark, point staying.  Not a port.
    recursive subroutine tag_all(tag, status)
        character(len=*), intent(in) :: tag
        type(status_type), intent(out) :: status
        integer(int64) :: saved

        saved = point()
        call goto_char(point_min())
        call bookmark_bmenu_ensure_position()
        status = status_type()

        do while (.not. is_eob())
            call tabulated_list_put_tag(tag, status, .true.)
            if (.not. status%ok()) exit
        end do

        call goto_char(saved)
    end subroutine tag_all

    !> Mark all listed bookmarks to be displayed by
    !> \<bookmark_bmenu_mode_map>\[bookmark_bmenu_select].  Port of
    !> bookmark.el:bookmark-bmenu-mark-all.
    recursive subroutine bookmark_bmenu_mark_all(status)
        type(status_type), intent(out) :: status

        call tag_all(">", status)
    end subroutine bookmark_bmenu_mark_all

    !> Select this line's bookmark; also display bookmarks marked with
    !> `>'.  You can mark bookmarks with the
    !> \<bookmark_bmenu_mode_map>\[bookmark_bmenu_mark] or
    !> \<bookmark_bmenu_mode_map>\[bookmark_bmenu_mark_all] commands.
    !> Port of bookmark.el:bookmark-bmenu-select.
    recursive subroutine bookmark_bmenu_select(status)
        use search_fm, only: re_search_forward
        use window_fm, only: delete_other_windows, split_window, other_window, bury_buffer
        use frame_fm, only: frame_lines
        type(status_type), intent(out) :: status
        type(minibuffer_string_type), allocatable :: others(:)
        type(handle_type) :: menu, new
        character(len=:), allocatable :: bmrk, tem
        integer(int64) :: found
        integer :: tem_size, k
        logical :: ro, dup

        bmrk = bookmark_bmenu_bookmark()
        menu = current_buffer()
        allocate (others(0))
        call goto_char(point_min())

        do
            call re_search_forward("^>", found, status, noerror=1)
            if (.not. status%ok()) return
            if (found == 0) exit
            tem = bookmark_bmenu_bookmark()
            ro = buffers(cur)%read_only
            buffers(cur)%read_only = .false.
            call delete_char(-1_int64, status)
            if (status%ok()) call insert(" ", status)
            buffers(cur)%read_only = ro
            if (.not. status%ok()) return
            dup = same(tem, bmrk)

            do k = 1, size(others)
                if (same(others(k)%s, tem)) dup = .true.
            end do

            if (.not. dup) call append_name(others, tem)
        end do

        tem_size = (frame_lines(selected_frame) - 1)/(1 + size(others))
        call delete_other_windows(selected_window(), status)
        if (.not. status%ok()) return
        call bookmark_jump(bmrk, status)
        if (.not. status%ok()) return
        call bury_buffer(status, menu)

        if (size(others) > 0) then
            do k = 1, size(others)
                call split_window(selected_window(), tem_size, .true., .false., new, status)
                if (.not. status%ok()) return
                call other_window(1_int64, status)
                if (.not. status%ok()) return
                call bookmark_jump(others(k)%s, status)
                if (.not. status%ok()) return
            end do
        else
            call other_window(1_int64, status)
        end if
    end subroutine bookmark_bmenu_select

    ! Not a port.
    subroutine append_name(names, name)
        type(minibuffer_string_type), allocatable, intent(inout) :: names(:)
        character(len=*), intent(in) :: name
        type(minibuffer_string_type), allocatable :: grown(:)
        integer :: k, n

        n = size(names)
        allocate (grown(n + 1))

        do k = 1, n
            call move_alloc(names(k)%s, grown(k)%s)
        end do

        grown(n + 1)%s = name
        call move_alloc(grown, names)
    end subroutine append_name

    !> Return true if any bookmarks are marked in the marks column.
    !> Port of bookmark.el:bookmark-bmenu-any-marks.
    logical function bookmark_bmenu_any_marks() result(any)
        use search_fm, only: looking_at
        integer(int64) :: saved, shortage
        type(status_type) :: st

        saved = point()
        call goto_char(point_min())
        call bookmark_bmenu_ensure_position()
        any = .false.

        do while (.not. is_eob())
            call beginning_of_line()

            if (looking_at("^\S-", st)) then
                any = .true.
                exit
            end if

            shortage = forward_line(1_int64)
        end do

        call goto_char(saved)
    end function bookmark_bmenu_any_marks

    !> Save the current list into a bookmark file.
    !> With a prefix arg, prompt for a file to save them in.
    !>
    !> See also the related behaviors of `bookmark_load' and
    !> `bookmark_bmenu_load'.  Port of bookmark.el:bookmark-bmenu-save.
    recursive subroutine bookmark_bmenu_save(status)
        type(status_type), intent(out) :: status
        type(window_configuration_type) :: config
        type(handle_type) :: old
        type(status_type) :: st
        integer(int64) :: pt

        old = current_buffer()
        pt = point()
        call current_window_configuration(config)
        call cmd_bookmark_save(status)
        if (status%ok()) call restore_buffer_modified(.false.)
        call set_window_configuration(config)
        call set_buffer(old, st)
        call goto_char(pt)
    end subroutine bookmark_bmenu_save

    !> Load bookmarks from a file and rebuild the bookmark menu-buffer.
    !> Prompt for a file, with the default choice being the value of
    !> `bookmark_default_file'.
    !>
    !> With a prefix argument, replace the current ambient bookmarks (i.e.,
    !> the ones in `bookmark_table') with the ones from the selected file and
    !> make that file be the new value of `bookmark_default_file'.  In other
    !> words, a prefix argument means "switch over to the bookmark universe
    !> defined in the loaded file".  Without a prefix argument, just add the
    !> loaded bookmarks into the current ambient set.
    !>
    !> See the documentation for `bookmark_load' for more details; see also
    !> the related behaviors of `bookmark_save' and `bookmark_bmenu_save'.
    !> Port of bookmark.el:bookmark-bmenu-load.
    recursive subroutine bookmark_bmenu_load(status)
        type(status_type), intent(out) :: status
        type(window_configuration_type) :: config
        type(handle_type) :: old
        type(status_type) :: st
        integer(int64) :: pt

        call bookmark_bmenu_ensure_position()
        old = current_buffer()
        pt = point()
        call current_window_configuration(config)
        ! This will call `bookmark_bmenu_list'
        call cmd_bookmark_load(status)
        call set_window_configuration(config)
        call set_buffer(old, st)
        call goto_char(pt)
    end subroutine bookmark_bmenu_load

    !> Select this line's bookmark, alone, in full frame.  Port of
    !> bookmark.el:bookmark-bmenu-1-window.
    recursive subroutine bookmark_bmenu_1_window(status)
        use window_fm, only: delete_other_windows, bury_buffer, other_buffer
        type(status_type), intent(out) :: status

        call bookmark_jump(bookmark_bmenu_bookmark(), status)
        if (.not. status%ok()) return
        call bury_buffer(status, other_buffer(current_buffer()))
        if (.not. status%ok()) return
        call delete_other_windows(selected_window(), status)
    end subroutine bookmark_bmenu_1_window

    !> Select this line's bookmark, with previous buffer in second window.
    !> Port of bookmark.el:bookmark-bmenu-2-window.
    recursive subroutine bookmark_bmenu_2_window(status)
        use window_fm, only: delete_other_windows, bury_buffer, other_buffer
        type(status_type), intent(out) :: status
        type(handle_type) :: menu, other
        character(len=:), allocatable :: bmrk

        bmrk = bookmark_bmenu_bookmark()
        menu = current_buffer()
        call delete_other_windows(selected_window(), status)
        if (.not. status%ok()) return
        other = other_buffer(current_buffer())
        call switch_to_buffer(buffers(other%index)%name, status, force_same_window=.true.)
        if (.not. status%ok()) return
        ! (pop-up-windows t: pop_to_buffer splits.)
        call bookmark_jump_via(bmrk, display_pop_to_buffer, status)
        if (.not. status%ok()) return
        call bury_buffer(status, menu)
    end subroutine bookmark_bmenu_2_window

    ! Not a port.
    recursive subroutine display_pop_to_buffer(buffer, status)
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status

        call pop_to_buffer(buffer, status)
    end subroutine display_pop_to_buffer

    !> Select this line's bookmark in this window.  Port of
    !> bookmark.el:bookmark-bmenu-this-window.
    recursive subroutine bookmark_bmenu_this_window(status)
        type(status_type), intent(out) :: status

        call bookmark_jump(bookmark_bmenu_bookmark(), status)
    end subroutine bookmark_bmenu_this_window

    !> Select this line's bookmark in other window, leaving bookmark menu
    !> visible.  Port of bookmark.el:bookmark-bmenu-other-window.
    recursive subroutine bookmark_bmenu_other_window(status)
        type(status_type), intent(out) :: status

        call bookmark_jump_via(bookmark_bmenu_bookmark(), display_other_window, status)
    end subroutine bookmark_bmenu_other_window

    !> Make the other window select this line's bookmark.
    !> The current window remains selected.  Port of
    !> bookmark.el:bookmark-bmenu-switch-other-window.
    recursive subroutine bookmark_bmenu_switch_other_window(status)
        type(status_type), intent(out) :: status

        call bookmark_jump_via(bookmark_bmenu_bookmark(), display_not_this_window, status)
    end subroutine bookmark_bmenu_switch_other_window

    ! (display-buffer B t): not in the selected window.  Not a port.
    recursive subroutine display_not_this_window(buffer, status)
        use window_fm, only: display_buffer
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        type(handle_type) :: window

        call display_buffer(buffer, window, status, table=display_action_table_type(inhibit_same_window=.true.))
    end subroutine display_not_this_window

    !> Show the annotation for the current bookmark in another window.
    !> Port of bookmark.el:bookmark-bmenu-show-annotation.
    recursive subroutine bookmark_bmenu_show_annotation(status)
        type(status_type), intent(out) :: status

        call bookmark_show_annotation(bookmark_bmenu_bookmark(), status)
    end subroutine bookmark_bmenu_show_annotation

    !> Show the annotation for all bookmarks in another window.  Port of
    !> bookmark.el:bookmark-bmenu-show-all-annotations.
    recursive subroutine bookmark_bmenu_show_all_annotations(status)
        type(status_type), intent(out) :: status

        call bookmark_show_all_annotations(status)
    end subroutine bookmark_bmenu_show_all_annotations

    !> Edit the annotation for the current bookmark in another window.
    !> Port of bookmark.el:bookmark-bmenu-edit-annotation.
    recursive subroutine bookmark_bmenu_edit_annotation(status)
        type(status_type), intent(out) :: status

        call bookmark_edit_annotation(bookmark_bmenu_bookmark(), status, .true.)
    end subroutine bookmark_bmenu_edit_annotation

    !> Cancel all requested operations on bookmark on this line and move
    !> down.  Optional BACKUP means move up.  Port of
    !> bookmark.el:bookmark-bmenu-unmark.
    recursive subroutine bookmark_bmenu_unmark(status, backup)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: backup
        integer(int64) :: shortage
        logical :: up

        up = .false.
        if (present(backup)) up = backup
        ! any flags to reset according to circumstances?  How about a flag
        ! indicating whether this bookmark is being visited?  well, we don't
        ! have this now, so maybe later.
        call bookmark_bmenu_ensure_position()
        call tabulated_list_put_tag(" ", status)

        if (up) then
            shortage = forward_line(-1_int64)
        else
            shortage = forward_line(1_int64)
        end if
    end subroutine bookmark_bmenu_unmark

    recursive subroutine cmd_bookmark_bmenu_unmark(status)
        type(status_type), intent(out) :: status

        call bookmark_bmenu_unmark(status, allocated(current_prefix_arg%a))
    end subroutine cmd_bookmark_bmenu_unmark

    !> Move up and cancel all requested operations on bookmark on line
    !> above.  Port of bookmark.el:bookmark-bmenu-backup-unmark.
    recursive subroutine bookmark_bmenu_backup_unmark(status)
        type(status_type), intent(out) :: status
        integer(int64) :: shortage

        shortage = forward_line(-1_int64)
        call bookmark_bmenu_ensure_position()
        call bookmark_bmenu_unmark(status)
        shortage = forward_line(-1_int64)
        call bookmark_bmenu_ensure_position()
    end subroutine bookmark_bmenu_backup_unmark

    !> Cancel all requested operations on all listed bookmarks.  Port of
    !> bookmark.el:bookmark-bmenu-unmark-all.
    recursive subroutine bookmark_bmenu_unmark_all(status)
        type(status_type), intent(out) :: status

        call tag_all(" ", status)
    end subroutine bookmark_bmenu_unmark_all

    !> Mark bookmark on this line to be deleted.
    !> To carry out the deletions that you've marked, use
    !> \<bookmark_bmenu_mode_map>\[bookmark_bmenu_execute_deletions].
    !> Port of bookmark.el:bookmark-bmenu-delete.
    recursive subroutine bookmark_bmenu_delete(status)
        type(status_type), intent(out) :: status

        call bookmark_bmenu_ensure_position()
        call tabulated_list_put_tag("D", status, .true.)
    end subroutine bookmark_bmenu_delete

    !> Mark bookmark on this line to be deleted, then move up one line.
    !> To carry out the deletions that you've marked, use
    !> \<bookmark_bmenu_mode_map>\[bookmark_bmenu_execute_deletions].
    !> Port of bookmark.el:bookmark-bmenu-delete-backwards.
    recursive subroutine bookmark_bmenu_delete_backwards(status)
        type(status_type), intent(out) :: status
        integer(int64) :: shortage

        call bookmark_bmenu_delete(status)
        shortage = forward_line(-2_int64)
    end subroutine bookmark_bmenu_delete_backwards

    !> Mark all listed bookmarks as to be deleted.
    !> To remove all deletion marks, use
    !> \<bookmark_bmenu_mode_map>\[bookmark_bmenu_unmark_all].  To carry
    !> out the deletions that you've marked, use
    !> \<bookmark_bmenu_mode_map>\[bookmark_bmenu_execute_deletions].
    !> Port of bookmark.el:bookmark-bmenu-delete-all.
    recursive subroutine bookmark_bmenu_delete_all(status)
        type(status_type), intent(out) :: status

        call tag_all("D", status)
    end subroutine bookmark_bmenu_delete_all

    !> Delete bookmarks flagged `D'.
    !> If `bookmark_menu_confirm_deletion' is true, prompt for confirmation
    !> first.  Port of bookmark.el:bookmark-bmenu-execute-deletions.
    recursive subroutine bookmark_bmenu_execute_deletions(status)
        use search_fm, only: re_search_forward
        use indent_fm, only: current_column
        type(status_type), intent(out) :: status
        type(progress_reporter_type) :: reporter
        character(len=:), allocatable :: o_str
        integer(int64) :: o_point, o_col, found, here
        logical :: ok, have_str

        status = status_type()

        if (bookmark_menu_confirm_deletion) then
            call read_yes_or_no("Delete selected bookmarks? ", ok, status)
            if (.not. status%ok()) return

            if (.not. ok) then
                call message("Bookmarks not deleted.")
                return
            end if
        end if

        call make_progress_reporter(reporter, "Deleting bookmarks...")
        o_point = point()
        here = pos_bol()
        have_str = .false.

        if (here < point_max()) then
            if (char_at(here) /= iachar("D")) have_str = .true.
        else
            have_str = .true.
        end if

        if (have_str) call buffer_substring(here, pos_eol(), o_str, status)
        o_col = current_column()
        call goto_char(point_min())

        do
            call re_search_forward("^D", found, status, bound=point_max(), noerror=1)
            if (.not. status%ok()) return
            if (found == 0) exit
            ! pass BATCH arg
            call bookmark_delete(bookmark_bmenu_bookmark(), status, batch=.true.)
            if (.not. status%ok()) return
        end do

        call bookmark_bmenu_list(status)
        if (.not. status%ok()) return

        if (have_str) then
            call goto_char(point_min())
            call search_forward(o_str, found, status)
            if (.not. status%ok()) return
            call beginning_of_line()
            call forward_char(status, o_col)
        else
            call goto_char(o_point)
        end if

        call beginning_of_line()
        call progress_reporter_done(reporter)

    contains

        integer function char_at(pos)
            use editfns_fm, only: char_after
            integer(int64), intent(in) :: pos
            integer(int32) :: c
            logical :: found

            call char_after(c, found, pos)
            char_at = -1
            if (found) char_at = c
        end function char_at
    end subroutine bookmark_bmenu_execute_deletions

    !> Rename bookmark on current line.  Prompts for a new name.  Port of
    !> bookmark.el:bookmark-bmenu-rename.
    recursive subroutine bookmark_bmenu_rename(status)
        type(status_type), intent(out) :: status
        integer(int64) :: thispoint
        character(len=:), allocatable :: bmrk

        bmrk = bookmark_bmenu_bookmark()
        thispoint = point()
        call bookmark_rename(bmrk, status)
        call goto_char(thispoint)
    end subroutine bookmark_bmenu_rename

    !> Display the location of the bookmark for this line.  Port of
    !> bookmark.el:bookmark-bmenu-locate.
    recursive subroutine bookmark_bmenu_locate(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call message(bookmark_location(bookmark_bmenu_bookmark()))
    end subroutine bookmark_bmenu_locate

    !> Change the absolute file name of the bookmark on the current line.
    !> Prompt with completion for the new path.  Port of
    !> bookmark.el:bookmark-bmenu-relocate.
    recursive subroutine bookmark_bmenu_relocate(status)
        type(status_type), intent(out) :: status
        integer(int64) :: thispoint
        character(len=:), allocatable :: bmrk

        bmrk = bookmark_bmenu_bookmark()
        thispoint = point()
        call bookmark_relocate(bmrk, status)
        call goto_char(thispoint)
    end subroutine bookmark_bmenu_relocate

    !> Filter `bookmark_table' with bookmarks matching REGEXP and rebuild
    !> list.  Port of bookmark.el:bookmark-bmenu-filter-alist-by-regexp.
    recursive subroutine bookmark_bmenu_filter_table_by_regexp(regexp, status)
        use search_fm, only: string_match
        character(len=*), intent(in) :: regexp
        type(status_type), intent(out) :: status
        type(bookmark_type), allocatable :: saved(:)
        type(status_type) :: st
        integer :: k

        call move_alloc(bookmark_table, saved)
        allocate (bookmark_table(0))

        do k = 1, size(saved)
            if (string_match(regexp, bookmark_name_from_full_record(saved(k)), st) > 0) call append_record(saved(k))
        end do

        call bookmark_bmenu_list(status)
        call move_alloc(saved, bookmark_table)
    end subroutine bookmark_bmenu_filter_table_by_regexp

    !> Incremental search of bookmarks, hiding the non-matches as we go.
    !> Port of bookmark.el:bookmark-bmenu-search.
    recursive subroutine bookmark_bmenu_search(status)
        use minibuf_fm, only: read_string, minibuffer_setup_hook
        use timer_fm, only: is_timer, free_timer
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: bmk, pattern
        type(status_type) :: st

        bmk = bookmark_bmenu_bookmark()
        search_timer = handle_type()
        call minibuffer_setup_hook%add(bmenu_search_setup)
        call read_string("Pattern: ", pattern, status)
        call minibuffer_setup_hook%remove(bmenu_search_setup)

        if (status%ok()) then
            if (is_timer(search_timer)) call free_timer(search_timer)
            search_timer = handle_type()
        end if

        if (is_timer(search_timer)) then
            ! Signaled an error or a `quit'.
            call free_timer(search_timer)
            search_timer = handle_type()
            call bookmark_bmenu_list(st)
            call bookmark_bmenu_goto_bookmark(bmk)
        end if
    end subroutine bookmark_bmenu_search

    ! The setup function of `bookmark_bmenu_search': filter the list by
    ! the minibuffer's contents every `bookmark_search_delay' seconds of
    ! idleness.  Not a port.
    recursive subroutine bmenu_search_setup(status)
        use minibuf_fm, only: minibuffer_setup_hook
        use timer_fm, only: execute_with_idle_timer
        type(status_type), intent(out) :: status

        ! (minibuffer-with-setup-hook: once.)
        call minibuffer_setup_hook%remove(bmenu_search_setup)
        search_minibuffer = current_buffer()
        call execute_with_idle_timer(search_timer, real(bookmark_search_delay, real64), .true., bmenu_search_filter, status)
    end subroutine bmenu_search_setup

    ! Not a port.
    recursive subroutine bmenu_search_filter(status)
        use minibuf_fm, only: minibuffer_contents
        type(status_type), intent(out) :: status
        type(handle_type) :: old
        type(status_type) :: st

        old = current_buffer()
        call set_buffer(search_minibuffer, status)
        if (.not. status%ok()) return
        call bookmark_bmenu_filter_table_by_regexp(minibuffer_contents(), status)
        call set_buffer(old, st)
    end subroutine bmenu_search_filter

    !> Move point to bookmark with name NAME.  Port of
    !> bookmark.el:bookmark-bmenu-goto-bookmark.
    subroutine bookmark_bmenu_goto_bookmark(name)
        character(len=*), intent(in) :: name
        integer(int64) :: shortage

        call goto_char(point_min())

        do while (.not. (same(name, bookmark_bmenu_bookmark()) .or. is_eob()))
            shortage = forward_line(1_int64)
        end do

        shortage = forward_line(0_int64)
    end subroutine bookmark_bmenu_goto_bookmark

    ! ------------------------------------------------------------------
    ! Fringe mark
    ! ------------------------------------------------------------------

    ! Whether overlay OV is a bookmark's mark.  Not a port.
    logical function is_bookmark_overlay(ov)
        use data_fm, only: value_eq
        type(handle_type), intent(in) :: ov

        is_bookmark_overlay = value_eq(overlay_get(ov, "category"), value_type(name="bookmark"))
    end function is_bookmark_overlay

    !> Apply a colorized overlay to the bookmarked location.
    !> See user option `bookmark_fringe_mark'.  Port of
    !> bookmark.el:bookmark--set-fringe-mark.
    subroutine bookmark_set_fringe_mark()
        use textprop_fm, only: propertize
        type(handle_type) :: bm
        type(status_type) :: st
        type(value_type) :: spec(3), props(2)

        call make_overlay(bm, pos_bol(), pos_bol() + 1, st)
        if (.not. st%ok()) return
        call overlay_put(bm, "category", value_type(name="bookmark"), st)
        call overlay_put(bm, "evaporate", value_type(.true.), st)
        spec(1) = value_type(name="left_fringe")
        spec(2) = value_type(name="bookmark_fringe_mark")
        spec(3) = value_type(name="bookmark_face")
        props(1) = value_type(name="display")
        props(2) = make_list(spec)
        call overlay_put(bm, "before_string", propertize(value_type(text="x"), make_list(props)), st)
    end subroutine bookmark_set_fringe_mark

    !> Remove a bookmark's colorized overlay.
    !> BM is a bookmark as returned from function `bookmark_get_bookmark'.
    !> See user option `bookmark_fringe_mark'.  Port of
    !> bookmark.el:bookmark--remove-fringe-mark.
    recursive subroutine bookmark_remove_fringe_mark(bm)
        type(bookmark_type), intent(in) :: bm
        type(handle_type), allocatable :: list(:), overlays(:)
        type(handle_type) :: old
        type(status_type) :: st
        character(len=:), allocatable :: bkmk_fname
        integer(int64) :: saved_point, saved_begv, saved_zv
        integer :: b, k
        logical :: found

        if (bm%position == 0 .or. .not. allocated(bm%filename)) return
        found = .false.
        list = buffer_list()
        old = current_buffer()

        do b = 1, size(list)
            if (found) exit
            call set_buffer(list(b), st)
            if (.not. st%ok()) cycle
            call bookmark_buffer_file_name(bkmk_fname, st)
            if (.not. st%ok()) cycle
            ! Normalize both filenames before comparing, because the
            ! filename we receive from the bookmark wasn't necessarily
            ! generated by `bookmark_buffer_file_name'.
            if (.not. same(expand(bm%filename), expand(bkmk_fname))) cycle
            ! Suppose bookmark "foo" was earlier set at location X in a
            ! file, but now the file is narrowed such that X is outside the
            ! restriction.  Then the `goto_char' below would go to the
            ! wrong place and thus the wrong overlays would be fetched.
            ! This is why we temporarily `widen' before fetching.
            saved_point = point()
            saved_begv = buffers(cur)%begv
            saved_zv = buffers(cur)%zv
            call widen()
            call goto_char(bm%position)
            call overlays_in(pos_bol(), pos_bol() + 1, overlays)
            call restore_restriction(saved_begv, saved_zv)
            call goto_char(saved_point)

            do k = 1, size(overlays)
                if (is_bookmark_overlay(overlays(k))) then
                    call free_overlay(overlays(k))
                    found = .true.
                    exit
                end if
            end do
        end do

        call set_buffer(old, st)
    end subroutine bookmark_remove_fringe_mark

    ! Narrow back to BEGV..ZV (save-restriction).  Not a port.
    subroutine restore_restriction(begv, zv)
        use editfns_fm, only: narrow_to_region
        integer(int64), intent(in) :: begv, zv
        type(status_type) :: st

        call narrow_to_region(begv, zv, st)
    end subroutine restore_restriction

    ! ------------------------------------------------------------------
    ! Exit hook
    ! ------------------------------------------------------------------

    !> Save bookmark state, if necessary, at Fortmacs exit time.
    !> This also runs `bookmark_exit_hook'.  Port of
    !> bookmark.el:bookmark-exit-hook-internal.
    recursive subroutine bookmark_exit_hook_internal(status)
        type(status_type), intent(out) :: status

        call bookmark_exit_hook%execute(status)
        if (.not. status%ok()) return
        if (is_bookmark_time_to_save(.true.)) call bookmark_save(status)
    end subroutine bookmark_exit_hook_internal

    ! ------------------------------------------------------------------
    ! Commands without arguments
    ! ------------------------------------------------------------------

    recursive subroutine cmd_bookmark_write(status)
        type(status_type), intent(out) :: status

        call bookmark_write(status)
    end subroutine cmd_bookmark_write

    ! ------------------------------------------------------------------
    ! Definitions
    ! ------------------------------------------------------------------

    !> The faces of bookmark.el.  Not a port.
    subroutine define_faces()
        type(named_face_type), pointer :: f
        integer, parameter :: C = FACE_CLASS_COLOR, G = FACE_CLASS_GRAYSCALE, L = FACE_BACKGROUND_LIGHT, &
                              D = FACE_BACKGROUND_DARK

        f => bookmark_face
        call f%define("bookmark_face", "Face used to highlight current line.")
        call f%spec_entry(display_when(classes=[G], backgrounds=[L]), face_attrs(foreground="DimGray"))
        call f%spec_entry(display_when(classes=[G], backgrounds=[D]), face_attrs(foreground="LightGray"))
        call f%spec_entry(display_when(classes=[C], backgrounds=[L]), &
                          face_attrs(foreground="DarkOrange1", distant_foreground="DarkOrange3"))
        call f%spec_entry(display_when(classes=[C], backgrounds=[D]), &
                          face_attrs(foreground="DarkOrange1", distant_foreground="Orange1"))
    end subroutine define_faces

    !> Define the commands, keymaps, the mode and the options' defaults.
    !> Not a port.
    subroutine syms_of_bookmark()
        type(status_type) :: st

        if (.not. allocated(bookmark_default_file)) bookmark_default_file = state_directory()//"bookmarks"
        if (.not. allocated(bookmark_fringe_mark)) bookmark_fringe_mark = "bookmark_mark"
        if (.not. allocated(bookmark_table)) allocate (bookmark_table(0))
        if (.not. allocated(bookmark_inhibit_context_functions)) allocate (bookmark_inhibit_context_functions(0))
        if (.not. associated(bookmark_make_record_function)) bookmark_make_record_function => make_record_default

        if (.not. associated(bookmark_edit_annotation_text_func)) then
            bookmark_edit_annotation_text_func => default_annotation_text
        end if

        call forward_setting(bookmark_history)
        call add_local_variables_killer(kill_bookmark_locals)
        call define_faces()
        call major_modes%bookmark_edit_annotation_mode%define("bookmark_edit_annotation_mode", &
                                                              "Edit Bookmark Annotation", &
                                                              bookmark_edit_annotation_mode, &
                                                              bookmark_edit_annotation_mode_hook, &
                                                              parent=major_modes%text_mode)
        bookmark_edit_annotation_mode_map = major_modes%bookmark_edit_annotation_mode%map
        call keymap_set(bookmark_edit_annotation_mode_map, "C-c C-c", "bookmark_edit_annotation_confirm", st)
        call keymap_set(bookmark_edit_annotation_mode_map, "C-c C-k", "bookmark_edit_annotation_cancel", st)
        commands%bookmark_edit_annotation_mode%execute => bookmark_edit_annotation_mode
        commands%bookmark_edit_annotation_confirm%execute => bookmark_edit_annotation_confirm
        commands%bookmark_edit_annotation_cancel%execute => bookmark_edit_annotation_cancel
        commands%bookmark_set%execute => cmd_bookmark_set
        commands%bookmark_set_no_overwrite%execute => cmd_bookmark_set_no_overwrite
        commands%bookmark_jump%execute => cmd_bookmark_jump
        commands%bookmark_jump_other_window%execute => cmd_bookmark_jump_other_window
        commands%bookmark_relocate%execute => cmd_bookmark_relocate
        commands%bookmark_insert_location%execute => cmd_bookmark_insert_location
        commands%bookmark_locate%execute => cmd_bookmark_insert_location
        commands%bookmark_rename%execute => cmd_bookmark_rename
        commands%bookmark_insert%execute => cmd_bookmark_insert
        commands%bookmark_delete%execute => cmd_bookmark_delete
        commands%bookmark_delete_all%execute => cmd_bookmark_delete_all
        commands%bookmark_write%execute => cmd_bookmark_write
        commands%bookmark_save%execute => cmd_bookmark_save
        commands%bookmark_load%execute => cmd_bookmark_load
        commands%bookmark_yank_word%execute => bookmark_yank_word
        ! The keymaps are rebuilt.
        if (is_keymap_live(bookmark_minibuffer_read_name_map)) call free_keymap(bookmark_minibuffer_read_name_map, st)
        call make_sparse_keymap(bookmark_minibuffer_read_name_map)
        call set_keymap_parent(bookmark_minibuffer_read_name_map, minibuffer_local_map, st)
        call keymap_set(bookmark_minibuffer_read_name_map, "C-w", "bookmark_yank_word", st)
        if (is_keymap_live(bookmark_map)) call free_keymap(bookmark_map, st)
        call make_sparse_keymap(bookmark_map)
        call keymap_set(bookmark_map, "x", "bookmark_set", st)
        ! "m"ark
        call keymap_set(bookmark_map, "m", "bookmark_set", st)
        ! "M"aybe mark
        call keymap_set(bookmark_map, "M", "bookmark_set_no_overwrite", st)
        call keymap_set(bookmark_map, "j", "bookmark_jump", st)
        ! "g"o
        call keymap_set(bookmark_map, "g", "bookmark_jump", st)
        call keymap_set(bookmark_map, "o", "bookmark_jump_other_window", st)
        call keymap_set(bookmark_map, "i", "bookmark_insert", st)
        call keymap_set(bookmark_map, "e", "edit_bookmarks", st)
        ! "f"ind
        call keymap_set(bookmark_map, "f", "bookmark_insert_location", st)
        call keymap_set(bookmark_map, "r", "bookmark_rename", st)
        call keymap_set(bookmark_map, "d", "bookmark_delete", st)
        call keymap_set(bookmark_map, "D", "bookmark_delete_all", st)
        call keymap_set(bookmark_map, "l", "bookmark_load", st)
        call keymap_set(bookmark_map, "w", "bookmark_write", st)
        call keymap_set(bookmark_map, "s", "bookmark_save", st)
        if (.not. allocated(bookmark_bmenu_buffer)) bookmark_bmenu_buffer = "*Bookmark List*"
        call bookmark_menu_bookmark%define("bookmark_menu_bookmark", &
                                           "Face used to highlight bookmark names in bookmark menu buffers.")
        call bookmark_menu_bookmark%spec_entry(display_when(), face_attrs(weight="bold"))
        call major_modes%bookmark_bmenu_mode%define("bookmark_bmenu_mode", "Bookmark Menu", bookmark_bmenu_mode, &
                                                    bookmark_bmenu_mode_hook, parent=major_modes%tabulated_list_mode)
        bookmark_bmenu_mode_map = major_modes%bookmark_bmenu_mode%map
        commands%bookmark_bmenu_mode%execute => bookmark_bmenu_mode
        commands%bookmark_bmenu_list%execute => cmd_bookmark_bmenu_list
        commands%list_bookmarks%execute => cmd_bookmark_bmenu_list
        commands%edit_bookmarks%execute => cmd_bookmark_bmenu_list
        commands%bookmark_bmenu_select%execute => bookmark_bmenu_select
        commands%bookmark_bmenu_locate%execute => bookmark_bmenu_locate
        commands%bookmark_bmenu_2_window%execute => bookmark_bmenu_2_window
        commands%bookmark_bmenu_1_window%execute => bookmark_bmenu_1_window
        commands%bookmark_bmenu_this_window%execute => bookmark_bmenu_this_window
        commands%bookmark_bmenu_other_window%execute => bookmark_bmenu_other_window
        commands%bookmark_bmenu_switch_other_window%execute => bookmark_bmenu_switch_other_window
        commands%bookmark_bmenu_save%execute => bookmark_bmenu_save
        commands%bookmark_bmenu_delete%execute => bookmark_bmenu_delete
        commands%bookmark_bmenu_delete_backwards%execute => bookmark_bmenu_delete_backwards
        commands%bookmark_bmenu_execute_deletions%execute => bookmark_bmenu_execute_deletions
        commands%bookmark_bmenu_delete_all%execute => bookmark_bmenu_delete_all
        commands%bookmark_bmenu_backup_unmark%execute => bookmark_bmenu_backup_unmark
        commands%bookmark_bmenu_unmark%execute => cmd_bookmark_bmenu_unmark
        commands%bookmark_bmenu_unmark_all%execute => bookmark_bmenu_unmark_all
        commands%bookmark_bmenu_mark%execute => bookmark_bmenu_mark
        commands%bookmark_bmenu_mark_all%execute => bookmark_bmenu_mark_all
        commands%bookmark_bmenu_load%execute => bookmark_bmenu_load
        commands%bookmark_bmenu_rename%execute => bookmark_bmenu_rename
        commands%bookmark_bmenu_relocate%execute => bookmark_bmenu_relocate
        commands%bookmark_bmenu_toggle_filenames%execute => cmd_bookmark_bmenu_toggle_filenames
        commands%bookmark_bmenu_show_annotation%execute => bookmark_bmenu_show_annotation
        commands%bookmark_bmenu_show_all_annotations%execute => bookmark_bmenu_show_all_annotations
        commands%bookmark_bmenu_edit_annotation%execute => bookmark_bmenu_edit_annotation
        commands%bookmark_bmenu_search%execute => bookmark_bmenu_search
        call keymap_set(bookmark_bmenu_mode_map, "v", "bookmark_bmenu_select", st)
        call keymap_set(bookmark_bmenu_mode_map, "w", "bookmark_bmenu_locate", st)
        call keymap_set(bookmark_bmenu_mode_map, "2", "bookmark_bmenu_2_window", st)
        call keymap_set(bookmark_bmenu_mode_map, "1", "bookmark_bmenu_1_window", st)
        call keymap_set(bookmark_bmenu_mode_map, "j", "bookmark_bmenu_this_window", st)
        call keymap_set(bookmark_bmenu_mode_map, "C-c C-c", "bookmark_bmenu_this_window", st)
        call keymap_set(bookmark_bmenu_mode_map, "f", "bookmark_bmenu_this_window", st)
        call keymap_set(bookmark_bmenu_mode_map, "C-m", "bookmark_bmenu_this_window", st)
        call keymap_set(bookmark_bmenu_mode_map, "o", "bookmark_bmenu_other_window", st)
        call keymap_set(bookmark_bmenu_mode_map, "C-o", "bookmark_bmenu_switch_other_window", st)
        call keymap_set(bookmark_bmenu_mode_map, "s", "bookmark_bmenu_save", st)
        call keymap_set(bookmark_bmenu_mode_map, "C-x C-s", "bookmark_bmenu_save", st)
        call keymap_set(bookmark_bmenu_mode_map, "k", "bookmark_bmenu_delete", st)
        call keymap_set(bookmark_bmenu_mode_map, "C-d", "bookmark_bmenu_delete_backwards", st)
        call keymap_set(bookmark_bmenu_mode_map, "x", "bookmark_bmenu_execute_deletions", st)
        call keymap_set(bookmark_bmenu_mode_map, "d", "bookmark_bmenu_delete", st)
        call keymap_set(bookmark_bmenu_mode_map, "D", "bookmark_bmenu_delete_all", st)
        call keymap_set(bookmark_bmenu_mode_map, "S-SPC", "previous_line", st)
        call keymap_set(bookmark_bmenu_mode_map, "SPC", "next_line", st)
        call keymap_set(bookmark_bmenu_mode_map, "DEL", "bookmark_bmenu_backup_unmark", st)
        call keymap_set(bookmark_bmenu_mode_map, "u", "bookmark_bmenu_unmark", st)
        call keymap_set(bookmark_bmenu_mode_map, "U", "bookmark_bmenu_unmark_all", st)
        call keymap_set(bookmark_bmenu_mode_map, "m", "bookmark_bmenu_mark", st)
        call keymap_set(bookmark_bmenu_mode_map, "M", "bookmark_bmenu_mark_all", st)
        call keymap_set(bookmark_bmenu_mode_map, "l", "bookmark_bmenu_load", st)
        call keymap_set(bookmark_bmenu_mode_map, "r", "bookmark_bmenu_rename", st)
        call keymap_set(bookmark_bmenu_mode_map, "R", "bookmark_bmenu_relocate", st)
        call keymap_set(bookmark_bmenu_mode_map, "t", "bookmark_bmenu_toggle_filenames", st)
        call keymap_set(bookmark_bmenu_mode_map, "a", "bookmark_bmenu_show_annotation", st)
        call keymap_set(bookmark_bmenu_mode_map, "A", "bookmark_bmenu_show_all_annotations", st)
        call keymap_set(bookmark_bmenu_mode_map, "e", "bookmark_bmenu_edit_annotation", st)
        call keymap_set(bookmark_bmenu_mode_map, "J", "bookmark_jump", st)
        call keymap_set(bookmark_bmenu_mode_map, "/", "bookmark_bmenu_search", st)
        call kill_emacs_hook%add(bookmark_exit_hook_internal)
    end subroutine syms_of_bookmark
end module bookmark_fm
