! directory_editor_mode_mod.f90 -- Dired, the directory editor.
! Ported from GNU Emacs 31.1 lisp/dired.el.
!
! Copyright (C) 1985-1986, 1992-1997, 2000-2026 Free Software Foundation, Inc.
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

!> Dired: a buffer listing a directory as `ls -al' does, where files are
!> visited, marked, flagged and deleted.  `dired' (C-x d) and
!> `dired_other_window' (C-x 4 d) read the directory in the minibuffer;
!> `dired_noselect' makes the buffer; `dired_mode' is its major mode,
!> with `dired_mode_map', `dired_mode_hook', a read-only buffer and the
!> Dired faces through Font Lock keywords.  The listing comes from
!> `insert_directory' (files.el), which runs `insert_directory_program'
!> with `dired_listing_switches' and "--dired" so the file names are
!> known exactly.  Named `directory_editor_mode_mod' because dired.c is `directory_editor_mod'.
!> dired-aux.el (the operations on marked files) is in the submodule
!> directory_editor_auxiliary_mod; its options are declared here.
!>
!> Deferred: a list of files as the directory argument (an array as
!> `dired_directory'), inserted subdirectories (`i', `dired_insert_subdirectory',
!> `$' hiding, `dired_switches_by_directory', `dired_subdirectory_switches'),
!> `dired_hide_details_mode' and its options (the `invisible' properties
!> are set, but redisplay shows invisible text), `dired_find_alternate_file'
!> (`a') and `dired_close_when_opening_new_dired_buffer', the dired-aux.el
!> commands listed as deferred in directory_editor_auxiliary_mod (`P', `!', `&', `X', `E',
!> `=', `A', `Q', `.'), `dired_toggle_read_only' (wdired; C-x C-q runs
!> `read_only_mode'), `w' with a prefix of 1 (project.el),
!> `dired_auto_revert_buffer' as a function, `dired_movement_style' t,
!> mouse, drag-and-drop and click-to-select, `dired_create_directory_clickable'
!> (keymaps in text properties), the warning about file names with
!> newlines and `dired_auto_toggle_b_switch', `dired_maybe_use_globstar',
!> `dired_filename_display_length', `mode_line_buffer_identification'
!> (%17b), `page_delimiter' and `list_buffers_directory' as buffer-local
!> values, `buffer_stale_function' (auto-revert), directory-local
!> variables, the desktop, isearch on file names, dired-x
!> (`dired_find_subdirectory', `dired_omit_mode', closing buffers of deleted
!> files), `combine_change_calls' in `dired_readin' (the change hooks run
!> for each change), trash (`delete_by_moving_to_trash'),
!> `dired_trivial_filenames' t, B and L (loading Lisp:
!> not applicable).
module directory_editor_mode_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use execute_interactively_mod, only: prefix_argument_type, PREFIX_NONE, PREFIX_NUMBER, PREFIX_RAW
    use error_mod, only: status_type, set_error, format_error, format_message, condition_matches, error_name, &
                         GENERIC_ERROR_CODE, USER_ERROR_CODE, QUIT_CODE
    use handle_mod, only: handle_type, null_handle
    use value_mod, only: value_type, list, list_elements, xfixnum, xstring, command_procedure, vtag, V_INTEGER, &
                         choice_type, assignment(=)
    use names_mod, only: read_string_literal
    use data_mod, only: set, append_locals_clearer, value_eq, clear_all_local_variables, choice_setting_type, choice_state
    use commands_mod, only: commands
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, is_buffer_live, buffer_serial_live, resolve_buffer, &
                          get_buffer, get_or_make_buffer, generate_new_buffer_name, close_buffer, erase_buffer, &
                          is_buffer_modified, restore_buffer_modified, inhibit_read_only, get_file_buffer, &
                          buf_fetch_char, set_buffer_read_only
    use marker_mod, only: make_marker, destroy_marker, set_marker, marker_position, marker_buffer, character_to_byte, &
                          set_marker_insertion_type
    use insert_delete_mod, only: insert, inhibit_modification_hooks
    use editing_functions_mod, only: point, point_minimum, point_maximum, goto_character, character_after, character_before, &
                                     following_character, preceding_character, is_beginning_of_buffer, is_end_of_buffer, &
                                     is_beginning_of_line, is_end_of_line, buffer_substring, delete_region, &
                                     line_beginning_position, line_end_position, widen, substitute_character_in_region, &
                                     region_beginning, region_end, insert_character_n, narrow_to_region
    use editing_commands_mod, only: forward_line, beginning_of_line, end_of_line, forward_character, delete_character
    use search_mod, only: regular_expression_search_forward, regular_expression_search_backward, search_forward, looking_at, &
                          string_match, match_beginning, match_end, regular_expression_quote, replace_match, &
                          string_index_to_offset, case_fold_search, SEARCH_NOERROR, SEARCH_MOVE
    use text_properties_mod, only: get_text_property, put_text_property, add_text_properties, text_properties_at, &
                                   set_text_properties, next_single_property_change, remove_text_properties
    use syntax_mod, only: skip_characters_forward
    use indent_mod, only: current_column, move_to_column, indent_rigidly, indent_tabs_mode
    use file_input_output_mod, only: expand_file_name, file_name_directory, file_name_nondirectory, file_name_as_directory, &
                                     directory_file_name, is_file_name_absolute, file_exists, is_file_directory, &
                                     is_file_readable, is_file_symlink, file_attributes, file_status_type, insert_file_contents, &
                                     set_visited_file_modtime, visited_file_modtime
    use time_functions_mod, only: current_time_string, time_type, is_time_equal, TIMESPEC_COUNT_RATE
    use files_mod, only: insert_directory, insert_directory_clean, is_insert_directory_wildcard_in_directory, &
                         insert_directory_program, abbreviate_file_name, make_file_buffer, find_file, &
                         find_file_other_window, find_file_noselect, file_relative_name, file_truename, &
                         delete_file, delete_directory, file_name_extension, is_auto_save_file_name, &
                         is_backup_file_name, byte_count_to_string, get_free_disk_space, revert_buffer, &
                         revert_buffer_function, find_directory_functions, find_file_run_dired, &
                         temporary_file_directory, dired_ls_error_buffer, dired_ls_error_file, create_directory, &
                         directory_listing_before_filename_regular_expression
    use execute_process_mod, only: execute_process, shell_file_name, initialize_callproc
    use directory_editor_mod, only: directory_files, completion_ignored_extensions
    use tab_bar_mod, only: switch_to_buffer_other_tab
    use window_mod, only: windows, window_leaves, is_window_live, pop_to_buffer_same_window, &
                          switch_to_buffer_other_window, switch_to_buffer_other_frame, &
                          display_buffer, fit_window_to_buffer, select_window, &
                          window_point, set_window_point, quit_restore_window, display_action_parameters_type, &
                          temporary_buffer_window_show, recenter, display_buffer_mark_dedicated, DEDICATED_WEAK, &
                          DISPLAY_ACTION_BELOW_SELECTED, BK_CLOSE, same_window
    use frame_mod, only: selected_frame, selected_window
    use keymap_mod, only: make_keymap, is_keymap_live, set_keymap_parent, define_key, keymap_set
    use keyboard_mod, only: read_event, last_command
    use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value
    use display_engine_mod, only: message, cursor_in_echo_area
    use display_update_mod, only: ding
    use simple_mod, only: line_number_at_position, line_move, line_move_visual, goal_column, copy_to_clipboard_history, &
                          append_to_clipboard_entry, push_mark, &
                          is_region_active, undo, special_mode_map
    use utility_functions_mod, only: count_characters, read_yes_or_no
    use basic_subprograms_mod, only: match_string, make_progress_reporter, progress_reporter_update, progress_reporter_done, &
                                     progress_reporter_type
    use read_y_or_n_each_mod, only: read_answer, read_answer_type
    use minibuffer_reading_mod, only: read_string
    use minibuffer_mod, only: read_file_name, completion_insert_strings
    use replace_mod, only: read_regular_expression
    use help_mod, only: substitute_command_keys, register_keymap_name
    use print_mod, only: readable_text
    use hooks_mod, only: hook_type
    use derived_mod, only: is_derived_mode
    use modes_mod, only: major_modes
    use faces_mod, only: named_face_type, face_attributes, display_when, FACE_CLASS_COLOR
    use font_core_mod, only: font_lock_keyword_type, font_lock_highlight_type, font_lock_defaults, &
                             FONT_LOCK_OVERRIDE_NONE
    use font_lock_mod, only: font_lock_keyword, font_lock_highlight, font_lock_anchored
    use string_type_mod, only: string_type, append_string, prepend_string
    implicit none(type, external)
    private

    public :: initialize_dired_mode, dired, dired_other_window, dired_other_frame, dired_other_tab, dired_noselect, dired_mode
    public :: dired_get_filename, dired_move_to_filename, dired_move_to_end_of_filename, dired_current_directory, &
              dired_get_marked_files, dired_goto_file, dired_revert, dired_build_subdirectories, dired_get_subdirectory, &
              dired_buffers_for_directory, dired_file_name_at_point, dired_insert_directory, dired_readin, &
              dired_find_buffer_nocreate, dired_save_positions, dired_restore_positions, dired_positions_type, &
              dired_between_files, dired_next_line, dired_previous_line, dired_next_directory_line, dired_previous_directory_line, &
              dired_up_directory, dired_find_file, dired_unmark, dired_flag_file_deletion, &
              dired_toggle_marks, dired_mark_files_regular_expression, dired_mark_files_containing_regular_expression, &
              dired_do_flagged_delete, dired_do_delete, dired_delete_file, dired_sort_other, dired_sort_toggle, &
              dired_unmark_all_files, dired_change_marks, dired_subdirectory_directories, dired_directory_of, &
              dired_actual_switches_of, dired_check_switches, is_dired_switches_escape, is_dired_switches_recursive, &
              dired_glob_regular_expression, dired_make_relative, dired_remove_entry, dired_delete_entry, &
              dired_number_of_marked_files, dired_marker_prompt
    public :: dired_create_directory, dired_add_file, dired_add_entry, dired_do_redisplay, dired_update_file_line, &
              dired_goto_subdirectory, dired_remove_subdirectory, dired_mark_subdirectory_files, dired_after_subdirectory_garbage, &
              dired_previous_subdirectory
    ! Used by the directory_editor_auxiliary_mod submodule (gfortran drops private parent
    ! procedures that submodules call).
    public :: dired_goto_next_nontrivial_file, dired_map_over_marks, dired_mark_files_in_region, dired_next_subdirectory, &
              dired_subdirectory_maximum, dired_mark_1, dired_log, dired_log_summary, is_dired_in_this_tree, &
              dired_normalize_subdirectory, &
              dired_get_file_for_visit, dired_advertise, temporary_buffer_window_setup, insert_marked_files, fit_window
    public :: dired_do_chmod, dired_do_chgrp, dired_do_chown, dired_do_touch, dired_remove_line, dired_do_remove_lines, &
              dired_compress_file, dired_do_compress, dired_do_compress_to, dired_remove_file, dired_relist_file, &
              dired_query, dired_copy_file, dired_copy_file_recursive, dired_rename_file, dired_hardlink, &
              dired_make_relative_symlink, dired_create_files, dired_do_create_files, dired_do_copy, dired_do_symlink, &
              dired_do_relsymlink, dired_do_hardlink, dired_do_rename, dired_do_rename_regular_expression, &
              dired_do_copy_regular_expression, &
              dired_do_hardlink_regular_expression, dired_do_symlink_regular_expression, dired_do_relsymlink_regular_expression, &
              dired_upcase, &
              dired_downcase, dired_create_empty_file, dired_view_file, dired_show_file_type, dired_file_creator_procedure, &
              dired_name_constructor_procedure

    ! ------------------------------------------------------------------
    ! Options
    ! ------------------------------------------------------------------

    !> Switches passed to `ls' for Dired.  MUST contain the `l' option.
    !> May contain all other options that don't contradict `-l';
    !> may contain even `F', `b', `i' and `s'.  See also the variable
    !> `dired_ls_f_marks_symlinks' concerning the `F' switch.
    !>
    !> If you have files with names with embedded newline characters, adding
    !> `b' to the switches will allow Dired to handle those files better.
    !>
    !> Options that include embedded whitespace must be quoted
    !> like this: "--option=value with spaces".  Port of dired.el
    !> dired-listing-switches.
    character(len=:), allocatable, public :: dired_listing_switches

    !> Values of `dired_use_ls_dired' besides .true. and .false.
    integer, parameter, public :: DIRED_USE_LS_DIRED_UNSPECIFIED = 2

    !> .true. means Dired should pass the "--dired" option to "ls".
    !> If .false., don't pass "--dired" to "ls".
    !> The special value DIRED_USE_LS_DIRED_UNSPECIFIED means to check whether
    !> `insert_directory_program' supports the "--dired" option, and save
    !> the result in this variable.
    !> This is performed the first time `dired_insert_directory'
    !> invokes `insert_directory_program'.
    !>
    !> Note that if you set this option to .false., either through choice or
    !> because your "ls" program does not support "--dired", Dired
    !> will fail to parse some "unusual" file names, e.g. those with leading
    !> spaces.  You might want to install ls from GNU Coreutils, which does
    !> support this option.  Port of dired.el dired-use-ls-dired.
    type(choice_setting_type), public :: dired_use_ls_dired = &
                                         choice_setting_type(name="dired_use_ls_dired", choice=DIRED_USE_LS_DIRED_UNSPECIFIED)

    !> Informs Dired about how `ls -lF' marks symbolic links.
    !> Set this to true if `ls' (or whatever program is specified by
    !> `insert_directory_program') with `-lF' marks the symbolic link
    !> itself with a trailing @ (usually the case under Ultrix and macOS).
    !>
    !> Example: if `ln -s foo bar; ls -F bar' gives `bar -> foo', set it to
    !> false (the default), if it gives `bar@ -> foo', set it to true.
    !>
    !> Dired checks if there is really a @ appended.  Thus, if you have a
    !> marking `ls' program on one host and a non-marking on another host, and
    !> don't care about symbolic links which really end in a @, you can
    !> always set this variable to true.  Port of dired.el
    !> dired-ls-F-marks-symlinks.
    logical, public :: dired_ls_f_marks_symlinks = .false.

    !> Regexp of files to skip when finding first file of a directory.
    !> An unallocated value means move to the subdir line.  Port of
    !> dired.el dired-trivial-filenames.
    character(len=:), allocatable, public :: dired_trivial_filenames

    !> Values of `dired_free_space' besides .false.
    integer, parameter, public :: DIRED_FREE_SPACE_SEPARATE = 2, DIRED_FREE_SPACE_FIRST = 3

    !> Whether and how to display the disk space usage info in Dired buffers.
    !> If .false., don't display.
    !> If DIRED_FREE_SPACE_SEPARATE, display on a separate line, and include both the used
    !> and the free disk space.
    !> If DIRED_FREE_SPACE_FIRST, the default, display only the free disk space on the first
    !> line, following the directory name.  Port of dired.el
    !> dired-free-space (with DIRED_FREE_SPACE_FIRST the space is a
    !> `display' property, which redisplay does not show yet).
    type(choice_setting_type), public :: dired_free_space = &
                                     choice_setting_type(name="dired_free_space", true_valid=.false., choice=DIRED_FREE_SPACE_FIRST)

    !> Values of `dired_mark_region' besides .false.
    integer, parameter, public :: DIRED_MARK_REGION_FILE = 2, DIRED_MARK_REGION_LINE = 3

    !> Defines what commands that mark files do with the active region.
    !>
    !> When .false., marking commands don't operate on all files in the
    !> active region.  They process their prefix arguments as usual.
    !>
    !> When the value of this option is another one, then all Dired commands
    !> that mark or unmark files will operate on all files in the region
    !> if the region is active in Transient Mark mode.
    !>
    !> When DIRED_MARK_REGION_FILE, the region marking is based on the file name.
    !> This means don't mark the file if the end of the region is
    !> before the file name displayed on the Dired line, so the file name
    !> is visually outside the region.  This behavior is consistent with
    !> marking files without the region using the key `m' that works on
    !> the file name where the cursor is.
    !>
    !> When DIRED_MARK_REGION_LINE, the region marking is based on Dired lines,
    !> so include the file into marking if the end of the region
    !> is anywhere on its Dired line, except the beginning of the line.
    !> Port of dired.el dired-mark-region.
    type(choice_setting_type), public :: dired_mark_region = &
                                    choice_setting_type(name="dired_mark_region", true_valid=.false., choice=DIRED_MARK_REGION_FILE)

    !> Values of `dired_movement_style' besides .false. and .true.
    integer, parameter, public :: DIRED_MOVEMENT_STYLE_CYCLE = 2, DIRED_MOVEMENT_STYLE_BOUNDED = 3, &
                                  DIRED_MOVEMENT_STYLE_CYCLE_FILES = 4, DIRED_MOVEMENT_STYLE_BOUNDED_FILES = 5

    !> True means point skips empty
    !> lines when moving in Dired buffers.
    !> This affects only `dired_next_line', `dired_previous_line',
    !> `dired_next_directory_line', `dired_previous_directory_line'.
    !>
    !> Possible values other than .false. (.true. is DIRED_MOVEMENT_STYLE_BOUNDED):
    !>  * DIRED_MOVEMENT_STYLE_CYCLE:  when moving from the last/first visible line, cycle back
    !>              to the first/last visible line.
    !>  * DIRED_MOVEMENT_STYLE_BOUNDED: don't move up/down if the current line is the
    !>               first/last visible line.
    !>  * DIRED_MOVEMENT_STYLE_CYCLE_FILES: like DIRED_MOVEMENT_STYLE_CYCLE
    !>    but moves only between file lines.
    !>  * DIRED_MOVEMENT_STYLE_BOUNDED_FILES: like DIRED_MOVEMENT_STYLE_BOUNDED
    !>    but moves only between file lines.
    !> Port of dired.el dired-movement-style.
    type(choice_setting_type), public :: dired_movement_style = choice_setting_type(name="dired_movement_style")

    !> Automatically revert Dired buffers on revisiting their directory.
    !> If true, revisiting an existing Dired buffer always reverts it.
    !> If false, revisiting a Dired buffer doesn't revert it, but displays
    !> a message telling the user how to revert it if the directory has
    !> changed on disk.  Port of dired.el dired-auto-revert-buffer (a
    !> test function as the value is deferred).
    logical, public :: dired_auto_revert_buffer = .false.

    !> Whether symlinks are checked for validity.
    !> Set it to false for remote directories, which suffer from a slow
    !> connection.  Port of dired.el dired-check-symlinks.
    logical, public :: dired_check_symlinks = .true.

    !> Values of `dired_recursive_deletes' and `dired_recursive_copies'
    !> besides .true. and .false.
    integer, parameter, public :: DIRED_RECURSIVE_TOP = 2, DIRED_RECURSIVE_ASK = 3

    !> Whether Dired deletes directories recursively.
    !> If .false., Dired will not delete non-empty directories.
    !> .true. means to delete non-empty directories recursively,
    !> without asking.  This is dangerous!
    !> DIRED_RECURSIVE_TOP means to ask for each top-level directory specified by the
    !> Dired deletion command, and delete its subdirectories without
    !> asking.
    !> DIRED_RECURSIVE_ASK means to ask for each directory.  Port of
    !> dired.el dired-recursive-deletes.
    type(choice_setting_type), public :: dired_recursive_deletes = &
                                         choice_setting_type(name="dired_recursive_deletes", choice=DIRED_RECURSIVE_TOP)

    !> Dired commands for which Dired should not popup list of affected
    !> files: `all', or the names of the commands in `commands'.
    type, public :: dired_no_confirm_type
        logical :: all = .false.
        character(len=16), allocatable :: commands(:)
    end type dired_no_confirm_type

    !> Dired commands for which Dired should not popup list of affected files.
    !>
    !> If set, Dired will not pop up the list of files to be affected by
    !> some Dired commands, when asking for confirmation.  (Dired will still
    !> ask for confirmation, just without showing the affected files.)
    !>
    !> If `all' is true, the list of affected files is never popped up.
    !> `commands' can also hold command names: then the list of the
    !> affected files will not be popped up only for the corresponding Dired
    !> commands.  Recognized command names are `byte_compile', `chgrp',
    !> `chmod', `chown', `compress', `copy', `delete', `hardlink', `load',
    !> `move', `print', `shell', `symlink', `touch' and `uncompress'.  Port of
    !> dired.el dired-no-confirm.
    type(dired_no_confirm_type), public :: dired_no_confirm

    !> Values of `dired_switches_in_mode_line' besides a number of characters
    !> (`%characters') and a procedure (`%procedure').
    integer, parameter, public :: DIRED_SWITCHES_IN_MODE_LINE_SORT_ORDER = 2, DIRED_SWITCHES_IN_MODE_LINE_AS_IS = 3

    abstract interface
        !> A `dired_switches_in_mode_line%procedure': TEXT, the text to show
        !> after "Dired " for the listing switches SWITCHES.  Not a port.
        subroutine dired_switches_in_mode_line_procedure(switches, text)
            implicit none(type, external)
            character(len=*), intent(in) :: switches
            character(len=:), allocatable, intent(out) :: text
        end subroutine dired_switches_in_mode_line_procedure
    end interface

    !> The setting `dired_switches_in_mode_line': one of its named constants,
    !> a number of characters (R6-39: `%characters = 10'; negative when no
    !> number is set) or a procedure (R6-37: `%procedure => mine').
    !> Not a port.
    type, extends(choice_setting_type), public :: dired_switches_in_mode_line_choice_setting_type
        integer :: characters = -1
        procedure(dired_switches_in_mode_line_procedure), pointer, nopass :: procedure => null()
    contains
        procedure :: clear_kinds => dired_switches_in_mode_line_clear_kinds
        procedure :: other_value => dired_switches_in_mode_line_other_value
        procedure :: kind_hint => dired_switches_in_mode_line_kind_hint
    end type dired_switches_in_mode_line_choice_setting_type

    !> How to indicate `dired_actual_switches' in mode-line.
    !> Possible values:
    !>  * DIRED_SWITCHES_IN_MODE_LINE_SORT_ORDER: Indicate name-or-date sort order, if
    !>    possible.  Else show full switches.
    !>  * DIRED_SWITCHES_IN_MODE_LINE_AS_IS:  Show full switches.
    !>  * `%characters = N':  Show only the first N chars of full switches.
    !>  * `%procedure => f':  Pass `dired_actual_switches' as arg and show result.
    !> Port of dired.el dired-switches-in-mode-line.
    type(dired_switches_in_mode_line_choice_setting_type), public :: dired_switches_in_mode_line = &
      dired_switches_in_mode_line_choice_setting_type(name="dired_switches_in_mode_line", true_valid=.false., false_valid=.false., &
                                                                                      choice=DIRED_SWITCHES_IN_MODE_LINE_SORT_ORDER)

    !> True means the Dired sort command is disabled.
    !> The idea is to set this buffer-locally in special Dired buffers.
    !> Port of dired.el dired-sort-inhibit (one global value).
    logical, public :: dired_sort_inhibit = .false.

    !> Regular expression to match "garbage" files for
    !> `dired_flag_garbage_files'.  Port of dired.el
    !> dired-garbage-files-regexp.
    character(len=:), allocatable, public :: dired_garbage_files_regular_expression

    !> String of `ls' switches (single letters) except "t" that influence
    !> sorting.
    !>
    !> This indicates to Dired which option switches to watch out for because they
    !> will change the sorting order behavior of `ls'.
    !>
    !> To change the default sorting order (e.g. add a `-v' option), see the
    !> variable `dired_listing_switches'.  To temporarily override the listing
    !> format, use `\[universal_argument] \[dired]'.  Port of dired.el
    !> dired-ls-sorting-switches.
    character(len=*), parameter, public :: dired_ls_sorting_switches = "SXU"

    !> True means that Dired, when looking for files containing a
    !> regexp, reads them from the file system even if a buffer visits
    !> them.  Port of dired.el dired-always-read-filesystem.
    logical, public :: dired_always_read_filesystem = .false.

    !> In Dired, the current mark character.
    !> This is what the do-commands look for, and what the mark-commands
    !> store.  Port of dired.el dired-marker-char.
    character, public :: dired_marker_character = "*"

    !> Character used to flag files for deletion.  Port of dired.el
    !> dired-del-marker.
    character, public :: dired_delete_marker = "D"

    !> The buffer where Dired logs its errors.  Port of dired.el
    !> dired-log-buffer.
    character(len=*), parameter, public :: dired_log_buffer = "*Dired log*"

    !> Name of chown command (usually `chown').  Port of dired.el
    !> dired-chown-program.
    character(len=:), allocatable, public :: dired_chown_program

    !> Name of touch command (usually `touch').  Port of dired.el
    !> dired-touch-program.
    character(len=:), allocatable, public :: dired_touch_program

    !> The value of the `dired_keep_marker_*' options that keeps the
    !> marks (upstream t).
    character, parameter, public :: DIRED_KEEP_MARKER = achar(0)

    !> Controls marking of renamed files.
    !> If DIRED_KEEP_MARKER, files keep their previous marks when they are renamed.
    !> If a character, renamed files (whether previously marked or not)
    !> are afterward marked with that character.
    !> This option affects only files renamed by `dired_do_rename' and
    !> `dired_do_rename_regular_expression'.  Port of dired.el dired-keep-marker-rename.
    character, public :: dired_keep_marker_rename = DIRED_KEEP_MARKER

    !> Controls marking of copied files.
    !> If DIRED_KEEP_MARKER, copied files are marked if and as the corresponding original files were.
    !> If a character, copied files are unconditionally marked with that character.
    !> Port of dired.el dired-keep-marker-copy.
    character, public :: dired_keep_marker_copy = "C"

    !> Controls marking of newly made hard links.
    !> If DIRED_KEEP_MARKER, they are marked if and as the files linked to were marked.
    !> If a character, new links are unconditionally marked with that character.
    !> Port of dired.el dired-keep-marker-hardlink.
    character, public :: dired_keep_marker_hardlink = "H"

    !> Controls marking of newly made symbolic links.
    !> If DIRED_KEEP_MARKER, they are marked if and as the files linked to were marked.
    !> If a character, new links are unconditionally marked with that character.
    !> Port of dired.el dired-keep-marker-symlink.
    character, public :: dired_keep_marker_symlink = "Y"

    !> Controls marking of newly made relative symbolic links.
    !> If DIRED_KEEP_MARKER, they are marked if and as the files linked to were marked.
    !> If a character, new links are unconditionally marked with that character.
    !> Port of dired.el dired-keep-marker-relsymlink.
    character, public :: dired_keep_marker_relsymlink = "S"

    !> If true, Dired preserves the last-modified time in a file copy.
    !> (This works on only some systems.)  Port of dired.el
    !> dired-copy-preserve-time.
    logical, public :: dired_copy_preserve_time = .true.

    !> If true, Dired dereferences symlinks when copying them.
    !> This is similar to the "-L" option for the "cp" shell command.
    !> Port of dired.el dired-copy-dereference.
    logical, public :: dired_copy_dereference = .false.

    !> Whether Dired copies directories recursively.
    !> If .false., never copy recursively.
    !> .true. means to copy recursively without asking.
    !> DIRED_RECURSIVE_TOP means to ask for each directory at top level.
    !> DIRED_RECURSIVE_ASK means to ask for each directory.  Port of
    !> dired.el dired-recursive-copies.
    type(choice_setting_type), public :: dired_recursive_copies = &
                                         choice_setting_type(name="dired_recursive_copies", choice=DIRED_RECURSIVE_TOP)

    abstract interface
        !> A `dired_do_what_i_mean_target%procedure': the directories to offer as
        !> targets, the default first.  Not a port.
        subroutine dired_do_what_i_mean_target_procedure(directories)
            import :: string_type
            implicit none(type, external)
            type(string_type), allocatable, intent(out) :: directories(:)
        end subroutine dired_do_what_i_mean_target_procedure
    end interface

    !> The setting `dired_do_what_i_mean_target': a choice setting whose value can
    !> also be a procedure (R6-37), set with `=>'.  Not a port.
    type, extends(choice_setting_type), public :: dired_do_what_i_mean_target_choice_setting_type
        procedure(dired_do_what_i_mean_target_procedure), pointer, nopass :: procedure => null()
    contains
        procedure :: clear_kinds => dired_do_what_i_mean_target_clear_kinds
        procedure :: other_value => dired_do_what_i_mean_target_other_value
        procedure :: kind_hint => dired_do_what_i_mean_target_kind_hint
    end type dired_do_what_i_mean_target_choice_setting_type

    !> Values of `dired_do_what_i_mean_target' besides .true. and .false.: the
    !> windows to prefer (upstream's named functions).
    integer, parameter, public :: DIRED_DO_WHAT_I_MEAN_TARGET_NEXT = 2, DIRED_DO_WHAT_I_MEAN_TARGET_NEXT_VISIBLE = 3, &
                                  DIRED_DO_WHAT_I_MEAN_TARGET_RECENT = 4

    !> If true, Dired tries to guess a default target directory.
    !> This means: if there is a Dired buffer displayed in some window,
    !> use its current directory, instead of this Dired buffer's
    !> current directory.
    !>
    !> You can set it to prefer either the next window with a Dired buffer
    !> (DIRED_DO_WHAT_I_MEAN_TARGET_NEXT, or DIRED_DO_WHAT_I_MEAN_TARGET_NEXT_VISIBLE on
    !> visible frames), or the most recently used window with a Dired
    !> buffer (DIRED_DO_WHAT_I_MEAN_TARGET_RECENT).
    !>
    !> The target is used in the prompt for file copy, rename etc.
    !> A procedure of your own set with `=>' (dired_do_what_i_mean_target%procedure)
    !> gives the directories to offer, the default first.  Port of dired.el
    !> dired-dwim-target.
    type(dired_do_what_i_mean_target_choice_setting_type), public :: dired_do_what_i_mean_target = &
                                                                     dired_do_what_i_mean_target_choice_setting_type( &
                                                                     name="dired_do_what_i_mean_target")

    !> Values of `dired_create_destination_directories' besides .true. and .false.
    integer, parameter, public :: DIRED_CREATE_DESTINATION_DIRECTORIES_ASK = 2

    !> Whether Dired should create destination dirs when copying/moving files.
    !> If .false., don't create non-existent destination directories.
    !> If DIRED_CREATE_DESTINATION_DIRECTORIES_ASK, ask the user whether to create them.
    !> If .true., create them without asking.
    !>
    !> Also see `dired_create_destination_directories_on_trailing_dirsep'.
    !> Port of dired-aux.el dired-create-destination-dirs.
    type(choice_setting_type), public :: dired_create_destination_directories = &
                                         choice_setting_type(name="dired_create_destination_directories")

    !> If true, consider a file name ending in a slash as a directory to create.
    !>
    !> If this variable is true and a single destination file name of
    !> a Dired command ends in a directory separator (/), that file name
    !> will be treated as a non-existent directory, and that directory
    !> will be created if the variable `dired_create_destination_directories'
    !> says so.
    !>
    !> For example, if both `dired_create_destination_directories' and this
    !> option are set, renaming a directory named `old_name' to
    !> `new_name/' (note the trailing directory separator) will
    !> create `new_name' if it does not already exist, and will
    !> move `old_name' into it.  By contrast, if only `new_name'
    !> (without the trailing /) is given, or this option or
    !> `dired_create_destination_directories' is not set, `old_name' will be
    !> renamed to `new_name'.  Port of dired-aux.el
    !> dired-create-destination-dirs-on-trailing-dirsep.
    logical, public :: dired_create_destination_directories_on_trailing_dirsep = .false.

    !> Automatically revert Dired buffers after `dired_do' operations.
    !> This option controls whether to refresh the directory listing in a
    !> Dired buffer that is the destination of one of these operations:
    !> `dired_do_copy', `dired_do_rename', `dired_do_symlink', `dired_do_hardlink'.
    !> If the value is true, always revert the Dired buffer updated in the result
    !> of these operations.  Port of dired-aux.el dired-do-revert-buffer
    !> (a function as the value is deferred).
    logical, public :: dired_do_revert_buffer = .false.

    !> Whether `dired_create_empty_file' acts on the current directory.
    !> If true, `dired_create_empty_file' creates a new empty file and adds
    !> an entry for it (or its topmost new parent directory if created) under
    !> the current subdirectory in the Dired buffer by default (otherwise, it
    !> adds the new file (and new subdirectories if provided) to whichever
    !> directory the user enters at the prompt).  If false,
    !> `dired_create_empty_file' acts on the default directory by default.
    !> Port of dired-aux.el dired-create-empty-file-in-current-directory.
    logical, public :: dired_create_empty_file_in_current_directory = .false.

    !> Default suffix for compressing a single file.
    !> If empty, ".gz" will be used.  Port of dired-aux.el
    !> dired-compress-file-default-suffix.
    character(len=:), allocatable, public :: dired_compress_file_default_suffix

    !> Default suffix for compressing a directory.
    !> If empty, ".tar.gz" will be used.  Port of dired-aux.el
    !> dired-compress-directory-default-suffix.
    character(len=:), allocatable, public :: dired_compress_directory_default_suffix

    !> A rule of `dired_compress_file_suffixes': (REGULAR_EXPRESSION NEW-SUFFIX
    !> PROGRAM), PROGRAM empty for none.
    type, public :: dired_compress_suffix_type
        character(len=:), allocatable :: regular_expression, new_suffix, program
    end type dired_compress_suffix_type

    !> A rule, with `regular_expression' and `command', of `dired_compress_file_commands' and
    !> `dired_compress_files_commands'.
    type, public :: dired_compress_rule_type
        character(len=:), allocatable :: regular_expression, command
    end type dired_compress_rule_type

    !> Control changes in file name suffixes for compression and uncompression.
    !> Each element specifies one transformation rule, and has the form:
    !>   (REGULAR_EXPRESSION NEW-SUFFIX PROGRAM)
    !> The rule applies when the old file name matches REGULAR_EXPRESSION.
    !> The new file name is computed by deleting the part that matches REGULAR_EXPRESSION
    !>  (as well as anything after that), then adding NEW-SUFFIX in its place.
    !> If PROGRAM is not empty, the rule is an uncompression rule,
    !> and uncompression is done by running PROGRAM.
    !>
    !> Within PROGRAM, %i denotes the input file, and %o denotes the
    !> output file.
    !>
    !> Otherwise, the rule is a compression rule, and compression is done with gzip.
    !> ARGUMENTS are command switches passed to PROGRAM.  Port of dired-aux.el
    !> dired-compress-file-suffixes.
    type(dired_compress_suffix_type), allocatable, public :: dired_compress_file_suffixes(:)

    !> Controls the compression shell command for `dired_do_compress_to'.
    !>
    !> Each element has REGULAR_EXPRESSION and COMMAND, where REGULAR_EXPRESSION is the name of the
    !> archive to which you want to compress, and COMMAND is the
    !> corresponding command.
    !>
    !> Within COMMAND, %i denotes the input file(s), and %o denotes the
    !> output file.  %i path(s) are relative, while %o is absolute.
    !> Port of dired-aux.el dired-compress-file-alist.
    type(dired_compress_rule_type), allocatable, public :: dired_compress_file_commands(:)

    !> Controls the compression shell command for `dired_do_compress_to'.
    !>
    !> Each element has REGULAR_EXPRESSION and COMMAND, where REGULAR_EXPRESSION is the name of the
    !> archive to which you want to compress, and COMMAND is the
    !> corresponding command.
    !>
    !> Within COMMAND, %i denotes the input file(s), and %o denotes the
    !> output file.  %i path(s) are relative, while %o is absolute.
    !> Port of dired-aux.el dired-compress-files-alist.
    type(dired_compress_rule_type), allocatable, public :: dired_compress_files_commands(:)

    !> What a file creator of `dired_create_files' is, for its special
    !> cases: `dired_copy_file', `create_symbolic_link' or another.
    integer, parameter, public :: DIRED_CREATOR_OTHER = 0, DIRED_CREATOR_COPY = 1, DIRED_CREATOR_SYMLINK = 2

    abstract interface
        !> A FILE-CREATOR of `dired_create_files': create TO from FROM,
        !> overwriting TO if OK_FLAG.  Not a port.
        recursive subroutine dired_file_creator_procedure(from, to, ok_flag, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: from, to
            logical, intent(in) :: ok_flag
            type(status_type), intent(out) :: status
        end subroutine dired_file_creator_procedure

        !> A NAME-CONSTRUCTOR of `dired_create_files': the new file name TO
        !> for FROM, or SKIP .true. to skip it.  Not a port.
        recursive subroutine dired_name_constructor_procedure(from, to, skip, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: from
            character(len=:), allocatable, intent(out) :: to
            logical, intent(out) :: skip
            type(status_type), intent(out) :: status
        end subroutine dired_name_constructor_procedure
    end interface

    abstract interface
        !> A `dired_deletion_confirmer': ask PROMPT, ANSWER true for yes.
        !> Not a port.
        recursive subroutine dired_confirmer_procedure(prompt, answer, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: prompt
            logical, intent(out) :: answer
            type(status_type), intent(out) :: status
        end subroutine dired_confirmer_procedure
    end interface

    !> How Dired asks whether to delete the files: `read_yes_or_no' (or
    !> read_y_or_n).  Port of dired.el dired-deletion-confirmer.
    procedure(dired_confirmer_procedure), pointer, public :: dired_deletion_confirmer => null()

    ! ------------------------------------------------------------------
    ! Hooks
    ! ------------------------------------------------------------------

    !> Run at the very end of `dired_mode'.  Port of dired.el
    !> dired-mode-hook.
    type(hook_type), target, public :: dired_mode_hook = hook_type(name="dired_mode_hook")

    !> Event Hook run before reading in the directory.  Port of dired.el
    !> dired-before-readin-hook.
    type(hook_type), target, public :: dired_before_readin_hook = hook_type(name="dired_before_readin_hook")

    !> Hook run after each time a file or directory is read by Dired.
    !> After each listing of a file or directory, this hook is run
    !> with the buffer narrowed to the listing.  Port of dired.el
    !> dired-after-readin-hook.
    type(hook_type), target, public :: dired_after_readin_hook = hook_type(name="dired_after_readin_hook")

    !> This hook is used to position the point.
    !> It is run by the function `dired_initial_position'.  Port of
    !> dired.el dired-initial-position-hook.
    type(hook_type), target, public :: dired_initial_position_hook = hook_type(name="dired_initial_position_hook")

    !> Local keymap for Dired mode buffers.  Port of dired.el
    !> dired-mode-map.
    type(handle_type), public :: dired_mode_map

    ! ------------------------------------------------------------------
    ! Faces
    ! ------------------------------------------------------------------

    !> Face used for directory headers.  Port of dired.el dired-header.
    type(named_face_type), target, public :: dired_header
    !> Face used for Dired marks.  Port of dired.el dired-mark.
    type(named_face_type), target, public :: dired_mark
    !> Face used for marked files.  Port of dired.el dired-marked.
    type(named_face_type), target, public :: dired_marked
    !> Face used for files flagged for deletion.  Port of dired.el
    !> dired-flagged.
    type(named_face_type), target, public :: dired_flagged
    !> Face used to highlight a part of a buffer that needs user attention.
    !> Port of dired.el dired-warning.
    type(named_face_type), target, public :: dired_warning
    !> Face used to highlight permissions of group- and world-writable files.
    !> Port of dired.el dired-perm-write.
    type(named_face_type), target, public :: dired_permission_write
    !> Face used to highlight permissions of suid and guid files.  Port of
    !> dired.el dired-set-id.
    type(named_face_type), target, public :: dired_set_id
    !> Face used for subdirectories.  Port of dired.el dired-directory.
    type(named_face_type), target, public :: dired_directory
    !> Face used for symbolic links.  Port of dired.el dired-symlink.
    type(named_face_type), target, public :: dired_symlink
    !> Face used for broken symbolic links.  Port of dired.el
    !> dired-broken-symlink.
    type(named_face_type), target, public :: dired_broken_symlink
    !> Face used for sockets, pipes, block devices and char devices.
    !> Port of dired.el dired-special.
    type(named_face_type), target, public :: dired_special
    !> Face used for files suffixed with `completion_ignored_extensions'.
    !> Port of dired.el dired-ignored.
    type(named_face_type), target, public :: dired_ignored

    !> Face name used for directory headers.  Port of dired.el
    !> dired-header-face.
    character(len=:), allocatable, public :: dired_header_face
    !> Face name used for Dired marks.  Port of dired.el dired-mark-face.
    character(len=:), allocatable, public :: dired_mark_face
    !> Face name used for marked files.  Port of dired.el
    !> dired-marked-face.
    character(len=:), allocatable, public :: dired_marked_face
    !> Face name used for files flagged for deletion.  Port of dired.el
    !> dired-flagged-face.
    character(len=:), allocatable, public :: dired_flagged_face
    !> Face name used for a part of a buffer that needs user attention.
    !> Port of dired.el dired-warning-face.
    character(len=:), allocatable, public :: dired_warning_face
    !> Face name used for permissions of group- and world-writable files.
    !> Port of dired.el dired-perm-write-face.
    character(len=:), allocatable, public :: dired_permission_write_face
    !> Face name used for subdirectories.  Port of dired.el
    !> dired-directory-face.
    character(len=:), allocatable, public :: dired_directory_face
    !> Face name used for symbolic links.  Port of dired.el
    !> dired-symlink-face.
    character(len=:), allocatable, public :: dired_symlink_face
    !> Face name used for files suffixed with `completion_ignored_extensions'.
    !> Port of dired.el dired-ignored-face.
    character(len=:), allocatable, public :: dired_ignored_face

    ! ------------------------------------------------------------------
    ! Internal state
    ! ------------------------------------------------------------------

    character(len=*), parameter :: TAB = achar(9), LF = achar(10)

    !> Regexp for optional initial inode and file size as made by `ls -i -s'.
    character(len=*), parameter :: dired_regular_expression_inode_size = "[0-9 "//TAB//"]*[.,0-9]*[BkKMGTPEZYRQ]?[ "//TAB//"]*"
    !> Regexp matching a marked line.  The match ends just after the marker.
    character(len=*), parameter :: dired_regular_expression_mark = "^[^ "//LF//"]"
    character(len=*), parameter :: dired_regular_expression_maybe_mark = "^. "
    character(len=*), parameter :: dired_regular_expression_directory = dired_regular_expression_maybe_mark &
                                   //dired_regular_expression_inode_size//"d[^:]"
    character(len=*), parameter :: dired_regular_expression_symbol = dired_regular_expression_maybe_mark &
                                   //dired_regular_expression_inode_size//"l[^:]"
    character(len=*), parameter :: dired_regular_expression_special = dired_regular_expression_maybe_mark &
                                   //dired_regular_expression_inode_size//"[bcsp][^:]"
    !> Match ls permission string of an executable file.
    character(len=*), parameter :: dired_regular_expression_exe = dired_regular_expression_maybe_mark &
                                   //dired_regular_expression_inode_size// &
                                   "-[-r][-w][xs][-r][-w].[-r][-w].\|"// &
                                   dired_regular_expression_maybe_mark//dired_regular_expression_inode_size// &
                                   "-[-r][-w].[-r][-w][xs][-r][-w].\|"// &
                                   dired_regular_expression_maybe_mark//dired_regular_expression_inode_size// &
                                   "-[-r][-w].[-r][-w].[-r][-w][xst]"
    character(len=*), parameter :: dired_regular_expression_permissions = "[-bcdlps][-r][-w].[-r][-w].[-r][-w]."
    character(len=*), parameter :: dired_regular_expression_dot = "^.* \.\.?/?$"
    !> Regexp matching a maybe hidden subdirectory line in `ls -lR' output.
    character(len=*), parameter :: dired_subdirectory_regular_expression = "^. \(.+\)\(:\)"//LF
    !> Regular expression to match the permission flags in `ls -l'.
    character(len=*), parameter :: dired_permission_flags_regular_expression = &
                                   "\([^ ]\)[-r][-w]\([^ ]\)[-r][-w]\([^ ]\)[-r][-w]\([^ ]\)"
    !> Regexp recognized by Dired to set `by date' mode.
    character(len=*), parameter :: dired_sort_by_date_regular_expression = &
                                   "\(\`\| \)-[^- ]*t[^ "//dired_ls_sorting_switches//"]*\(\(\`\| +\)\(--[^ ]+\|-[^- t"// &
                                   dired_ls_sorting_switches//"]+\|""[^""]*""\)\)* *$"
    !> Regexp recognized by Dired to set `by name' mode.
    character(len=*), parameter :: dired_sort_by_name_regular_expression = &
                                   "\`\(\(\`\| +\)\(--[^ ]+\|-[^- t"//dired_ls_sorting_switches// &
                                   "]+\|""[^""]*""\)\)* *$"

    !> Values of the LOCALP argument of `dired_get_filename'.
    integer, parameter, public :: DIRED_LOCALP_ABSOLUTE = 0, DIRED_LOCALP_RELATIVE = 1, DIRED_LOCALP_NO_DIRECTORY = 2, &
                                  DIRED_LOCALP_VERBATIM = 3

    !> Kinds of the ARGUMENT of `dired_map_over_marks' and the commands on
    !> marked files: DIRED_ARGUMENT_NONE (the marked files), DIRED_ARGUMENT_INTEGER
    !> with a number N (the next N files), DIRED_ARGUMENT_MARKED (only marked
    !> files) or DIRED_ARGUMENT_OTHER (the current file).
    integer, parameter, public :: DIRED_ARGUMENT_NONE = 0, DIRED_ARGUMENT_INTEGER = 1, DIRED_ARGUMENT_MARKED = 2, &
                                  DIRED_ARGUMENT_OTHER = 3

    !> One element of `dired_subdirectories': a listed directory and a
    !> marker at the start of its header line.
    type :: dired_subdirectory_type
        character(len=:), allocatable :: directory
        type(handle_type) :: marker
    end type dired_subdirectory_type

    !> A Dired buffer's own variables (buffer-local upstream): by buffer
    !> serial number.
    type :: dired_local_type
        integer(int64) :: serial = 0
        logical :: set = .false.
        !> dired-directory
        character(len=:), allocatable :: directory
        !> dired-actual-switches
        character(len=:), allocatable :: actual_switches
        !> dired-subdir-alist, its first element first (the last
        !> directory in the buffer)
        type(dired_subdirectory_type), allocatable :: subdirectory_table(:)
        !> dired-subdir-alist-pre-R (directory names; its markers are
        !> never used)
        logical :: has_pre_r = .false.
        type(string_type), allocatable :: subdirectory_table_pre_r(:)
    end type dired_local_type

    type(dired_local_type), allocatable :: locals(:)

    !> One element of `dired_buffers'.
    type :: dired_buffer_entry_type
        character(len=:), allocatable :: directory
        type(handle_type) :: buffer
    end type dired_buffer_entry_type

    !> Table of expanded directories and their associated Dired buffers
    !> (the first is the most recent).  Port of dired.el dired-buffers.
    type(dired_buffer_entry_type), allocatable :: dired_buffers(:)

    !> A position saved by `dired_save_positions': a window (or a marker
    !> for a window's previous buffer), the file name there (if any) and
    !> the line number.
    type :: dired_saved_position_type
        type(handle_type) :: window, marker
        logical :: has_file = .false.
        character(len=:), allocatable :: file
        integer(int64) :: line = 1
    end type dired_saved_position_type

    !> What `dired_save_positions' returns: the buffer's position, those of
    !> the windows showing it, and those it has in windows' previous buffers.
    type :: dired_positions_type
        type(handle_type) :: buffer
        type(dired_saved_position_type) :: here
        type(dired_saved_position_type), allocatable :: windows(:), previouss(:)
    end type dired_positions_type

    !> Results collected by the bodies of `dired_map_over_marks'.
    type(string_type), allocatable :: map_files(:)
    logical, allocatable :: map_found(:)
    type(handle_type), allocatable :: map_markers(:)
    integer :: map_n = 0
    integer :: map_localp = DIRED_LOCALP_ABSOLUTE
    logical :: map_no_error = .false.

    !> The regexp `dired_mark_files_regular_expression' and its siblings test.
    character(len=:), allocatable :: mark_regular_expression

    !> Arguments of the body of `dired_mark' (dired_repeat_over_lines).
    character :: repeat_character

    !> The files `dired_mark_pop_up' shows.
    type(string_type), allocatable :: pop_up_files(:)

    ! dired-aux.el, in the submodule directory_editor_auxiliary_mod.
    interface
        !> Create a directory called DIRECTORY.
        !> Parent directories of DIRECTORY are created as needed.
        !> If DIRECTORY already exists, an error condition occurs.  Port of
        !> dired-aux.el:dired-create-directory.  (In directory_editor_auxiliary_mod.)
        recursive module subroutine dired_create_directory(directory, status)
            implicit none(type, external)
            character(len=*), intent(in) :: directory
            type(status_type), intent(out) :: status
        end subroutine dired_create_directory

        !> Add an entry for FILENAME in every Dired buffer listing its
        !> directory, marked with MARKER_CHARACTER if present.  Port of
        !> dired-aux.el:dired-add-file.  (In directory_editor_auxiliary_mod.)
        recursive module subroutine dired_add_file(filename, status, marker_character)
            implicit none(type, external)
            character(len=*), intent(in) :: filename
            type(status_type), intent(out) :: status
            character, intent(in), optional :: marker_character
        end subroutine dired_add_file

        !> Add a new Dired entry for FILENAME.
        !> Optionally mark it with MARKER_CHARACTER.  Note that this adds the
        !> entry `out of order' if files are sorted by time, etc.
        !> Skips files that match `dired_trivial_filenames'.
        !> Exposes hidden subdirectories if a file is added there.
        !> ADDED is true on success.  Port of dired-aux.el:dired-add-entry
        !> (no dired-x omitting).  (In directory_editor_auxiliary_mod.)
        recursive module subroutine dired_add_entry(filename, added, status, marker_character, relative)
            implicit none(type, external)
            character(len=*), intent(in) :: filename
            logical, intent(out) :: added
            type(status_type), intent(out) :: status
            character, intent(in), optional :: marker_character
            logical, intent(in), optional :: relative
        end subroutine dired_add_entry

        !> Redisplay all marked (or next ARGUMENT) files.
        !> If on a subdir line, redisplay that subdirectory (not ported
        !> yet: an error).  Port of dired-aux.el:dired-do-redisplay.  (In
        !> directory_editor_auxiliary_mod.)
        recursive module subroutine dired_do_redisplay(argument_kind, argument_n, test_for_subdirectory, status)
            implicit none(type, external)
            integer, intent(in) :: argument_kind
            integer(int64), intent(in) :: argument_n
            logical, intent(in) :: test_for_subdirectory
            type(status_type), intent(out) :: status
        end subroutine dired_do_redisplay

        !> Delete the current line, and insert an entry for FILE (none if
        !> FILE is absent), keeping the mark in its first column.
        !> Port of dired-aux.el:dired-update-file-line.  (In directory_editor_auxiliary_mod.)
        recursive module subroutine dired_update_file_line(status, file)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: file
        end subroutine dired_update_file_line

        !> Go to end of header line of inserted directory DIRECTORY in this Dired
        !> buffer.  Return value of point on success, otherwise 0.  The
        !> next char is \n.  Port of dired-aux.el:dired-goto-subdir.  (In
        !> directory_editor_auxiliary_mod.)
        module function dired_goto_subdirectory(directory) result(position)
            implicit none(type, external)
            character(len=*), intent(in) :: directory
            integer(int64) :: position
        end function dired_goto_subdirectory

        !> Remove all lines of current subdirectory.
        !> Lower levels are unaffected.  Port of
        !> dired-aux.el:dired-kill-subdir (no REMEMBER-MARKS).  (In
        !> directory_editor_auxiliary_mod.)
        recursive module subroutine dired_remove_subdirectory(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine dired_remove_subdirectory

        !> Mark all files except `.' and `..' in current subdirectory.
        !> If the Dired buffer shows multiple directories, this command
        !> marks the files listed in the subdirectory that point is in.
        !> Port of dired-aux.el:dired-mark-subdir-files.  (In directory_editor_auxiliary_mod.)
        recursive module subroutine dired_mark_subdirectory_files(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine dired_mark_subdirectory_files

        !> Return pos of first file line of DIRECTORY, skipping header and total
        !> or wildcard lines.  Port of
        !> dired-aux.el:dired-after-subdir-garbage.  (In directory_editor_auxiliary_mod.)
        module function dired_after_subdirectory_garbage(directory, status) result(position)
            implicit none(type, external)
            character(len=*), intent(in) :: directory
            type(status_type), intent(out) :: status
            integer(int64) :: position
        end function dired_after_subdirectory_garbage

        !> Go to previous subdirectory, regardless of level.  Return the
        !> position or 0.  Port of dired-aux.el:dired-prev-subdir.  (In
        !> directory_editor_auxiliary_mod.)
        module function dired_previous_subdirectory(argument, status, no_error_if_not_found, no_skip) result(position)
            implicit none(type, external)
            integer(int64), intent(in) :: argument
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: no_error_if_not_found, no_skip
            integer(int64) :: position
        end function dired_previous_subdirectory

        !> Change the mode of the marked (or next ARGUMENT) files.
        !> Both octal numeric modes like `644' and symbolic modes like `g+w'
        !> are supported.  Port of dired-aux.el:dired-do-chmod.  (In
        !> directory_editor_auxiliary_mod.)
        recursive module subroutine dired_do_chmod(argument_kind, argument_n, status)
            implicit none(type, external)
            integer, intent(in) :: argument_kind
            integer(int64), intent(in) :: argument_n
            type(status_type), intent(out) :: status
        end subroutine dired_do_chmod

        !> Change the group of the marked (or next ARGUMENT) files.  Port of
        !> dired-aux.el:dired-do-chgrp.  (In directory_editor_auxiliary_mod.)
        recursive module subroutine dired_do_chgrp(argument_kind, argument_n, status)
            implicit none(type, external)
            integer, intent(in) :: argument_kind
            integer(int64), intent(in) :: argument_n
            type(status_type), intent(out) :: status
        end subroutine dired_do_chgrp

        !> Change the owner of the marked (or next ARGUMENT) files.  Port of
        !> dired-aux.el:dired-do-chown.  (In directory_editor_auxiliary_mod.)
        recursive module subroutine dired_do_chown(argument_kind, argument_n, status)
            implicit none(type, external)
            integer, intent(in) :: argument_kind
            integer(int64), intent(in) :: argument_n
            type(status_type), intent(out) :: status
        end subroutine dired_do_chown

        !> Change the timestamp of the marked (or next ARGUMENT) files.
        !> This calls touch.  Port of dired-aux.el:dired-do-touch.  (In
        !> directory_editor_auxiliary_mod.)
        recursive module subroutine dired_do_touch(argument_kind, argument_n, status)
            implicit none(type, external)
            integer, intent(in) :: argument_kind
            integer(int64), intent(in) :: argument_n
            type(status_type), intent(out) :: status
        end subroutine dired_do_touch

        !> Kill the current line (not the files).
        !> Kill ARGUMENT lines starting with the current line.
        !> (A negative argument kills backward.)  Port of
        !> dired-aux.el:dired-kill-line.  (In directory_editor_auxiliary_mod.)
        recursive module subroutine dired_remove_line(argument, status)
            implicit none(type, external)
            integer(int64), intent(in) :: argument
            type(status_type), intent(out) :: status
        end subroutine dired_remove_line

        !> Remove all marked lines, or the next ARGUMENT lines.
        !> The files or directories on those lines are _not_ deleted.  Only the
        !> Dired listing is affected.
        !>
        !> INITIALIZE_COUNT is added to the number of lines removed by this
        !> invocation, for the reporting message; COUNT is that number.
        !>
        !> A FMT of "" will suppress the messaging.  Port of
        !> dired-aux.el:dired-do-kill-lines.  (In directory_editor_auxiliary_mod.)
        recursive module subroutine dired_do_remove_lines(argument_kind, argument_n, status, fmt, initialize_count, count)
            implicit none(type, external)
            integer, intent(in) :: argument_kind
            integer(int64), intent(in) :: argument_n
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: fmt
            integer(int64), intent(in), optional :: initialize_count
            integer(int64), intent(out), optional :: count
        end subroutine dired_do_remove_lines

        !> Compress or uncompress FILE.
        !> NEWNAME is the name of the compressed or uncompressed file;
        !> FOUND is false if there was no change in files.  Port of
        !> dired-aux.el:dired-compress-file.  (In directory_editor_auxiliary_mod.)
        recursive module subroutine dired_compress_file(file, newname, found, status)
            implicit none(type, external)
            character(len=*), intent(in) :: file
            character(len=:), allocatable, intent(out) :: newname
            logical, intent(out) :: found
            type(status_type), intent(out) :: status
        end subroutine dired_compress_file

        !> Compress or uncompress marked (or next ARGUMENT) files.
        !> If invoked on a directory, compress all of the files in
        !> the directory and all of its subdirectories, recursively,
        !> into a .tar.gz archive.
        !> If invoked on a .tar.gz or a .tgz or a .zip or a .7z archive,
        !> uncompress and unpack all the files in the archive.
        !>
        !> If ARGUMENT_KIND is DIRED_ARGUMENT_MARKED, compress only the marked files,
        !> or none if no files are marked.  Port of
        !> dired-aux.el:dired-do-compress.  (In directory_editor_auxiliary_mod.)
        recursive module subroutine dired_do_compress(argument_kind, argument_n, status)
            implicit none(type, external)
            integer, intent(in) :: argument_kind
            integer(int64), intent(in) :: argument_n
            type(status_type), intent(out) :: status
        end subroutine dired_do_compress

        !> Compress selected files and directories to an archive.
        !> Prompt for the archive file name.
        !> Choose the archiving command based on the archive file-name extension
        !> and `dired_compress_files_commands'.  Port of
        !> dired-aux.el:dired-do-compress-to.  (In directory_editor_auxiliary_mod.)
        recursive module subroutine dired_do_compress_to(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine dired_do_compress_to

        !> Remove entry FILE on each Dired buffer.
        !> Note this doesn't delete FILE in the file system.
        !> See `dired_delete_file' in case you wish that.  Port of
        !> dired-aux.el:dired-remove-file.  (In directory_editor_auxiliary_mod.)
        module subroutine dired_remove_file(file)
            implicit none(type, external)
            character(len=*), intent(in) :: file
        end subroutine dired_remove_file

        !> Create or update the line for FILE in all Dired buffers it would
        !> belong in.  Port of dired-aux.el:dired-relist-file.  (In
        !> directory_editor_auxiliary_mod.)
        recursive module subroutine dired_relist_file(file, status)
            implicit none(type, external)
            character(len=*), intent(in) :: file
            type(status_type), intent(out) :: status
        end subroutine dired_relist_file

        !> Query user with PROMPT, and store the answer in STATE.
        !> ANSWER is false or true.
        !>
        !> The user may type y or SPC to accept once; n or DEL to skip once;
        !> ! to accept this and subsequent queries; or q or ESC to decline
        !> this and subsequent queries.
        !>
        !> If STATE already holds an answer (it starts as 0), this function
        !> may return automatically without querying the user.  If STATE
        !> is !, ANSWER is true; if STATE is q or ESC, false.  HELP_PROCEDURE is
        !> what the help character shows.  Port of dired-aux.el:dired-query.
        !> (In directory_editor_auxiliary_mod.)
        recursive module subroutine dired_query(state, prompt, answer, status, help_procedure)
            implicit none(type, external)
            integer, intent(inout) :: state
            character(len=*), intent(in) :: prompt
            logical, intent(out) :: answer
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: help_procedure
        end subroutine dired_query

        !> Copy FROM to TO, overwriting it if OK_FLAG, as Dired's file
        !> creator for copying.  Port of dired-aux.el:dired-copy-file.  (In
        !> directory_editor_auxiliary_mod.)
        recursive module subroutine dired_copy_file(from, to, ok_flag, status)
            implicit none(type, external)
            character(len=*), intent(in) :: from, to
            logical, intent(in) :: ok_flag
            type(status_type), intent(out) :: status
        end subroutine dired_copy_file

        !> Copy FROM to TO; a directory with `copy_directory' when RECURSIVE
        !> (a `dired_recursive_copies' value) says so, a symbolic link as a
        !> link unless DEREFERENCE.  PRESERVE_TIME keeps the modification
        !> time; TOP says FROM is a top-level file of the operation.  Port of
        !> dired-aux.el:dired-copy-file-recursive.  (In directory_editor_auxiliary_mod.)
        recursive module subroutine dired_copy_file_recursive(from, to, ok_flag, status, preserve_time, top, recursive, &
                                                              dereference)
            implicit none(type, external)
            character(len=*), intent(in) :: from, to
            logical, intent(in) :: ok_flag
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: preserve_time, top, dereference
            type(choice_type), intent(in), optional :: recursive
        end subroutine dired_copy_file_recursive

        !> Rename FILE to NEWNAME.
        !> Signal a `file_already_exists' error if a file NEWNAME already exists
        !> unless OK_IF_ALREADY_EXISTS is true.  Port of
        !> dired-aux.el:dired-rename-file (`dired_vc_rename_file' is
        !> deferred).  (In directory_editor_auxiliary_mod.)
        recursive module subroutine dired_rename_file(file, newname, ok_if_already_exists, status)
            implicit none(type, external)
            character(len=*), intent(in) :: file, newname
            logical, intent(in) :: ok_if_already_exists
            type(status_type), intent(out) :: status
        end subroutine dired_rename_file

        !> Give FILE the additional name NEWNAME and update its line.
        !> Port of dired-aux.el:dired-hardlink.  (In directory_editor_auxiliary_mod.)
        recursive module subroutine dired_hardlink(file, newname, ok_if_already_exists, status)
            implicit none(type, external)
            character(len=*), intent(in) :: file, newname
            logical, intent(in) :: ok_if_already_exists
            type(status_type), intent(out) :: status
        end subroutine dired_hardlink

        !> Make a symbolic link (pointing to FILE1) in FILE2.
        !> The link is relative (if possible), for example
        !>
        !>     "/vol/tex/bin/foo" "/vol/local/bin/foo"
        !>
        !> results in
        !>
        !>     "../../tex/bin/foo" "/vol/local/bin/foo"
        !>
        !> Port of dired-aux.el:dired-make-relative-symlink.  (In
        !> directory_editor_auxiliary_mod.)
        recursive module subroutine dired_make_relative_symlink(file1, file2, status, ok_if_already_exists)
            implicit none(type, external)
            character(len=*), intent(in) :: file1, file2
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: ok_if_already_exists
        end subroutine dired_make_relative_symlink

        !> Create one or more new files from a list of existing files FILE_NAMES.
        !> This function also handles querying the user, updating Dired
        !> buffers, and displaying a success or failure message.
        !>
        !> FILE_CREATOR is called once for each file in FILE_NAMES, and must
        !> create a new file, querying the user and updating Dired buffers
        !> as necessary.  It is given the old file name, the new name, and
        !> an argument OK_FLAG with the same meaning as
        !> OK_IF_ALREADY_EXISTS in `copy_file'.  CREATOR_KIND tells
        !> whether it is `dired_copy_file' (DIRED_CREATOR_COPY).
        !>
        !> OPERATION should be a capitalized string describing the operation
        !> performed (e.g. `Copy').  It is used for error logging.
        !>
        !> FILE_NAMES is the list of files to copy (full absolute file names).
        !>
        !> NAME_CONSTRUCTOR is given the name of an old file and returns
        !> either the corresponding new file name or SKIP .true. to skip.
        !>
        !> Mark each newly-created file's Dired entry with the character
        !> MARKER_CHARACTER, or with the file's own mark if MARKER_CHARACTER is
        !> DIRED_KEEP_MARKER (" " for none).  Port of
        !> dired-aux.el:dired-create-files.  (In directory_editor_auxiliary_mod.)
        recursive module subroutine dired_create_files(file_creator, creator_kind, operation, file_names, &
                                                       name_constructor, marker_character, status)
            implicit none(type, external)
            procedure(dired_file_creator_procedure) :: file_creator
            integer, intent(in) :: creator_kind
            character(len=*), intent(in) :: operation
            type(string_type), intent(in) :: file_names(:)
            procedure(dired_name_constructor_procedure) :: name_constructor
            character, intent(in) :: marker_character
            type(status_type), intent(out) :: status
        end subroutine dired_create_files

        !> Create a new file for each marked file.
        !> Prompt user for a target directory in which to create the new
        !>   files.  The target may also be a non-directory file, if only
        !>   one file is marked.  The initial suggestion for target is the
        !>   Dired buffer's current directory (or, if `dired_do_what_i_mean_target' is
        !>   set, the current directory of a neighboring Dired window).
        !>
        !> OP_SYMBOL is the name for the operation.  Function
        !>   `dired_mark_pop_up' will determine whether pop-ups are
        !>   appropriate for this OP_SYMBOL.
        !>
        !> FILE_CREATOR, CREATOR_KIND and OPERATION as in `dired_create_files'.
        !>
        !> ARGUMENT as in `dired_get_marked_files'.
        !>
        !> MARKER_CHARACTER as in `dired_create_files'.
        !>
        !> OP1 is an alternate form for OPERATION if there is only one file.
        !>
        !> HOW_TO_PLAIN true means the target is always treated as a plain
        !> file (upstream's HOW-TO t); otherwise `is_file_directory' tells
        !> whether the target is a directory.  Port of
        !> dired-aux.el:dired-do-create-files (HOW-TO functions are
        !> deferred).  (In directory_editor_auxiliary_mod.)
        recursive module subroutine dired_do_create_files(op_symbol, file_creator, creator_kind, operation, argument_kind, &
                                                          argument_n, marker_character, status, op1, how_to_plain)
            implicit none(type, external)
            character(len=*), intent(in) :: op_symbol, operation
            procedure(dired_file_creator_procedure) :: file_creator
            integer, intent(in) :: creator_kind, argument_kind
            integer(int64), intent(in) :: argument_n
            character, intent(in) :: marker_character
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: op1
            logical, intent(in), optional :: how_to_plain
        end subroutine dired_do_create_files

        !> Copy all marked (or next ARGUMENT) files, or copy the current file,
        !> into the directory (or to the file) read in the minibuffer;
        !> INVERT_DEREFERENCE inverts `dired_copy_dereference' (C-u).
        !> Port of dired-aux.el:dired-do-copy.  (In directory_editor_auxiliary_mod.)
        recursive module subroutine dired_do_copy(argument_kind, argument_n, status, invert_dereference)
            implicit none(type, external)
            integer, intent(in) :: argument_kind
            integer(int64), intent(in) :: argument_n
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: invert_dereference
        end subroutine dired_do_copy

        !> Make symbolic links to current file or all marked (or next ARGUMENT)
        !> files.  Port of dired-aux.el:dired-do-symlink.  (In directory_editor_auxiliary_mod.)
        recursive module subroutine dired_do_symlink(argument_kind, argument_n, status)
            implicit none(type, external)
            integer, intent(in) :: argument_kind
            integer(int64), intent(in) :: argument_n
            type(status_type), intent(out) :: status
        end subroutine dired_do_symlink

        !> Relative symlink all marked (or next ARGUMENT) files into a
        !> directory.  Port of dired-aux.el:dired-do-relsymlink.  (In
        !> directory_editor_auxiliary_mod.)
        recursive module subroutine dired_do_relsymlink(argument_kind, argument_n, status)
            implicit none(type, external)
            integer, intent(in) :: argument_kind
            integer(int64), intent(in) :: argument_n
            type(status_type), intent(out) :: status
        end subroutine dired_do_relsymlink

        !> Add names (hard links) current file or all marked (or next ARGUMENT)
        !> files.  Port of dired-aux.el:dired-do-hardlink.  (In directory_editor_auxiliary_mod.)
        recursive module subroutine dired_do_hardlink(argument_kind, argument_n, status)
            implicit none(type, external)
            integer, intent(in) :: argument_kind
            integer(int64), intent(in) :: argument_n
            type(status_type), intent(out) :: status
        end subroutine dired_do_hardlink

        !> Rename current file or all marked (or next ARGUMENT) files.  Port of
        !> dired-aux.el:dired-do-rename.  (In directory_editor_auxiliary_mod.)
        recursive module subroutine dired_do_rename(argument_kind, argument_n, status)
            implicit none(type, external)
            integer, intent(in) :: argument_kind
            integer(int64), intent(in) :: argument_n
            type(status_type), intent(out) :: status
        end subroutine dired_do_rename

        !> Rename selected files whose names match REGULAR_EXPRESSION to NEWNAME.
        !> With WHOLE_NAME, renaming by regexp affects the absolute file
        !> name.  Port of dired-aux.el:dired-do-rename-regexp.  (In
        !> directory_editor_auxiliary_mod.)
        recursive module subroutine dired_do_rename_regular_expression(regular_expression, newname, argument_kind, argument_n, &
                                                                       whole_name, status)
            implicit none(type, external)
            character(len=*), intent(in) :: regular_expression, newname
            integer, intent(in) :: argument_kind
            integer(int64), intent(in) :: argument_n
            logical, intent(in) :: whole_name
            type(status_type), intent(out) :: status
        end subroutine dired_do_rename_regular_expression

        !> Copy selected files whose names match REGULAR_EXPRESSION to NEWNAME.
        !> See function `dired_do_rename_regular_expression' for more info.  Port of
        !> dired-aux.el:dired-do-copy-regexp.  (In directory_editor_auxiliary_mod.)
        recursive module subroutine dired_do_copy_regular_expression(regular_expression, newname, argument_kind, argument_n, &
                                                                     whole_name, status)
            implicit none(type, external)
            character(len=*), intent(in) :: regular_expression, newname
            integer, intent(in) :: argument_kind
            integer(int64), intent(in) :: argument_n
            logical, intent(in) :: whole_name
            type(status_type), intent(out) :: status
        end subroutine dired_do_copy_regular_expression

        !> Hardlink selected files whose names match REGULAR_EXPRESSION to NEWNAME.
        !> See function `dired_do_rename_regular_expression' for more info.  Port of
        !> dired-aux.el:dired-do-hardlink-regexp.  (In directory_editor_auxiliary_mod.)
        recursive module subroutine dired_do_hardlink_regular_expression(regular_expression, newname, argument_kind, argument_n, &
                                                                         whole_name, status)
            implicit none(type, external)
            character(len=*), intent(in) :: regular_expression, newname
            integer, intent(in) :: argument_kind
            integer(int64), intent(in) :: argument_n
            logical, intent(in) :: whole_name
            type(status_type), intent(out) :: status
        end subroutine dired_do_hardlink_regular_expression

        !> Symlink selected files whose names match REGULAR_EXPRESSION to NEWNAME.
        !> See function `dired_do_rename_regular_expression' for more info.  Port of
        !> dired-aux.el:dired-do-symlink-regexp.  (In directory_editor_auxiliary_mod.)
        recursive module subroutine dired_do_symlink_regular_expression(regular_expression, newname, argument_kind, argument_n, &
                                                                        whole_name, status)
            implicit none(type, external)
            character(len=*), intent(in) :: regular_expression, newname
            integer, intent(in) :: argument_kind
            integer(int64), intent(in) :: argument_n
            logical, intent(in) :: whole_name
            type(status_type), intent(out) :: status
        end subroutine dired_do_symlink_regular_expression

        !> RelSymlink all marked files containing REGULAR_EXPRESSION to NEWNAME.
        !> See functions `dired_do_rename_regular_expression' and `dired_do_relsymlink'
        !> for more info.  Port of dired-aux.el:dired-do-relsymlink-regexp.
        !> (In directory_editor_auxiliary_mod.)
        recursive module subroutine dired_do_relsymlink_regular_expression(regular_expression, newname, argument_kind, argument_n, &
                                                                           whole_name, status)
            implicit none(type, external)
            character(len=*), intent(in) :: regular_expression, newname
            integer, intent(in) :: argument_kind
            integer(int64), intent(in) :: argument_n
            logical, intent(in) :: whole_name
            type(status_type), intent(out) :: status
        end subroutine dired_do_relsymlink_regular_expression

        !> Rename all marked (or next ARGUMENT) files to upper case.  Port of
        !> dired-aux.el:dired-upcase.  (In directory_editor_auxiliary_mod.)
        recursive module subroutine dired_upcase(argument_kind, argument_n, status)
            implicit none(type, external)
            integer, intent(in) :: argument_kind
            integer(int64), intent(in) :: argument_n
            type(status_type), intent(out) :: status
        end subroutine dired_upcase

        !> Rename all marked (or next ARGUMENT) files to lower case.  Port of
        !> dired-aux.el:dired-downcase.  (In directory_editor_auxiliary_mod.)
        recursive module subroutine dired_downcase(argument_kind, argument_n, status)
            implicit none(type, external)
            integer, intent(in) :: argument_kind
            integer(int64), intent(in) :: argument_n
            type(status_type), intent(out) :: status
        end subroutine dired_downcase

        !> Create an empty file called FILE.
        !> Parent directories of FILE are created as needed.
        !> Add an entry in the Dired buffer for the topmost new parent
        !> directory of FILE, if created, otherwise for the new file.
        !> If FILE already exists, an error condition occurs.  Port of
        !> dired-aux.el:dired-create-empty-file.  (In directory_editor_auxiliary_mod.)
        recursive module subroutine dired_create_empty_file(file, status)
            implicit none(type, external)
            character(len=*), intent(in) :: file
            type(status_type), intent(out) :: status
        end subroutine dired_create_empty_file

        !> In Dired, examine a file in view mode, returning to Dired when done.
        !> When file is a directory, show it in this buffer if it is inserted.
        !> Otherwise, display it in another buffer.  Port of
        !> dired.el:dired-view-file.  (In directory_editor_auxiliary_mod.)
        recursive module subroutine dired_view_file(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine dired_view_file

        !> Print the type of FILE, according to the `file' command.
        !> If DEREF_SYMLINKS is true and FILE is a symbolic link, then the
        !> command will print the type of the target of the link instead.
        !> Port of dired-aux.el:dired-show-file-type.  (In directory_editor_auxiliary_mod.)
        recursive module subroutine dired_show_file_type(file, deref_symlinks, status)
            implicit none(type, external)
            character(len=*), intent(in) :: file
            logical, intent(in) :: deref_symlinks
            type(status_type), intent(out) :: status
        end subroutine dired_show_file_type

        !> Bind the dired-aux.el commands in `dired_mode_map'.  (In
        !> directory_editor_auxiliary_mod.)  Not a port.
        module subroutine dired_aux_keys()
            implicit none(type, external)
        end subroutine dired_aux_keys

        !> Register the dired-aux.el commands.  (In directory_editor_auxiliary_mod.)
        !> Not a port.
        module subroutine initialize_dired_aux()
            implicit none(type, external)
        end subroutine initialize_dired_aux
    end interface

contains

    ! ------------------------------------------------------------------
    ! Buffer-local variables
    ! ------------------------------------------------------------------

    !> Free the markers of a subdirectory table.  Not a port.
    subroutine destroy_table(table)
        type(dired_subdirectory_type), allocatable, intent(inout) :: table(:)
        integer :: k

        if (.not. allocated(table)) return

        do k = 1, size(table)
            if (table(k)%marker%index /= 0) call destroy_marker_if_live(table(k)%marker)
        end do

        deallocate (table)
    end subroutine destroy_table

    !> Forget entry K (of a closed buffer, or for kill-all-local-variables).
    !> Not a port.
    subroutine clear_entry(k)
        integer, intent(in) :: k

        call destroy_table(locals(k)%subdirectory_table)
        if (allocated(locals(k)%directory)) deallocate (locals(k)%directory)
        if (allocated(locals(k)%actual_switches)) deallocate (locals(k)%actual_switches)
        if (allocated(locals(k)%subdirectory_table_pre_r)) deallocate (locals(k)%subdirectory_table_pre_r)
        locals(k)%has_pre_r = .false.
        locals(k)%set = .false.
        locals(k)%serial = 0
    end subroutine clear_entry

    !> The index of the current buffer's entry in `locals' (0 if none).
    !> Not a port.
    integer function local_index() result(k)
        k = 0
        if (current == 0) return
        if (.not. allocated(locals)) return
        k = findloc(locals%serial, buffers(current)%serial, mask=locals%set, dim=1)
    end function local_index

    !> Find or make the current buffer's entry in `locals', reusing the
    !> entry of a closed buffer; K is its index (0 without a current
    !> buffer).  Not a port.
    subroutine make_local_entry(k)
        integer, intent(out) :: k
        type(dired_local_type) :: entry

        k = local_index()
        if (k > 0 .or. current == 0) return
        if (.not. allocated(locals)) allocate (locals(0))

        ! Reuse the entry of a dead buffer.
        do k = 1, size(locals)
            if (.not. locals(k)%set) exit
            if (.not. buffer_serial_live(locals(k)%serial)) exit
        end do

        if (k > size(locals)) then
            locals = [locals, entry]
        else if (locals(k)%set) then
            call clear_entry(k)
        end if

        locals(k)%set = .true.
        locals(k)%serial = buffers(current)%serial
    end subroutine make_local_entry

    !> kill-all-local-variables: Dired's buffer-local variables.
    !> Not a port.
    subroutine clear_dired_locals()
        integer :: k

        k = local_index()
        if (k > 0) call clear_entry(k)
    end subroutine clear_dired_locals

    !> The current buffer's `dired_directory' ("" if unset).  Port of
    !> dired.el dired-directory (buffer-local).
    function dired_directory_of() result(directory)
        character(len=:), allocatable :: directory
        integer :: k

        directory = ""
        k = local_index()
        if (k == 0) return
        if (allocated(locals(k)%directory)) directory = locals(k)%directory
    end function dired_directory_of

    !> The current buffer's `dired_actual_switches' ("" if unset): the
    !> value of `dired_listing_switches' used to make this buffer's text.
    !> Port of dired.el dired-actual-switches (buffer-local).
    function dired_actual_switches_of() result(sw)
        character(len=:), allocatable :: sw
        integer :: k

        sw = ""
        k = local_index()
        if (k == 0) return
        if (allocated(locals(k)%actual_switches)) sw = locals(k)%actual_switches
    end function dired_actual_switches_of

    !> The directories of the current buffer's `dired_subdirectories', in
    !> its order (the last one in the buffer first).  Not a port.
    function dired_subdirectory_directories() result(directories)
        type(string_type), allocatable :: directories(:)
        integer :: k, i

        allocate (directories(0))
        k = local_index()
        if (k == 0) return
        if (.not. allocated(locals(k)%subdirectory_table)) return
        deallocate (directories)
        allocate (directories(size(locals(k)%subdirectory_table)))

        do i = 1, size(directories)
            directories(i)%text = locals(k)%subdirectory_table(i)%directory
        end do
    end function dired_subdirectory_directories

    !> The number of elements of the current buffer's `dired_subdirectories'.
    !> Not a port.
    integer function table_size()
        integer :: k

        table_size = 0
        k = local_index()
        if (k == 0) return
        if (allocated(locals(k)%subdirectory_table)) table_size = size(locals(k)%subdirectory_table)
    end function table_size

    !> The current buffer's `default_directory'.  Not a port.
    function default_directory() result(directory)
        character(len=:), allocatable :: directory

        directory = "./"
        if (current == 0) return
        if (allocated(buffers(current)%directory)) directory = buffers(current)%directory
    end function default_directory

    !> `expand_file_name' as a function (relative to `default_directory').
    !> Not a port.
    function expand(name, directory) result(full)
        character(len=*), intent(in) :: name
        character(len=*), intent(in), optional :: directory
        character(len=:), allocatable :: full
        type(status_type) :: st

        call expand_file_name(name, full, st, directory)
        if (.not. st%ok()) full = name
    end function expand

    ! ------------------------------------------------------------------
    ! Small helpers
    ! ------------------------------------------------------------------

    !> Whether text after point matches REGULAR_EXPRESSION (looking-at-p).  Not a port.
    logical function is_looking_at(regular_expression)
        character(len=*), intent(in) :: regular_expression
        type(status_type) :: st

        is_looking_at = looking_at(regular_expression, st, inhibit_modify=.true.)
    end function is_looking_at

    !> Whether REGULAR_EXPRESSION matches STRING (string-match-p).  Not a port.
    logical function string_matches(regular_expression, string)
        character(len=*), intent(in) :: regular_expression, string
        type(status_type) :: st

        string_matches = string_match(regular_expression, string, st, inhibit_modify=.true.) > 0
    end function string_matches

    !> Search forward for REGULAR_EXPRESSION, bounded by BOUND if positive; true if
    !> found (point after the match), else point unchanged.  Not a port.
    logical function is_regular_expression_search_forward(regular_expression, bound, count) result(found)
        character(len=*), intent(in) :: regular_expression
        integer(int64), intent(in), optional :: bound, count
        integer(int64) :: position
        type(status_type) :: st

        found = .false.

        if (present(bound)) then
            if (bound > 0) then
                call regular_expression_search_forward(regular_expression, position, st, bound=bound, noerror=SEARCH_NOERROR, &
                                                       count=count)
            else
                call regular_expression_search_forward(regular_expression, position, st, noerror=SEARCH_NOERROR, count=count)
            end if
        else
            call regular_expression_search_forward(regular_expression, position, st, noerror=SEARCH_NOERROR, count=count)
        end if

        found = st%ok() .and. position > 0
    end function is_regular_expression_search_forward

    !> Search backward for REGULAR_EXPRESSION; true if found.  Not a port.
    logical function is_regular_expression_search_backward(regular_expression, count) result(found)
        character(len=*), intent(in) :: regular_expression
        integer(int64), intent(in), optional :: count
        integer(int64) :: position
        type(status_type) :: st

        call regular_expression_search_backward(regular_expression, position, st, noerror=SEARCH_NOERROR, count=count)
        found = st%ok() .and. position > 0
    end function is_regular_expression_search_backward

    !> The position of marker M (0 if it points nowhere).  Not a port.
    integer(int64) function marked_position(m)
        type(handle_type), intent(in) :: m
        type(status_type) :: st

        call marker_position(m, marked_position, st)
        if (.not. st%ok()) marked_position = 0
    end function marked_position

    !> Free marker M, ignoring a stale handle.  Not a port.
    subroutine destroy_marker_if_live(m)
        type(handle_type), intent(in) :: m
        type(status_type) :: st

        call destroy_marker(m, st)
    end subroutine destroy_marker_if_live

    !> Match data position (0 if the group did not match).  Not a port.
    integer(int64) function mbeg(n)
        integer, intent(in) :: n
        type(status_type) :: st

        mbeg = match_beginning(n, st)
    end function mbeg

    ! Not a port.
    integer(int64) function mend(n)
        integer, intent(in) :: n
        type(status_type) :: st

        mend = match_end(n, st)
    end function mend

    !> The position N lines after this one's start (line-beginning-position N).
    !> Not a port.
    integer(int64) function line_beginning(n)
        integer(int64), intent(in), optional :: n

        line_beginning = line_beginning_position(n)
    end function line_beginning

    ! Not a port.
    integer(int64) function line_end()
        line_end = line_end_position()
    end function line_end

    !> Move N lines (forward-line), ignoring the shortage.  Not a port.
    subroutine fwd_line(n)
        integer(int64), intent(in) :: n
        integer(int64) :: shortage

        shortage = forward_line(n)
    end subroutine fwd_line

    !> The character after POSITION (default point) as an ASCII character, or
    !> achar(0) when there is none or it is not ASCII.  Not a port.
    character function character_at(position)
        integer(int64), intent(in), optional :: position
        integer(int32) :: c
        logical :: found

        if (present(position)) then
            call character_after(c, found, position)
        else
            call character_after(c, found)
        end if

        character_at = achar(0)
        if (found .and. c >= 0 .and. c < 128) character_at = achar(c)
    end function character_at

    !> The character before point (ASCII, else achar(0)).  Not a port.
    character function character_before_point()
        integer(int32) :: c
        logical :: found

        call character_before(c, found)
        character_before_point = achar(0)
        if (found .and. c >= 0 .and. c < 128) character_before_point = achar(c)
    end function character_before_point

    !> Bind `inhibit_read_only' to true (SAVED keeps the old value).
    !> Not a port.
    subroutine inhibit_read_only_begin(saved)
        type(value_type), intent(out) :: saved
        type(status_type) :: st

        saved = inhibit_read_only()
        call set(value_type(name="inhibit_read_only"), value_type(.true.), st)
    end subroutine inhibit_read_only_begin

    ! Not a port.
    subroutine inhibit_read_only_end(saved)
        type(value_type), intent(in) :: saved
        type(status_type) :: st

        call set(value_type(name="inhibit_read_only"), saved, st)
    end subroutine inhibit_read_only_end

    !> Replace the character after point with C (delete-char 1, insert).
    !> Not a port.
    subroutine replace_character_at_point(c, status)
        character, intent(in) :: c
        type(status_type), intent(out) :: status

        call delete_character(1_int64, status)
        if (.not. status%ok()) return
        call insert(c, status)
    end subroutine replace_character_at_point

    !> TEXT with every occurrence of FROM replaced by TO (string-replace).
    !> Not a port.
    function string_replace(from, to, text) result(out)
        character(len=*), intent(in) :: from, to, text
        character(len=:), allocatable :: out
        integer :: i, k

        out = ""
        i = 1

        do
            k = index(text(i:), from)
            if (k == 0) exit
            out = out//text(i:i + k - 2)//to
            i = i + k - 1 + len(from)
        end do

        out = out//text(i:)
    end function string_replace

    !> "" if COUNT is 1, else "s".  Port of dired.el:dired-plural-s.
    function dired_plural_s(count) result(s)
        integer(int64), intent(in) :: count
        character(len=:), allocatable :: s

        s = "s"
        if (count == 1) s = ""
    end function dired_plural_s

    !> Execute hook HOOK, ignoring its status.  Not a port.
    subroutine execute_hook_quietly(hook)
        type(hook_type), intent(inout) :: hook
        type(status_type) :: st

        call hook%execute(st)
    end subroutine execute_hook_quietly

    ! ------------------------------------------------------------------
    ! Switches
    ! ------------------------------------------------------------------

    !> Return true if the string SWITCHES matches LONG or SHORT format.
    !> Port of dired.el:dired-check-switches.
    logical function dired_check_switches(switches, short, long) result(p)
        character(len=*), intent(in) :: switches, short
        character(len=*), intent(in), optional :: long
        character(len=:), allocatable :: regular_expression
        logical :: saved

        regular_expression = "\(\`\| \)-[[:alnum:]]*"//short
        if (present(long)) regular_expression = regular_expression//"\|--"//long//"\>"
        saved = case_fold_search%value
        case_fold_search%value = .false.
        p = string_matches(regular_expression, switches)
        case_fold_search%value = saved
    end function dired_check_switches

    !> Return true if the string SWITCHES contains -b or --escape.
    !> Port of dired.el:dired-switches-escape-p.
    logical function is_dired_switches_escape(switches)
        character(len=*), intent(in) :: switches

        is_dired_switches_escape = dired_check_switches(switches, "b", "\(quoting-style=\)?escape")
    end function is_dired_switches_escape

    !> Return true if the string SWITCHES contains -R or --recursive.
    !> Port of dired.el:dired-switches-recursive-p.
    logical function is_dired_switches_recursive(switches)
        character(len=*), intent(in) :: switches

        is_dired_switches_recursive = dired_check_switches(switches, "R", "recursive")
    end function is_dired_switches_recursive

    !> Return true if `ls' supports the "--dired" switch.  Port of
    !> dired.el:dired--check-use-ls-dired.
    logical function dired_check_use_ls_dired()
        type(value_type) :: result
        type(status_type) :: st

        call execute_process(insert_directory_program, result, st, arguments=list(value_type(text="--dired"), &
                                                                                  value_type(text="-N")))
        dired_check_use_ls_dired = .false.
        if (st%ok() .and. vtag(result) == V_INTEGER) dired_check_use_ls_dired = xfixnum(result) == 0
    end function dired_check_use_ls_dired

    ! ------------------------------------------------------------------
    ! Reading in a directory
    ! ------------------------------------------------------------------

    !> Insert a directory listing of DIRECTORY, Dired style.
    !> Use SWITCHES to make the listings.
    !> If FILE_LIST is present, list only those files.
    !> Otherwise, if WILDCARD is true, expand wildcards;
    !>  in that case, DIRECTORY should be a file name that uses wildcards.
    !> In other cases, DIRECTORY should be a directory name or a directory filename.
    !> If HDR is true, insert a header line with the directory name.
    !> Port of dired.el:dired-insert-directory (no remote directories or
    !> globstar).
    recursive subroutine dired_insert_directory(directory_in, switches_in, status, file_list, wildcard, hdr)
        character(len=*), intent(in) :: directory_in, switches_in
        type(status_type), intent(out) :: status
        type(string_type), intent(in), optional :: file_list(:)
        logical, intent(in), optional :: wildcard, hdr
        character(len=:), allocatable :: directory, switches, dw_directory, dw_wild, script, saved_directory, directory_name
        type(value_type) :: result, properties, saved_ro
        type(handle_type) :: end_marker
        type(status_type) :: st
        integer(int64) :: original_point, beginning, found, mb, content_point, here
        logical :: wild, header, use_ls_dired, directory_wildcard, has_list, changed, saved_tabs, had_directory
        integer :: k

        directory = directory_in
        switches = switches_in
        wild = .false.
        if (present(wildcard)) wild = wildcard
        header = .false.
        if (present(hdr)) header = hdr
        has_list = present(file_list)
        status = status_type()
        original_point = point()

        ! Use -N with --dired, to countermand possible non-default quoting
        ! style, in particular via the environment variable QUOTING_STYLE.
        if (dired_use_ls_dired%choice == DIRED_USE_LS_DIRED_UNSPECIFIED) then
            if (dired_check_use_ls_dired()) then
                dired_use_ls_dired = .true.
            else
                dired_use_ls_dired = .false.
                call message(format_message("ls does not support --dired -N; see `dired_use_ls_dired' "// &
                                            "for more details."))
            end if

            use_ls_dired = dired_use_ls_dired%value
        else
            use_ls_dired = .not. dired_use_ls_dired%is_off()
        end if

        if (use_ls_dired) switches = "--dired -N "//switches
        ! Expand directory wildcards and fill file-list.
        directory_wildcard = .false.
        if (.not. has_list .and. wild) directory_wildcard = is_insert_directory_wildcard_in_directory(directory, dw_directory, &
                                                                                                      dw_wild)

        if (directory_wildcard) then
            switches = "-d "//switches
            script = insert_directory_program//" "//switches//" "//dw_wild
            had_directory = allocated(buffers(current)%directory)
            if (had_directory) saved_directory = buffers(current)%directory
            buffers(current)%directory = dw_directory
            call execute_process(shell_name(), result, status, destination=value_type(.true.), &
                                 arguments=list(value_type(text="-c"), value_type(text=script)))

            if (had_directory) then
                buffers(current)%directory = saved_directory
            else
                deallocate (buffers(current)%directory)
            end if

            if (.not. status%ok()) return

            if (.not. (vtag(result) == V_INTEGER)) then
                call set_error(status, USER_ERROR_CODE, dw_wild//": No files matching wildcard")
                return
            else if (xfixnum(result) /= 0) then
                call set_error(status, USER_ERROR_CODE, dw_wild//": No files matching wildcard")
                return
            end if

            call insert_directory_clean(point(), switches, status)
            if (.not. status%ok()) return
        else if (has_list) then
            do k = 1, size(file_list)
                beginning = point()
                call insert_directory(file_list(k)%text, switches, status)
                if (.not. status%ok()) return
                ! `dired_align_file' doesn't fare well with dired
                ! implementations that don't indent entries by one column.
                here = point()
                call goto_character(beginning)

                if (.not. is_looking_at(" ")) then
                    call insert(" ", status)
                    here = here + 1
                end if

                call goto_character(here)
                ! Re-align fields, if necessary.
                call dired_align_file(beginning, point())
            end do
        else
            call insert_directory(directory, switches, status, wild,.not. wild)
            if (.not. status%ok()) return
        end if

        ! Quote certain characters, unless ls quoted them for us.
        if (.not. is_dired_switches_escape(dired_actual_switches_of())) then
            here = point()
            call make_marker(end_marker)
            call set_marker(end_marker, here, st)
            call goto_character(original_point)

            do
                call search_forward("\", found, st, bound=marked_position(end_marker), noerror=SEARCH_NOERROR)
                if (found <= 0) exit
                mb = mbeg(0)
                call text_properties_at(mb, properties, st)
                call replace_match("\\", st, literal=.true.)
                call set_text_properties(mb, mb + 2, properties, changed, st)
            end do

            call goto_character(original_point)

            do
                call search_forward(achar(13), found, st, bound=marked_position(end_marker), noerror=SEARCH_NOERROR)
                if (found <= 0) exit
                mb = mbeg(0)
                call text_properties_at(mb, properties, st)
                call replace_match("\015", st, literal=.true.)
                call set_text_properties(mb, mb + 4, properties, changed, st)
            end do

            call goto_character(marked_position(end_marker))
            call destroy_marker_if_live(end_marker)
        else
            ! Replace any newlines in DIRECTORY with literal "\n"s, for the sake
            ! of the header line.  To disambiguate a literal "\n" in the
            ! actual dirname, we also replace "\" with "\\".
            directory = string_replace("\", "\\", directory)
            directory = string_replace(LF, "\n", directory)
        end if

        ! If we used --dired and it worked, the lines are already indented.
        ! Otherwise, indent them.
        here = point()
        call goto_character(original_point)

        if (.not. is_looking_at("  ")) then
            call goto_character(here)
            saved_tabs = indent_tabs_mode%value
            indent_tabs_mode%value = .false.
            call inhibit_read_only_begin(saved_ro)
            call indent_rigidly(original_point, point(), 2_int64, st)
            call inhibit_read_only_end(saved_ro)
            indent_tabs_mode%value = saved_tabs
        else
            call goto_character(here)
        end if

        ! Insert text at the beginning to standardize things.
        content_point = original_point
        here = point()
        call make_marker(end_marker)
        call set_marker(end_marker, here, st)
        call goto_character(original_point)

        if (header .or. wild) then
            if (.not. header_present()) then
                ! Inserted directory name must be absolute, but keep in
                ! mind it may be replaced in some instances like in
                ! `dired_build_subdirectories'.
                if (directory_wildcard) then
                    directory_name = dw_directory
                else
                    directory_name = directory_file_name(file_name_directory(directory))
                end if

                call insert("  "//directory_name//":"//LF, status)
                if (.not. status%ok()) return
            end if

            content_point = point()
        end if

        if (wild) then
            ! Insert "wildcard" line where "total" line would be for a full dir.
            if (is_insert_directory_wildcard_in_directory(directory, dw_directory, dw_wild)) then
                call insert("  wildcard "//dw_wild//LF, status)
            else
                call insert("  wildcard "//file_name_nondirectory(directory)//LF, status)
            end if

            if (.not. status%ok()) return
        end if

        content_point = dired_insert_disk_space(original_point, directory, status)
        if (.not. status%ok()) return
        call goto_character(marked_position(end_marker))
        call destroy_marker_if_live(end_marker)
        call dired_insert_set_properties(content_point, point())

    contains

        !> (and (looking-at "^  \\(.*\\):$")
        !>      (file-name-absolute-p (match-string 1)))
        logical function header_present()
            character(len=:), allocatable :: name
            type(status_type) :: s2

            header_present = .false.
            if (.not. looking_at("^  \(.*\):$", s2)) return
            call buffer_substring(mbeg(1), mend(1), name, s2)
            header_present = is_file_name_absolute(name)
        end function header_present
    end subroutine dired_insert_directory

    !> The shell for wildcard listings: `shell_file_name' (callproc.c).
    !> Not a port.
    function shell_name() result(sh)
        character(len=:), allocatable :: sh

        if (.not. allocated(shell_file_name%value)) call initialize_callproc()
        sh = shell_file_name%value
    end function shell_name

    !> Insert the amount of free space after the listing that starts at
    !> BEGINNING (for FILE), as `dired_free_space' says; return where the file
    !> lines start.  Port of dired.el:dired--insert-disk-space.
    integer(int64) function dired_insert_disk_space(beginning, file, status) result(position)
        integer(int64), intent(in) :: beginning
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: available
        integer(int64) :: original_point
        logical :: found

        status = status_type()
        original_point = point()
        position = beginning
        call goto_character(beginning)

        ! First find the line to put it on.
        if (.not. is_regular_expression_search_forward("^ *\(total\)")) then
            call goto_character(original_point)
            return
        end if

        if (dired_free_space%is_off() .or. dired_free_space%choice == DIRED_FREE_SPACE_FIRST) then
            call delete_region(mbeg(0), line_beginning(2_int64), status)
        else
            ! Replace "total" with "total used in directory" to avoid
            ! confusion.
            call replace_match("total used in directory", status, subexp=1)
        end if

        if (.not. status%ok()) return
        call get_free_disk_space(file, available, found, status)
        if (.not. status%ok()) return

        if (found) then
            select case (dired_free_space%choice)
            case (DIRED_FREE_SPACE_SEPARATE)
                call end_of_line()
                call insert(" available "//available, status)
                ! The separate free-space line is considered part of the
                ! directory content, for the purposes of
                ! `dired_hide_details_mode'.
                call beginning_of_line()
                position = point()
            case (DIRED_FREE_SPACE_FIRST)
                call goto_character(beginning)

                if (is_looking_at(" */")) then
                    call end_of_line()

                    if (character_before_point() == ":") then
                        call put_text_property(point() - 1, point(), value_type(name="display"), &
                                               value_type(text=": ("//available//" available)"), status)
                    end if
                end if

                call fwd_line(1_int64)
                position = point()
            case default
                position = beginning
            end select
        end if

        call goto_character(original_point)
    end function dired_insert_disk_space

    !> Add various text properties to the lines in the region, from BEGINNING to
    !> END.  Port of dired.el:dired-insert-set-properties (no
    !> `dired_hide_details_preserved_columns', click-to-select, mouse
    !> dragging or `dired_filename_display_length').
    subroutine dired_insert_set_properties(beginning, end)
        integer(int64), intent(in) :: beginning, end
        type(handle_type) :: end_marker
        type(status_type) :: st
        type(value_type) :: properties
        integer(int64) :: original_point, fbeg, fend, position, b
        logical :: changed

        original_point = point()
        call make_marker(end_marker)
        call set_marker(end_marker, end, st)
        call goto_character(beginning)

        do while (point() < marked_position(end_marker))
            ! (ignore-errors ...)
            position = dired_move_to_filename(st)

            if (position == 0) then
                if (.not. (is_looking_at("^$") .or. is_looking_at(dired_subdirectory_regular_expression))) then
                    call put_text_property(line_beginning(), line_end() + 1, value_type(name="invisible"), &
                                           value_type(name="dired_hide_details_information"), st)
                end if
            else
                fend = point() - 1
                b = line_beginning() + 1
                call put_text_property(b, fend, value_type(name="invisible"), value_type(name="dired_hide_details_detail"), st)
                fbeg = point()
                position = dired_move_to_end_of_filename(st)

                if (st%ok()) then
                    fend = position - 1
                    call goto_character(fbeg)
                    properties = list(value_type(name="mouse_face"), value_type(name="highlight"), &
                                      value_type(name="dired_filename"), &
                                      value_type(.true.), &
                                      value_type(name="help_echo"), value_type(text="mouse-2: visit this file in other window"))
                    call add_text_properties(fbeg, fend + 1, properties, changed, st)

                    if (fend + 5 < line_end()) then
                        call put_text_property(fend + 5, line_end(), value_type(name="invisible"), &
                                               value_type(name="dired_hide_details_link"), st)
                    end if
                end if
            end if

            call fwd_line(1_int64)
        end do

        call destroy_marker_if_live(end_marker)
        call goto_character(original_point)
    end subroutine dired_insert_set_properties

    !> Align the fields of a file to the ones of surrounding lines.
    !> BEGINNING..END is the line where the file info is located.  Port of
    !> dired.el:dired-align-file (the spaces do not inherit text
    !> properties).
    subroutine dired_align_file(beginning, end)
        integer(int64), intent(in) :: beginning, end
        integer(int64) :: original_point, file, file_column, other, other_column, alt_column, curcol, align_point_offset, spaces, &
                          shortage, reached, moved, filepos
        type(handle_type) :: file_marker, other_marker
        type(status_type) :: st
        character(len=:), allocatable :: regular_expression
        logical :: align_count, has_offset

        original_point = point()
        ! Check that there is indeed a file, and that there is another
        ! adjacent file with which to align, and that additional spaces are
        ! needed to align the filenames.
        call goto_character(beginning)
        file = dired_move_to_filename(st, line_end_in=end)

        if (file == 0) then
            call goto_character(original_point)
            return
        end if

        file_column = current_column()
        other = 0
        call goto_character(beginning)
        shortage = forward_line(-1_int64)
        if (shortage == 0) other = dired_move_to_filename(st)

        if (other == 0) then
            call goto_character(beginning)
            shortage = forward_line(1_int64)
            if (shortage == 0) other = dired_move_to_filename(st)
        end if

        if (other == 0) then
            call goto_character(original_point)
            return
        end if

        other_column = current_column()

        if (other == file .or. other_column <= file_column) then
            call goto_character(original_point)
            return
        end if

        ! If we've only looked at the line above, check to see if the line
        ! below exists as well and if so, align with the shorter one.
        if (other < file) then
            call goto_character(beginning)
            shortage = forward_line(1_int64)

            if (shortage == 0) then
                if (dired_move_to_filename(st) /= 0) then
                    alt_column = current_column()

                    if (alt_column < other_column) then
                        other_column = alt_column
                        other = point()
                    end if
                end if
            end if
        end if

        ! Keep positions up-to-date when we insert stuff.
        call make_marker(other_marker)
        call set_marker(other_marker, other, st)
        call make_marker(file_marker)
        call set_marker(file_marker, file, st)
        ! Main loop.
        call goto_character(beginning)
        ! Skip to the first field.
        moved = skip_characters_forward(" ", st)

        do while (other_column > file_column .and. marked_position(file_marker) > point())
            ! We're now just in front of a field, with a space behind us.
            curcol = current_column()
            ! Nums are right-aligned.
            align_count = is_looking_at("[0-9]")
            ! Let's find the distance to the alignment-point (either the
            ! beginning or the end of the next field, depending on whether
            ! this field is left or right aligned).  It is never TRT to
            ! realign the first column of file's data.
            has_offset = .true.
            align_point_offset = 0

            if (curcol /= 0) then
                filepos = point()
                call goto_character(marked_position(other_marker))
                call move_to_column(curcol, reached)

                if (character_before_point() == " ") then
                    regular_expression = " *"
                else
                    regular_expression = "[^ ]* *"
                end if

                if (align_count) regular_expression = regular_expression//"[0-9][^ ]*"
                has_offset = looking_at(regular_expression, st)
                if (has_offset) align_point_offset = mend(0) - mbeg(0)
                call goto_character(filepos)
            end if

            ! Now, the number of spaces to insert is align-pt-offset minus
            ! the distance to the equivalent point on the current line.
            spaces = 0

            if (has_offset) then
                if (.not. align_count) then
                    spaces = align_point_offset
                else
                    filepos = point()
                    moved = skip_characters_forward("^ ", st)
                    spaces = align_point_offset - (current_column() - curcol)
                    call goto_character(filepos)
                end if
            end if

            if (has_offset .and. spaces > 0) then
                file_column = spaces + file_column
                if (file_column > other_column) spaces = spaces - (file_column - other_column)
                call insert_character_n(32, spaces, st)
                ! Let's just make really sure we did not mess up.
                filepos = point()

                if (dired_move_to_filename(st) /= marked_position(file_marker)) then
                    ! Damn!  We messed up: let's revert the change.
                    call goto_character(filepos)
                    call delete_character(-spaces, st)
                else
                    call goto_character(filepos)
                end if
            end if

            ! Now skip to next field.
            moved = skip_characters_forward("^ ", st)
            moved = skip_characters_forward(" ", st)
        end do

        call destroy_marker_if_live(file_marker)
        call destroy_marker_if_live(other_marker)
        call goto_character(original_point)
    end subroutine dired_align_file

    !> Read in a new Dired buffer.
    !> Differs from `dired_insert_subdirectory' in that it accepts
    !> wildcards, erases the buffer, and builds the subdir-alist anew
    !> (including making it buffer-local and clearing it first).  Port of
    !> dired.el:dired-readin (no `combine_change_calls').
    recursive subroutine dired_readin(status, run_after_hook)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: run_after_hook
        character(len=:), allocatable :: directory_name
        type(value_type) :: saved_ro
        type(file_status_type) :: attributes
        type(status_type) :: st
        integer(int64) :: original_point
        logical :: found, saved_undo, after

        after = .true.
        if (present(run_after_hook)) after = run_after_hook
        status = status_type()
        ! default-directory and dired-actual-switches must be buffer-local
        ! and initialized by now.
        directory_name = expand(dired_directory_of())
        original_point = point()
        ! This hook which may want to modify dired-actual-switches based on
        ! dired-directory.
        call dired_before_readin_hook%execute(status)
        if (.not. status%ok()) return
        if (buffers(current)%undo_enabled) buffers(current)%undo_count = 0
        call widen()
        call inhibit_read_only_begin(saved_ro)
        ! Don't make undo entries for readin.
        saved_undo = buffers(current)%undo_enabled
        buffers(current)%undo_enabled = .false.
        call erase_buffer(status)
        if (status%ok()) call dired_readin_insert(status)
        buffers(current)%undo_enabled = saved_undo
        call inhibit_read_only_end(saved_ro)
        if (.not. status%ok()) return
        call goto_character(point_minimum())
        call dired_build_subdirectories(st)
        call file_attributes(directory_name, attributes, found)

        if (found) then
            if (iand(attributes%mode, int(o'170000', int32)) == int(o'040000', int32)) then
                call set_visited_file_modtime(time_type(attributes%mtime_second, int(attributes%mtime_nsec, int64), &
                                                        TIMESPEC_COUNT_RATE))
            end if
        end if

        call restore_buffer_modified(.false.)
        ! (save-excursion: the text was replaced, so point is at the start.)
        call goto_character(point_minimum())
        ! The hook can successfully use dired functions (e.g.
        ! dired-get-filename) as the subdir-alist has been built.
        if (after) call dired_after_readin_hook%execute(status)
    end subroutine dired_readin

    !> Insert listing for the specified dir already in dired-directory,
    !> assuming a clean buffer.  Port of dired.el:dired-readin-insert (no
    !> file list).
    recursive subroutine dired_readin_insert(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: directory, base
        type(handle_type) :: tmpbuf, old
        type(dired_subdirectory_type), allocatable :: saved_table(:)
        logical :: had_table
        type(status_type) :: st
        type(handle_type) :: beginning_of_accessible_portion_m, end_of_accessible_portion_m
        integer(int64) :: found, original_point
        integer :: k

        status = status_type()
        directory = expand(dired_directory_of())

        if (len(file_name_nondirectory(directory)) == 0) then
            ! If we are reading a whole single directory...
            call dired_insert_directory(directory, dired_actual_switches_of(), status, &
                                        wildcard=.not. is_file_directory(directory), hdr=.true.)
        else if (.not. (is_insert_directory_wildcard_in_directory(directory) .or. &
                        is_file_readable(directory_file_name(file_name_directory(directory))))) then
            call set_error(status, GENERIC_ERROR_CODE, "Directory "//directory//" inaccessible or nonexistent")
        else
            ! Else treat it as a wildcard spec.
            call dired_insert_directory(directory, dired_actual_switches_of(), status, wildcard=.true., hdr=.true.)
        end if

        if (.not. status%ok()) return
        ! Every time `temporary_file_directory' is (re)displayed in Dired a
        ! new `ls' error file is created and the Dired buffer has an entry
        ! for it.  The file itself is deleted in `insert_directory' but its
        ! Dired entry remains, so we remove it here.
        tmpbuf = dired_find_buffer_nocreate(temporary_file_directory())
        if (tmpbuf%index == 0 .or. .not. allocated(dired_ls_error_file)) return
        old = current_buffer()
        call set_buffer(tmpbuf, st)
        original_point = point()
        call make_marker(beginning_of_accessible_portion_m)
        call set_marker(beginning_of_accessible_portion_m, buffers(current)%beginning_of_accessible_portion, st)
        call make_marker(end_of_accessible_portion_m)
        call set_marker(end_of_accessible_portion_m, buffers(current)%end_of_accessible_portion, st)
        call set_marker_insertion_type(end_of_accessible_portion_m, .true., st)
        call widen()
        call goto_character(point_minimum())
        base = file_name_base(dired_ls_error_file)
        call search_forward(base, found, st, noerror=SEARCH_NOERROR)

        if (found > 0) then
            ! The call chain of `dired_remove_entry' requires an allocated
            ! `dired_subdirectories', so we set it.
            call make_local_entry(k)
            had_table = allocated(locals(k)%subdirectory_table)

            if (had_table) then
                saved_table = locals(k)%subdirectory_table
                deallocate (locals(k)%subdirectory_table)
            end if

            allocate (locals(k)%subdirectory_table(1))
            locals(k)%subdirectory_table(1)%directory = temporary_file_directory()
            call make_marker(locals(k)%subdirectory_table(1)%marker)
            call set_marker(locals(k)%subdirectory_table(1)%marker, point_minimum(), st)
            call dired_remove_entry(dired_ls_error_file)
            call destroy_table(locals(k)%subdirectory_table)
            if (had_table) locals(k)%subdirectory_table = saved_table
        end if

        call narrow_to_region(marked_position(beginning_of_accessible_portion_m), marked_position(end_of_accessible_portion_m), st)
        call destroy_marker_if_live(beginning_of_accessible_portion_m)
        call destroy_marker_if_live(end_of_accessible_portion_m)
        call goto_character(original_point)
        call set_buffer(old, st)

    contains

        !> FILENAME without directory and extension (file-name-base).
        function file_name_base(filename) result(b)
            character(len=*), intent(in) :: filename
            character(len=:), allocatable :: b
            integer :: k2

            b = file_name_nondirectory(filename)
            k2 = index(b, ".", back=.true.)
            if (k2 > 1) b = b(1:k2 - 1)
        end function file_name_base
    end subroutine dired_readin_insert

    ! ------------------------------------------------------------------
    ! The subdirectory table
    ! ------------------------------------------------------------------

    !> Build `dired_subdirectories' by parsing the buffer.
    !> If optional arg SWITCHES is present, use its value instead of
    !> `dired_actual_switches'.  Port of dired.el:dired-build-subdir-alist.
    recursive subroutine dired_build_subdirectories(status, switches, called_interactively)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: switches
        logical, intent(in), optional :: called_interactively
        character(len=:), allocatable :: sw, new_directory_name, temporary, res
        type(value_type) :: saved_ro
        type(handle_type) :: m
        type(dired_subdirectory_type) :: subdirectory
        integer(int64) :: original_point, count, b1, e1, header_beginning_of_line, after
        logical :: saved_undo, file_entry, escape
        integer :: k, i
        character(len=48) :: text

        status = status_type()
        call make_local_entry(k)
        call destroy_table(locals(k)%subdirectory_table)
        allocate (locals(k)%subdirectory_table(0))
        original_point = point()
        call inhibit_read_only_begin(saved_ro)
        saved_undo = buffers(current)%undo_enabled
        buffers(current)%undo_enabled = .false.

        if (present(switches)) then
            sw = switches
        else
            sw = dired_actual_switches_of()
        end if

        count = 0
        call goto_character(point_minimum())

        do while (is_regular_expression_search_forward(dired_subdirectory_regular_expression))
            after = point()
            b1 = mbeg(1)
            e1 = mend(1)
            ! Avoid taking a file name ending in a colon as a subdir name.
            call goto_character(mbeg(0))
            call beginning_of_line()
            header_beginning_of_line = point()
            file_entry = is_looking_at(dired_regular_expression_maybe_mark//dired_regular_expression_inode_size &
                                       //dired_regular_expression_permissions)

            if (.not. file_entry) then
                call buffer_substring(b1, e1, new_directory_name, status)
                new_directory_name = expand(new_directory_name)
                call goto_character(b1)
                call delete_region(b1, e1, status)
                call insert(new_directory_name, status)
                after = after + (count_characters(new_directory_name) - (e1 - b1))
                count = count + 1

                ! Undo any escaping of newlines and \ by
                ! dired-insert-directory.  Convert "n" preceded by odd number
                ! of \ to newline, and \\ to \.
                if (is_dired_switches_escape(sw) .and. index(new_directory_name, "\") > 0) then
                    res = ""
                    escape = .false.

                    do i = 1, len(new_directory_name)
                        if (new_directory_name(i:i) == "\") then
                            if (escape) then
                                res = res//"\"
                                escape = .false.
                            else
                                escape = .true.
                            end if
                        else if (escape .and. new_directory_name(i:i) == "n") then
                            res = res//LF
                            escape = .false.
                        else
                            temporary = ""
                            if (escape) temporary = "\"
                            res = res//temporary//new_directory_name(i:i)
                            escape = .false.
                        end if
                    end do

                    new_directory_name = res
                end if

                ! dired-alist-add-1: place a sub directory boundary between
                ! lines.
                call make_marker(m)
                call set_marker(m, header_beginning_of_line, status)
                subdirectory%directory = dired_normalize_subdirectory(new_directory_name)
                subdirectory%marker = m
                locals(k)%subdirectory_table = [subdirectory, locals(k)%subdirectory_table]
            end if

            call goto_character(after)
        end do

        buffers(current)%undo_enabled = saved_undo
        call inhibit_read_only_end(saved_ro)
        call goto_character(original_point)

        if (count > 1 .and. present(called_interactively)) then
            if (called_interactively) then
                write (text, '("Buffer includes ", i0, " directories")') count
                call message(trim(text))
            end if
        end if
    end subroutine dired_build_subdirectories

    !> DIRECTORY as an absolute directory name (relative names are relative to
    !> `default_directory').  Port of dired.el:dired-normalize-subdir.
    function dired_normalize_subdirectory(directory) result(norm)
        character(len=*), intent(in) :: directory
        character(len=:), allocatable :: norm

        if (is_file_name_absolute(directory)) then
            norm = file_name_as_directory(directory)
        else
            norm = file_name_as_directory(expand(directory, default_directory()))
        end if
    end function dired_normalize_subdirectory

    !> Return the name of the subdirectory to which this line belongs.
    !> This returns a string with trailing slash, like `default_directory'.
    !> With LOCALP, return a file name relative to `default_directory',
    !> in which case the value could be an empty string if
    !> `default_directory' is the directory where the file on this line
    !> resides.  Port of dired.el:dired-current-directory.
    function dired_current_directory(status, localp) result(directory)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: localp
        character(len=:), allocatable :: directory
        integer :: k, i
        integer(int64) :: here

        status = status_type()
        directory = ""
        k = local_index()

        if (k == 0 .or. table_size() == 0) then
            ! probably because called in a non-dired buffer
            call set_error(status, GENERIC_ERROR_CODE, "No subdirectory_table in #<buffer "//buffers(current)%name//">")
            return
        end if

        here = point()

        do i = 1, size(locals(k)%subdirectory_table)
            directory = locals(k)%subdirectory_table(i)%directory
            ! use `<=' (not `<') as subdir line is part of subdir
            if (marked_position(locals(k)%subdirectory_table(i)%marker) <= here) exit
        end do

        if (present(localp)) then
            if (localp) directory = dired_make_relative(directory, default_directory())
        end if
    end function dired_current_directory

    !> Convert FILE (an absolute file name) to a name relative to DIRECTORY.
    !> If DIRECTORY is omitted, it defaults to `default_directory'.
    !> If FILE is not in the directory tree of DIRECTORY, return FILE
    !> unchanged.  Port of dired.el:dired-make-relative.
    function dired_make_relative(file, directory_in) result(relative)
        character(len=*), intent(in) :: file
        character(len=*), intent(in), optional :: directory_in
        character(len=:), allocatable :: relative, directory
        type(status_type) :: st
        integer(int64) :: e

        if (present(directory_in)) then
            directory = directory_in
        else
            directory = default_directory()
        end if

        ! This case comes into play if default-directory is set to use ~.
        if (string_matches("\(\`\|:\)~", directory)) directory = expand(directory)
        relative = file

        if (string_match("^"//regular_expression_quote(directory), file, st) > 0) then
            e = mend(0)
            relative = file(string_index_to_offset(file, e) + 1:)
        end if
    end function dired_make_relative

    !> Return the subdir name on this line, or "" if not on a headerline.
    !> Port of dired.el:dired-get-subdir.
    function dired_get_subdirectory() result(directory)
        character(len=:), allocatable :: directory
        character(len=:), allocatable :: current_directory
        type(status_type) :: st
        integer :: k, i

        directory = ""
        current_directory = dired_current_directory(st)
        if (.not. st%ok()) return
        k = local_index()

        do i = 1, size(locals(k)%subdirectory_table)
            if (locals(k)%subdirectory_table(i)%directory == current_directory) then
                ! table stores b-o-l positions
                if (line_beginning() == marked_position(locals(k)%subdirectory_table(i)%marker)) directory = current_directory
                return
            end if
        end do
    end function dired_get_subdirectory

    !> The index of DIRECTORY in `dired_subdirectories' (0-based, as nth wants
    !> it), or -1.  Port of dired.el:dired-subdir-index.
    integer function dired_subdirectory_index(directory) result(index)
        character(len=*), intent(in) :: directory
        integer :: k, i

        index = -1
        k = local_index()
        if (k == 0) return

        do i = 1, size(locals(k)%subdirectory_table)
            if (locals(k)%subdirectory_table(i)%directory == directory) then
                index = i - 1
                return
            end if
        end do
    end function dired_subdirectory_index

    !> Go to next subdirectory, regardless of level.  Return the position
    !> (0 if not found and NO_ERROR_IF_NOT_FOUND).  Port of
    !> dired.el:dired-next-subdir.
    function dired_next_subdirectory(argument, status, no_error_if_not_found, no_skip) result(position)
        integer(int64), intent(in) :: argument
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_error_if_not_found, no_skip
        integer(int64) :: position
        character(len=:), allocatable :: this_directory
        integer :: k, index
        logical :: noerr, skip

        position = 0
        noerr = .false.
        if (present(no_error_if_not_found)) noerr = no_error_if_not_found
        skip = .true.
        if (present(no_skip)) skip = .not. no_skip
        this_directory = dired_current_directory(status)
        if (.not. status%ok()) return
        k = local_index()
        index = dired_subdirectory_index(this_directory) - int(argument)

        if (index >= 0 .and. index < size(locals(k)%subdirectory_table)) then
            call goto_character(marked_position(locals(k)%subdirectory_table(index + 1)%marker))
            if (skip) call end_of_line()
            position = point()
        else if (.not. noerr) then
            if (argument > 0) then
                call set_error(status, GENERIC_ERROR_CODE, "Last directory")
            else
                call set_error(status, GENERIC_ERROR_CODE, "First directory")
            end if
        end if
    end function dired_next_subdirectory

    !> Where the current subdirectory ends.  Port of
    !> dired.el:dired-subdir-max.
    integer(int64) function dired_subdirectory_maximum() result(position)
        integer(int64) :: original_point
        type(status_type) :: st

        original_point = point()
        position = point_maximum()

        if (table_size() > 1) then
            if (dired_next_subdirectory(1_int64, st, .true., .true.) /= 0) position = point()
        end if

        call goto_character(original_point)
    end function dired_subdirectory_maximum

    ! ------------------------------------------------------------------
    ! File names in the buffer
    ! ------------------------------------------------------------------

    !> Move to the beginning of the filename on the current line.
    !> Return the position of the beginning of the filename, or 0 if none
    !> found.
    !>
    !> If RAISE_ERROR, an error condition occurs if we can't find the filename on
    !> the current line.
    !>
    !> If LINE_END_IN is present, it is a position to use instead of
    !> `line_end_position' as the end of the line.  Port of
    !> dired.el:dired-move-to-filename.
    integer(int64) function dired_move_to_filename(status, raise_error, line_end_in) result(position)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: raise_error
        integer(int64), intent(in), optional :: line_end_in
        integer(int64) :: e, change
        logical :: raise, found

        status = status_type()
        raise = .false.
        if (present(raise_error)) raise = raise_error

        if (present(line_end_in)) then
            e = line_end_in
        else
            e = line_end()
        end if

        position = 0
        call beginning_of_line()
        ! First try assuming `ls --dired' was used.
        call next_single_property_change(point(), value_type(name="dired_filename"), change, found, status, limit=e)
        if (.not. status%ok()) return

        if (found .and. change < e) then
            call goto_character(change)
            position = change
        else if (is_regular_expression_search_forward(directory_listing_regular_expression(), e)) then
            call goto_character(mend(0))
            position = point()
        else if (is_regular_expression_search_forward(dired_permission_flags_regular_expression, e)) then
            ! Ha!  There *is* a file.  Our regexp-from-hell just failed to
            ! find it.
            if (raise) then
                call set_error(status, GENERIC_ERROR_CODE, &
                               "Unrecognized line!  Check directory_listing_before_filename_regular_expression")
                return
            end if

            call beginning_of_line()
        else if (raise) then
            call set_error(status, GENERIC_ERROR_CODE, "No file on this line")
        end if
    end function dired_move_to_filename

    !> `directory_listing_before_filename_regular_expression' (files.el).  Not a port.
    function directory_listing_regular_expression() result(regular_expression)
        character(len=:), allocatable :: regular_expression

        regular_expression = directory_listing_before_filename_regular_expression
    end function directory_listing_regular_expression

    !> Move to the end of the file name, with point at its beginning
    !> (after `dired_move_to_filename').  Return the position, or 0 if
    !> point did not move; an error then unless NO_ERROR.  Port of
    !> dired.el:dired-move-to-end-of-filename.
    integer(int64) function dired_move_to_end_of_filename(status, no_error) result(position)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_error
        type(value_type) :: v
        character(len=:), allocatable :: bits
        character :: file_type
        integer(int64) :: original_point, e, change, found_position, saved
        logical :: noerr, used_f, hidden, symlink, executable, found

        status = status_type()
        noerr = .false.
        if (present(no_error)) noerr = no_error
        position = 0
        call get_text_property(point(), value_type(name="dired_filename"), v, status)
        if (.not. status%ok()) return

        if (allocated(v%a)) then
            call next_single_property_change(point(), value_type(name="dired_filename"), change, found, status)
            ! No property change can happen on or before the last file name
            ! in the Dired listing when there is at least one prior file
            ! name containing a newline (bug#79528).
            if (.not. found) change = point_maximum() - 1
            call goto_character(change)
            position = change
            return
        end if

        original_point = point()
        used_f = dired_check_switches(dired_actual_switches_of(), "F", "classify")
        e = line_end()
        hidden = is_dired_hidden()
        file_type = " "
        symlink = .false.
        executable = .false.

        if (.not. hidden) then
            saved = point()

            ! Find out what kind of file this is: restrict perm bits to be
            ! non-blank, otherwise this matches one char to early (looking
            ! backward).
            if (is_regular_expression_search_backward(dired_permission_flags_regular_expression)) then
                file_type = character_at(mbeg(1))
                symlink = file_type == "l"

                ! Only with -F we need to know whether it's an executable.
                if (used_f) then
                    call buffer_substring(mbeg(2), mend(2), bits, status)
                    executable = scan(bits, "xst") > 0
                    call buffer_substring(mbeg(3), mend(3), bits, status)
                    executable = executable .or. scan(bits, "xst") > 0
                    call buffer_substring(mbeg(4), mend(4), bits, status)
                    executable = executable .or. scan(bits, "xst") > 0
                end if
            else if (.not. noerr) then
                call goto_character(saved)
                call set_error(status, GENERIC_ERROR_CODE, "No file on this line")
                return
            end if

            call goto_character(saved)

            ! Move point to end of name:
            if (symlink) then
                call search_forward(" -> ", found_position, status, bound=e, noerror=SEARCH_NOERROR)

                if (found_position > 0) then
                    call goto_character(point() - 4)

                    ! did ls really mark the link?
                    if (used_f .and. dired_ls_f_marks_symlinks .and. character_before_point() == "@") then
                        call goto_character(point() - 1)
                    end if
                end if
            else
                ! else not a symbolic link
                call goto_character(e)
                ! ls -lF marks dirs, sockets, fifos and executables with
                ! exactly one trailing character.
                if (used_f .and. (scan(file_type, "dsp") > 0 .or. executable)) call goto_character(point() - 1)
            end if
        end if

        if (.not. noerr .and. original_point == point()) then
            if (hidden) then
                call set_error(status, GENERIC_ERROR_CODE, &
                               substitute_command_keys("File line is hidden, type \[dired_hide_subdirectory] to unhide"))
            else
                call set_error(status, GENERIC_ERROR_CODE, "No file on this line")
            end if

            return
        end if

        if (original_point /= point()) position = point()
    end function dired_move_to_end_of_filename

    !> Whether the character at point is hidden (`invisible' is `dired').
    !> Port of dired.el:dired--hidden-p (text properties only).
    logical function is_dired_hidden(position)
        integer(int64), intent(in), optional :: position
        type(value_type) :: v
        type(status_type) :: st
        integer(int64) :: p

        p = point()
        if (present(position)) p = position
        is_dired_hidden = .false.
        if (p >= point_maximum()) return
        call get_text_property(p, value_type(name="invisible"), v, st)
        if (st%ok()) is_dired_hidden = value_eq(v, value_type(name="dired"))
    end function is_dired_hidden

    !> In Dired, return name of file mentioned on this line.
    !> Value returned normally includes the directory name.
    !> LOCALP with value DIRED_LOCALP_NO_DIRECTORY means don't include directory
    !> name in result.  A value of DIRED_LOCALP_VERBATIM means to return
    !> the name exactly as it occurs in the buffer, and DIRED_LOCALP_RELATIVE
    !> means construct name relative to `default_directory', which still
    !> may contain slashes if in a subdirectory.
    !> NO_ERROR_IF_NOT_FILEP means treat `.' and `..' as regular filenames
    !> and return no name (FOUND false) if no filename on this line.
    !> Otherwise, an error occurs in these cases.  Port of
    !> dired.el:dired-get-filename.
    recursive subroutine dired_get_filename(file, found, status, localp, no_error_if_not_filep)
        character(len=:), allocatable, intent(out) :: file
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: localp
        logical, intent(in), optional :: no_error_if_not_filep
        character(len=:), allocatable :: raw, quoted, rep, directory
        integer(int64) :: original_point, p1, p2, next, start, b
        integer :: lp, i, shift
        logical :: noerr, saved_fold, already_absolute, verbatim
        type(status_type) :: st

        lp = DIRED_LOCALP_ABSOLUTE
        if (present(localp)) lp = localp
        noerr = .false.
        if (present(no_error_if_not_filep)) noerr = no_error_if_not_filep
        status = status_type()
        found = .false.
        file = ""
        ! (Hidden subdirectories are deferred: no subdirectory is hidden.)
        saved_fold = case_fold_search%value
        case_fold_search%value = .false.
        original_point = point()
        p2 = 0
        p1 = dired_move_to_filename(status,.not. noerr)
        if (status%ok() .and. p1 /= 0) p2 = dired_move_to_end_of_filename(status, noerr)
        call goto_character(original_point)

        if (.not. status%ok()) then
            case_fold_search%value = saved_fold
            return
        end if

        ! nil if no file on this line, but no-error-if-not-filep is t:
        if (p1 /= 0 .and. p2 /= 0) then
            call buffer_substring(p1, p2, raw, status)
            ! Unquote names quoted by ls or by dired-insert-directory.
            quoted = ""

            do i = 1, len(raw)
                if (raw(i:i) == '"') then
                    if (i == 1) then
                        quoted = quoted//"\"
                    else if (raw(i - 1:i - 1) /= "\") then
                        quoted = quoted//"\"
                    end if
                end if

                quoted = quoted//raw(i:i)
            end do

            ! Unescape any spaces escaped by ls -b (bug#10469).
            if (is_dired_switches_escape(dired_actual_switches_of())) then
                verbatim = lp == DIRED_LOCALP_VERBATIM
                rep = ""
                shift = -1

                if (verbatim) then
                    rep = "\\"
                    shift = 1
                end if

                start = 1

                do
                    b = string_match("\(\\\) ", quoted, st, start)
                    if (b == 0) exit
                    quoted = quoted(1:string_index_to_offset(quoted, mbeg(1)))//rep// &
                             quoted(string_index_to_offset(quoted, mend(1)) + 1:)
                    start = shift + mend(0)
                end do
            end if

            ! Hence we don't need to worry about converting `\\' back to `\'.
            call read_string_literal(quoted//'"', 1_int64, file, next, status)

            if (.not. status%ok()) then
                case_fold_search%value = saved_fold
                return
            end if

            found = .true.
        end if

        case_fold_search%value = saved_fold
        if (.not. found) return
        already_absolute = index(file, "/") == 1

        if (lp == DIRED_LOCALP_VERBATIM) then
            return
        else if (.not. noerr .and. (file == "." .or. file == "..")) then
            call set_error(status, GENERIC_ERROR_CODE, format_message("Cannot operate on `.' or `..'"))
            found = .false.
        else if (lp == DIRED_LOCALP_NO_DIRECTORY .and. already_absolute) then
            file = file_name_nondirectory(file)
        else if (already_absolute) then
            return
        else if (lp == DIRED_LOCALP_NO_DIRECTORY) then
            return
        else
            directory = dired_current_directory(status, lp == DIRED_LOCALP_RELATIVE)

            if (.not. status%ok()) then
                found = .false.
                return
            end if

            file = directory//file
        end if
    end subroutine dired_get_filename

    !> Try to get a file name at point in the current Dired buffer.
    !> It returns an abbreviated name that can't be used as an argument
    !> to `dired_goto_file'.  FOUND is false for none.  Port of
    !> dired.el:dired-file-name-at-point.
    subroutine dired_file_name_at_point(name, found)
        character(len=:), allocatable, intent(out) :: name
        logical, intent(out) :: found
        character(len=:), allocatable :: filename
        type(status_type) :: st

        name = ""
        call dired_get_filename(filename, found, st, no_error_if_not_filep=.true.)
        if (.not. found) return

        if (is_file_directory(filename)) then
            name = file_name_as_directory(abbreviate_file_name(filename))
        else
            name = abbreviate_file_name(filename)
        end if
    end subroutine dired_file_name_at_point

    !> True if there is no file name on this line.  Port of
    !> dired.el:dired-between-files.
    logical function dired_between_files()
        integer(int64) :: original_point
        type(status_type) :: st

        original_point = point()
        dired_between_files = dired_move_to_filename(st) == 0
        call goto_character(original_point)
    end function dired_between_files

    ! ------------------------------------------------------------------
    ! Dired buffers
    ! ------------------------------------------------------------------

    !> Is FILE part of the directory tree starting at DIRECTORY?  Port of
    !> dired.el:dired-in-this-tree-p.
    logical function is_dired_in_this_tree(file, directory)
        character(len=*), intent(in) :: file, directory
        logical :: saved

        saved = case_fold_search%value
        case_fold_search%value = .false.
        is_dired_in_this_tree = string_matches("^"//regular_expression_quote(directory), file)
        case_fold_search%value = saved
    end function is_dired_in_this_tree

    !> Convert glob-pattern PATTERN to a regular expression.  Port of
    !> dired.el:dired-glob-regexp.
    function dired_glob_regular_expression(pattern) result(regular_expression)
        character(len=*), intent(in) :: pattern
        character(len=:), allocatable :: regular_expression
        integer :: matched, k, set_start, set_cont, set_end

        regular_expression = ""
        matched = 0

        do
            k = scan(pattern(matched + 1:), "[?*")
            if (k == 0) exit
            k = matched + k
            regular_expression = regular_expression//regular_expression_quote(pattern(matched + 1:k - 1))

            select case (pattern(k:k))
            case ("?")
                regular_expression = regular_expression//"."
                matched = k
            case ("[")
                ! Fails to handle ^ yet ????
                set_start = k
                set_cont = set_start + 2

                if (set_start + 1 <= len(pattern)) then
                    if (pattern(set_start + 1:set_start + 1) == "^") set_cont = set_start + 3
                end if

                set_end = index(pattern(set_cont:), "]")

                if (set_end == 0) then
                    matched = len(pattern)
                    exit
                end if

                set_end = set_cont + set_end - 1
                regular_expression = regular_expression//pattern(set_start:set_end)
                matched = set_end
            case default
                regular_expression = regular_expression//".*"
                matched = k
            end select
        end do

        regular_expression = "\`"//regular_expression//regular_expression_quote(pattern(matched + 1:))//"\'"
    end function dired_glob_regular_expression

    !> Return a list of buffers for DIRECTORY (top level or in-situ subdir).
    !> If FILE is present, include only those whose wildcard pattern (if
    !> any) matches FILE.  The list is in reverse order of buffer creation,
    !> most recent last.  As a side effect, killed Dired buffers for DIRECTORY
    !> are removed from `dired_buffers'.  Port of
    !> dired.el:dired-buffers-for-dir.
    function dired_buffers_for_directory(directory_in, file) result(result)
        character(len=*), intent(in) :: directory_in
        character(len=*), intent(in), optional :: file
        type(handle_type), allocatable :: result(:)
        character(len=:), allocatable :: directory, wildcards
        type(handle_type) :: old, buffer
        type(status_type) :: st
        logical :: ok
        integer :: i, k

        allocate (result(0))
        call prune_dired_buffers()
        directory = file_name_as_directory(expand(directory_in))
        old = current_buffer()

        do i = 1, size(dired_buffers)
            buffer = dired_buffers(i)%buffer
            if (.not. is_dired_in_this_tree(directory, dired_buffers(i)%directory)) cycle
            call set_buffer(buffer, st)
            ok = .false.
            k = local_index()

            if (k > 0) then
                if (dired_subdirectory_index(directory) >= 0) then
                    if (.not. present(file)) then
                        ok = .true.
                    else
                        wildcards = file_name_nondirectory(dired_directory_of())
                        ok = len(wildcards) == 0
                        if (.not. ok) ok = string_matches(dired_glob_regular_expression(wildcards), file)
                    end if
                end if
            end if

            if (ok) result = [buffer, result]
        end do

        call set_buffer(old, st)
    end function dired_buffers_for_directory

    !> Remove the closed buffers from `dired_buffers'.  Not a port.
    subroutine prune_dired_buffers()
        type(dired_buffer_entry_type), allocatable :: kept(:)
        integer :: i, n

        if (.not. allocated(dired_buffers)) allocate (dired_buffers(0))
        n = 0

        do i = 1, size(dired_buffers)
            if (is_buffer_live(dired_buffers(i)%buffer)) n = n + 1
        end do

        if (n == size(dired_buffers)) return
        allocate (kept(n))
        n = 0

        do i = 1, size(dired_buffers)
            if (.not. is_buffer_live(dired_buffers(i)%buffer)) cycle
            n = n + 1
            kept(n) = dired_buffers(i)
        end do

        call move_alloc(kept, dired_buffers)
    end subroutine prune_dired_buffers

    !> Advertise in variable `dired_buffers' that we dired
    !> `default_directory'.  Port of dired.el:dired-advertise.
    subroutine dired_advertise()
        character(len=:), allocatable :: expanded_default
        type(handle_type), allocatable :: buffers(:)
        type(dired_buffer_entry_type) :: entry
        type(handle_type) :: me
        integer :: i

        expanded_default = expand(default_directory())
        me = current_buffer()
        buffers = dired_buffers_for_directory(expanded_default)

        do i = 1, size(buffers)
            ! we have already advertised ourselves
            if (same_window(buffers(i), me)) return
        end do

        entry%directory = expanded_default
        entry%buffer = me
        dired_buffers = [entry, dired_buffers]
    end subroutine dired_advertise

    !> Find the Dired buffer for DIRECTORY_NAME (the null handle for none).
    !> Port of dired.el:dired-find-buffer-nocreate (no dired-x; MODE
    !> `dired_mode').
    function dired_find_buffer_nocreate(directory_name_in) result(found)
        character(len=*), intent(in) :: directory_name_in
        type(handle_type) :: found
        character(len=:), allocatable :: directory_name, dd
        type(handle_type) :: old
        type(status_type) :: st
        integer :: i

        found = null_handle()
        call prune_dired_buffers()
        directory_name = expand(directory_name_in)
        old = current_buffer()

        do i = 1, size(dired_buffers)
            call set_buffer(dired_buffers(i)%buffer, st)
            if (.not. st%ok()) cycle
            if (.not. allocated(buffers(current)%major_mode)) cycle
            if (buffers(current)%major_mode /= "dired_mode") cycle
            dd = dired_directory_of()
            ! dired-directory is nil during find-alternate-file
            if (len(dd) == 0) cycle

            if (directory_name == expand(dd)) then
                found = dired_buffers(i)%buffer
                exit
            end if
        end do

        call set_buffer(old, st)
    end function dired_find_buffer_nocreate

    ! ------------------------------------------------------------------
    ! The dired command
    ! ------------------------------------------------------------------

    !> Read the directory (and with a prefix argument the switches) for
    !> `dired': "Dired STRING(directory): ".  Port of
    !> dired.el:dired-read-dir-and-switches.
    recursive subroutine dired_read_directory_and_switches(string, directory_name, switches, has_switches, status)
        character(len=*), intent(in) :: string
        character(len=:), allocatable, intent(out) :: directory_name, switches
        logical, intent(out) :: has_switches
        type(status_type), intent(out) :: status

        has_switches = current_prefix_argument%kind /= PREFIX_NONE
        switches = ""

        if (has_switches) then
            call read_string("Dired listing switches: ", switches, status, initial_input=dired_listing_switches)
            if (.not. status%ok()) return
        end if

        call read_file_name("Dired "//string//"(directory): ", directory_name, status, default_filename=default_directory())
    end subroutine dired_read_directory_and_switches

    !> "Edit" directory DIRECTORY_NAME--delete, rename, print, etc. some files in it.
    !> SWITCHES specifies the options to be used when invoking
    !> `insert_directory_program', usually `ls', which produces the
    !> listing of the directory files and their attributes.
    !> Interactively, a prefix argument will cause the command to prompt
    !> for SWITCHES.
    !>
    !> Dired displays a list of files in DIRECTORY_NAME (which may also have shell
    !> wildcards appended to select certain files).
    !>
    !> \<dired_mode_map>You can flag files for deletion with \[dired_flag_file_deletion] and then
    !> delete them by typing \[dired_do_flagged_delete].
    !> Type \[describe_mode] after entering Dired for more info.
    !>
    !> If DIRECTORY_NAME is already in a Dired buffer, that buffer is used without
    !> refresh.  Port of dired.el:dired (DIRECTORY_NAME a string).
    recursive subroutine dired(directory_name, status, switches)
        character(len=*), intent(in) :: directory_name
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: switches
        type(handle_type) :: buffer

        call dired_noselect(directory_name, buffer, status, switches)
        if (.not. status%ok()) return
        if (buffer%index /= 0) call pop_to_buffer_same_window(buffer, status)
        if (.not. status%ok()) return
        call dired_display_ls_error()
    end subroutine dired

    !> "Edit" directory DIRECTORY_NAME.  Like `dired' but select in another window.
    !> If this command needs to split the current window, it by default obeys
    !> the user options `split_height_threshold' and `split_width_threshold',
    !> when it decides whether to split the window horizontally or vertically.
    !> Port of dired.el:dired-other-window.
    recursive subroutine dired_other_window(directory_name, status, switches)
        character(len=*), intent(in) :: directory_name
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: switches
        type(handle_type) :: buffer

        call dired_noselect(directory_name, buffer, status, switches)
        if (.not. status%ok()) return
        if (buffer%index /= 0) call switch_to_buffer_other_window(buffer, status)
        if (.not. status%ok()) return
        call dired_display_ls_error()
    end subroutine dired_other_window

    !> "Edit" directory DIRECTORY_NAME.  Like `dired' but make a new frame.
    !> Port of dired.el:dired-other-frame.
    recursive subroutine dired_other_frame(directory_name, status, switches)
        character(len=*), intent(in) :: directory_name
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: switches
        type(handle_type) :: buffer

        call dired_noselect(directory_name, buffer, status, switches)
        if (.not. status%ok()) return
        if (buffer%index /= 0) call switch_to_buffer_other_frame(buffer, status)
        if (.not. status%ok()) return
        call dired_display_ls_error()
    end subroutine dired_other_frame

    !> "Edit" directory DIRECTORY_NAME.  Like `dired' but make a new tab.
    !> Port of dired.el:dired-other-tab.
    recursive subroutine dired_other_tab(directory_name, status, switches)
        character(len=*), intent(in) :: directory_name
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: switches
        type(handle_type) :: buffer

        call dired_noselect(directory_name, buffer, status, switches)
        if (.not. status%ok()) return
        if (buffer%index /= 0) call switch_to_buffer_other_tab(buffer, status)
        if (.not. status%ok()) return
        call dired_display_ls_error()
    end subroutine dired_other_tab

    !> "Edit" directory DIRECTORY_NAME--delete, rename, print, etc. some files in it.
    !> Optional second argument SWITCHES specifies the options to be used
    !> when invoking `insert_directory_program', usually `ls', which
    !> produces the listing of the directory files and their attributes.
    !> Interactively, a prefix argument will cause the command to prompt
    !> for SWITCHES.
    !>
    !> \<dired_mode_map>You can flag files for deletion with \[dired_flag_file_deletion] and then
    !> delete them by typing \[dired_do_flagged_delete].
    !> Type \[describe_mode] after entering Dired for more info.
    !>
    !> If DIRECTORY_NAME is already in a Dired buffer, that buffer is used without
    !> refresh.  Port of dired.el:dired (interactive).
    recursive subroutine execute_dired(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: directory_name, switches
        logical :: has_switches

        call dired_read_directory_and_switches("", directory_name, switches, has_switches, status)
        if (.not. status%ok()) return

        if (has_switches) then
            call dired(directory_name, status, switches)
        else
            call dired(directory_name, status)
        end if
    end subroutine execute_dired

    !> "Edit" directory DIRECTORY_NAME.  Like `dired' but select in another window.
    !> If this command needs to split the current window, it by default obeys
    !> the user options `split_height_threshold' and `split_width_threshold',
    !> when it decides whether to split the window horizontally or vertically.
    !> Port of dired.el:dired-other-window (interactive).
    recursive subroutine execute_dired_other_window(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: directory_name, switches
        logical :: has_switches

        call dired_read_directory_and_switches("in other window ", directory_name, switches, has_switches, status)
        if (.not. status%ok()) return

        if (has_switches) then
            call dired_other_window(directory_name, status, switches)
        else
            call dired_other_window(directory_name, status)
        end if
    end subroutine execute_dired_other_window

    !> "Edit" directory DIRECTORY_NAME.  Like `dired' but make a new frame.
    !> Port of dired.el:dired-other-frame (interactive).
    recursive subroutine execute_dired_other_frame(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: directory_name, switches
        logical :: has_switches

        call dired_read_directory_and_switches("in other frame ", directory_name, switches, has_switches, status)
        if (.not. status%ok()) return

        if (has_switches) then
            call dired_other_frame(directory_name, status, switches)
        else
            call dired_other_frame(directory_name, status)
        end if
    end subroutine execute_dired_other_frame

    !> "Edit" directory DIRECTORY_NAME.  Like `dired' but make a new tab.
    !> Port of dired.el:dired-other-tab (interactive).
    recursive subroutine execute_dired_other_tab(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: directory_name, switches
        logical :: has_switches

        call dired_read_directory_and_switches("in other tab ", directory_name, switches, has_switches, status)
        if (.not. status%ok()) return

        if (has_switches) then
            call dired_other_tab(directory_name, status, switches)
        else
            call dired_other_tab(directory_name, status)
        end if
    end subroutine execute_dired_other_tab

    !> Like `dired' but return the Dired buffer as value, do not select it.
    !> Port of dired.el:dired-noselect (DIR-OR-LIST a string;
    !> `find_file_visit_truename' .false.).
    recursive subroutine dired_noselect(directory_or_list, buffer, status, switches)
        character(len=*), intent(in) :: directory_or_list
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: switches
        character(len=:), allocatable :: directory_name
        logical :: initially_was_directory_name

        if (len(directory_or_list) == 0) then
            directory_name = default_directory()
        else
            directory_name = directory_or_list
        end if

        ! This loses the distinction between "/foo/*/" and "/foo/*" that
        ! some shells make:
        initially_was_directory_name = file_name_as_directory(directory_name) == directory_name
        call expand_file_name(directory_file_name(directory_name), directory_name, status)
        if (.not. status%ok()) return
        directory_name = abbreviate_file_name(directory_name)
        ! If the argument was syntactically a directory name not a file
        ! name, or if it happens to name a file that is a directory, convert
        ! it syntactically to a directory name.
        if (initially_was_directory_name .or. is_file_directory(directory_name)) directory_name = file_name_as_directory( &
                                                                                                  directory_name)
        call dired_internal_noselect(directory_name, buffer, status, switches)
    end subroutine dired_noselect

    !> `dired_noselect' as a `find_directory_functions' function.
    !> Not a port.
    recursive subroutine dired_noselect_directory(directory_name, buffer, status)
        character(len=*), intent(in) :: directory_name
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status

        call dired_noselect(directory_name, buffer, status)
    end subroutine dired_noselect_directory

    !> Whether the directory visited by the current dired buffer has
    !> changed on disk.  Port of dired.el:dired-directory-changed-p.
    logical function is_dired_directory_changed(directory_name) result(changed)
        character(len=*), intent(in) :: directory_name
        type(file_status_type) :: attributes
        logical :: found

        changed = .false.
        if (.not. buffers(current)%modtime_known) return
        call file_attributes(directory_name, attributes, found)
        if (.not. found) return
        if (iand(attributes%mode, int(o'170000', int32)) /= int(o'040000', int32)) return
        changed = .not. is_time_equal(time_type(attributes%mtime_second, int(attributes%mtime_nsec, int64), &
                                                TIMESPEC_COUNT_RATE), visited_file_modtime())
    end function is_dired_directory_changed

    !> Make or find the Dired buffer for DIRECTORY_NAME (the null handle when
    !> `ls' failed).  Port of dired.el:dired-internal-noselect (DIRECTORY_NAME a
    !> string; no MODE).
    recursive subroutine dired_internal_noselect(directory_name, buffer, status, switches)
        character(len=*), intent(in) :: directory_name
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: switches
        character(len=:), allocatable :: sw, directory_part
        type(handle_type) :: old_buf
        type(status_type) :: st
        integer :: k
        logical :: new_buffer_p, failed

        status = status_type()
        old_buf = current_buffer()
        ! Look for an existing buffer.
        buffer = dired_find_buffer_nocreate(directory_name)
        ! Note that buffer already is in dired-mode, if found.
        new_buffer_p = buffer%index == 0

        if (new_buffer_p) then
            call make_file_buffer(directory_name, buffer, status)
            if (.not. status%ok()) return
        end if

        call set_buffer(buffer, status)
        if (.not. status%ok()) return

        if (.not. new_buffer_p) then
            if (present(switches)) then
                ! ... but new switches: file list may have changed
                call make_local_entry(k)
                locals(k)%directory = directory_name
                ! this calls dired-revert
                call dired_sort_other(switches, status)
            else if (dired_auto_revert_buffer) then
                ! Always revert regardless of whether it has changed or not.
                call revert_buffer(status)
            else if (is_dired_directory_changed(directory_name)) then
                ! If directory has changed on disk, offer to revert.
                call message(substitute_command_keys( &
                             "Directory has changed on disk; type \[revert_buffer] to update Dired"))
            end if
        else
            ! Else a new buffer
            if (is_insert_directory_wildcard_in_directory(directory_name, directory_part)) then
                buffers(current)%directory = directory_part
            else
                ! We can do this unconditionally because dired-noselect
                ! ensures that the name is passed in directory name syntax
                ! if it was the name of a directory at all.
                buffers(current)%directory = file_name_directory(directory_name)
            end if

            if (present(switches)) then
                sw = switches
            else
                sw = dired_listing_switches
            end if

            call dired_mode(status, directory_name, sw)

            if (status%ok()) then
                ! default-directory and dired-actual-switches are set now
                ! (buffer-local), so we can call dired-readin.
                failed = .true.
                dired_ls_error_buffer = null_handle()
                call dired_readin(status)

                if (status%ok()) then
                    failed = .false.

                    if (is_buffer_live(dired_ls_error_buffer)) then
                        if (is_buffer_live(get_buffer("*ls error*"))) failed = .true.
                    end if
                end if
            else
                failed = .true.
            end if

            ! If either `dired_readin' failed (e.g. if parent directories
            ! are inaccessible) or `ls' errored, don't leave the Dired buffer
            ! around.
            if (failed) then
                call close_buffer(buffer, st)
                buffer = null_handle()
            else
                call goto_character(point_minimum())
                call dired_initial_position(directory_name)
            end if
        end if

        if (is_buffer_live(old_buf)) call set_buffer(old_buf, st)
    end subroutine dired_internal_noselect

    !> Pop up the buffer displaying the current `ls' error, if any.
    !> Port of dired.el:dired--display-ls-error.
    subroutine dired_display_ls_error()
        type(handle_type) :: errwin
        type(status_type) :: st

        if (is_buffer_live(dired_ls_error_buffer)) then
            call display_buffer(dired_ls_error_buffer, errwin, st)
            if (is_window_live(errwin)) call fit_window_to_buffer(errwin)
            dired_ls_error_buffer = null_handle()
        end if
    end subroutine dired_display_ls_error

    !> Position point in a new listing of DIRECTORY_NAME.
    !> Point is assumed to be at the beginning of a new subdir line.
    !> Runs the hook `dired_initial_position_hook'.  Port of
    !> dired.el:dired-initial-position (no dired-x).
    subroutine dired_initial_position(directory_name)
        character(len=*), intent(in) :: directory_name

        call end_of_line()
        if (allocated(dired_trivial_filenames)) call dired_goto_next_nontrivial_file()
        call execute_hook_quietly(dired_initial_position_hook)
        if (len(directory_name) < 0) return
    end subroutine dired_initial_position

    !> Position point on first nontrivial file after point.  Port of
    !> dired.el:dired-goto-next-nontrivial-file.
    subroutine dired_goto_next_nontrivial_file()
        character(len=:), allocatable :: fn
        type(status_type) :: st
        integer(int64) :: position
        logical :: found

        ! so there is a file to compare with
        call dired_goto_next_file()
        if (.not. allocated(dired_trivial_filenames)) return

        do while (.not. is_end_of_buffer())
            call dired_get_filename(fn, found, st, no_error_if_not_filep=.true.)
            if (.not. found) fn = ""
            if (.not. string_matches(dired_trivial_filenames, file_name_nondirectory(fn))) exit
            call fwd_line(1_int64)
            position = dired_move_to_filename(st)
        end do
    end subroutine dired_goto_next_nontrivial_file

    !> Move to the next line with a file, within the subdirectory.
    !> Port of dired.el:dired-goto-next-file.
    subroutine dired_goto_next_file()
        integer(int64) :: max
        type(status_type) :: st

        max = dired_subdirectory_maximum() - 1

        do while (dired_move_to_filename(st) == 0 .and. point() < max)
            call fwd_line(1_int64)
        end do
    end subroutine dired_goto_next_file

    ! ------------------------------------------------------------------
    ! Dired mode
    ! ------------------------------------------------------------------

    !> Mode for "editing" directory listings.
    !> In Dired, you are "editing" a list of the files in a directory and
    !>   (optionally) its subdirectories, in the format of `ls -lR'.
    !> "Editing" means that you can run shell commands on files, visit,
    !>   compress or load them, change their file attributes
    !>   and insert subdirectories into the same buffer.  You can "mark"
    !>   files for later commands or "flag" them for deletion, either file
    !>   by file or all files matching certain criteria.
    !> You can move using the usual cursor motion commands.\<dired_mode_map>
    !> The buffer is read-only.  Digits are prefix arguments.
    !> Type \[dired_flag_file_deletion] to flag a file `D' for deletion.
    !> Type \[dired_mark] to Mark a file or subdirectory for later commands.
    !>   Most commands operate on the marked files and use the current file
    !>   if no files are marked.  Use a numeric prefix argument to operate on
    !>   the next ARGUMENT (or previous -ARGUMENT if ARGUMENT<0) files, or just `1'
    !>   to operate on the current file only.  Prefix arguments override marks.
    !>   Mark-using commands display a list of failures afterwards.  Type \[dired_summary]
    !>   to see why something went wrong.
    !> Type \[dired_unmark] to Unmark a file or all files of an inserted subdirectory.
    !> Type \[dired_unmark_backward] to back up one line and unmark or unflag.
    !> Type \[dired_do_flagged_delete] to delete (eXpunge) the files flagged `D'.
    !> Type \[dired_find_file] to Find the current line's file
    !>   (or dired it in another buffer, if it is a directory).
    !> Type \[dired_find_file_other_window] to find file or Dired directory in Other window.
    !> Type \[dired_sort_toggle_or_edit] to toggle Sorting by name/date or change the `ls' switches.
    !> Type \[revert_buffer] to read all currently expanded directories aGain.
    !>   This retains all marks and hides subdirs again that were hidden before.
    !> Use \`SPC' and \`DEL' to move down and up by lines.
    !>
    !> If Dired ever gets confused, you can either type \[revert_buffer]
    !> to read the directories again, type \[dired_do_redisplay]
    !> to relist the file at point or the marked files or a
    !> subdirectory, or type \[dired_build_subdirectories] to parse the buffer
    !> again for the directory tree.
    !>
    !> This mode runs the following hooks:
    !>
    !>   `dired_before_readin_hook'
    !>   `dired_after_readin_hook'
    !>   `dired_mode_hook'
    !>
    !> DIRECTORY_NAME is the directory (`dired_directory'; by default
    !> `default_directory') and SWITCHES the `ls' switches (by default
    !> `dired_listing_switches').  Port of dired.el:dired-mode.
    recursive subroutine dired_mode(status, directory_name, switches)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: directory_name, switches
        type(font_lock_keyword_type), allocatable :: keywords(:)
        logical :: saved
        integer :: k

        if (.not. is_keymap_live(dired_mode_map)) call initialize_dired_mode()
        ! (kill-all-local-variables) (use-local-map dired-mode-map)
        call major_modes%dired_mode%begin(saved, status)
        if (.not. status%ok()) return
        ! default-directory is already set
        call dired_advertise()
        buffers(current)%read_only = .true.
        call revert_buffer_function%set_local(dired_revert)
        call make_local_entry(k)

        if (present(directory_name)) then
            locals(k)%directory = directory_name
        else
            locals(k)%directory = default_directory()
        end if

        if (present(switches)) then
            locals(k)%actual_switches = switches
        else
            locals(k)%actual_switches = dired_listing_switches
        end if

        call dired_font_lock_keywords(keywords)
        call font_lock_defaults%set_local(keywords=keywords, keywords_only=.true.)
        ! before sorting
        call dired_sort_other(locals(k)%actual_switches, status, no_revert=.true.)
        if (.not. status%ok()) return
        call major_modes%dired_mode%end(saved, status)
    end subroutine dired_mode

    !> `dired_mode' as a command procedure (not interactive).  Not a port.
    recursive subroutine dired_mode_command(status)
        type(status_type), intent(out) :: status

        call dired_mode(status)
    end subroutine dired_mode_command

    ! ------------------------------------------------------------------
    ! Reverting
    ! ------------------------------------------------------------------

    !> Reread the Dired buffer.
    !> Must also be called after `dired_actual_switches' have changed.
    !> Should not fail even on completely garbaged buffers.
    !> Preserves old cursor, marks/flags, hidden-p.
    !>
    !> Dired sets `revert_buffer_function' to this function.  The args
    !> IGNORE_AUTO and NOCONFIRM, passed from `revert_buffer', are
    !> ignored.  Port of dired.el:dired-revert (no inserted subdirectories
    !> to restore: they are deferred; no directory-local variables).
    recursive subroutine dired_revert(ignore_auto, noconfirm, status)
        logical, intent(in) :: ignore_auto, noconfirm
        type(status_type), intent(out) :: status
        type(dired_positions_type) :: positions
        type(string_type), allocatable :: mark_files(:)
        character, allocatable :: mark_characters(:)
        type(value_type) :: saved_ro
        logical :: modflag, saved_fold

        ! just in case user narrowed
        call widen()
        call is_buffer_modified(current_buffer(), modflag, status)
        if (.not. status%ok()) return
        call dired_save_positions(positions)
        saved_fold = case_fold_search%value
        ! we check for upper case ls flags
        case_fold_search%value = .false.
        call inhibit_read_only_begin(saved_ro)
        call goto_character(point_minimum())
        ! only after dired-remember-hidden since this unhides:
        call dired_remember_marks(point_minimum(), point_maximum(), mark_files, mark_characters)
        ! Run dired-after-readin-hook just once, below.
        call dired_readin(status, run_after_hook=.false.)

        if (status%ok()) then
            ! mark files that were marked
            call dired_mark_remembered(mark_files, mark_characters)
            ! ... run the hook for the whole buffer, and only after markers
            ! have been reinserted.
            call dired_after_readin_hook%execute(status)
            call dired_restore_positions(positions)
            if (.not. modflag) call restore_buffer_modified(.false.)
        end if

        call inhibit_read_only_end(saved_ro)
        case_fold_search%value = saved_fold
        call destroy_positions(positions)
        if (ignore_auto .and. noconfirm) continue
    end subroutine dired_revert

    !> Return current positions in the buffer and all windows with this
    !> directory: the file name and line number at point, in the windows
    !> showing the buffer, and at the point a window keeps for it among its
    !> previous buffers.  Port of dired.el:dired-save-positions.
    subroutine dired_save_positions(positions)
        type(dired_positions_type), intent(out) :: positions
        type(handle_type), allocatable :: list(:)
        type(dired_saved_position_type), allocatable :: grown(:)
        type(handle_type) :: me
        integer(int64) :: original_point, position
        integer :: k, j

        me = current_buffer()
        positions%buffer = me
        original_point = point()
        call save_at(original_point, positions%here)
        allocate (positions%windows(0), positions%previouss(0))
        call window_leaves(selected_frame%root_window, list)

        do k = 1, size(list)
            if (.not. same_window(windows(list(k)%index)%buffer, me)) cycle

            if (same_window(list(k), selected_window())) then
                position = original_point
            else
                position = window_point(windows(list(k)%index))
            end if

            allocate (grown(size(positions%windows) + 1))
            grown(1:size(positions%windows)) = positions%windows
            call save_at(position, grown(size(grown)))
            grown(size(grown))%window = list(k)
            call move_alloc(grown, positions%windows)
        end do

        ! For each window that showed the current buffer before, its
        ! window-point marker in the window's list of previous buffers.
        do k = 1, size(list)
            if (.not. allocated(windows(list(k)%index)%previous_buffers)) cycle

            do j = 1, size(windows(list(k)%index)%previous_buffers)
                if (.not. same_window(windows(list(k)%index)%previous_buffers(j)%buffer, me)) cycle
                if (windows(list(k)%index)%previous_buffers(j)%point%index == 0) exit
                position = marked_position(windows(list(k)%index)%previous_buffers(j)%point)
                allocate (grown(size(positions%previouss) + 1))
                grown(2:) = positions%previouss
                call save_at(position, grown(1))
                grown(1)%marker = windows(list(k)%index)%previous_buffers(j)%point
                call move_alloc(grown, positions%previouss)
                exit
            end do
        end do

        call goto_character(original_point)

    contains

        subroutine save_at(p, saved)
            integer(int64), intent(in) :: p
            type(dired_saved_position_type), intent(inout) :: saved
            character(len=:), allocatable :: fn
            type(status_type) :: st

            call goto_character(p)
            call dired_get_filename(fn, saved%has_file, st, no_error_if_not_filep=.true.)
            if (saved%has_file) saved%file = fn
            saved%line = line_number_at_position(p)
        end subroutine save_at
    end subroutine dired_save_positions

    !> Release what `dired_save_positions' made (nothing to free: the
    !> markers belong to the windows).  Not a port.
    subroutine destroy_positions(positions)
        type(dired_positions_type), intent(inout) :: positions

        if (allocated(positions%windows)) deallocate (positions%windows)
        if (allocated(positions%previouss)) deallocate (positions%previouss)
    end subroutine destroy_positions

    !> Restore POSITIONS saved with `dired_save_positions'.  Port of
    !> dired.el:dired-restore-positions.
    subroutine dired_restore_positions(positions)
        type(dired_positions_type), intent(in) :: positions
        type(status_type) :: st
        type(handle_type) :: mbuf
        integer(int64) :: original_point, position
        integer :: k

        call goto_character(position_of(positions%here))

        do k = 1, size(positions%windows)
            ! Ensure that window still displays the original buffer.
            if (.not. is_window_live(positions%windows(k)%window)) cycle
            if (.not. same_window(windows(positions%windows(k)%window%index)%buffer, positions%buffer)) cycle

            if (same_window(positions%windows(k)%window, selected_window())) then
                call goto_character(position_of(positions%windows(k)))
            else
                original_point = point()
                position = position_of(positions%windows(k))
                call goto_character(original_point)
                call set_window_point(windows(positions%windows(k)%window%index), position, st)
            end if
        end do

        original_point = point()

        do k = 1, size(positions%previouss)
            ! Sanity check of the point marker.
            call marker_buffer(positions%previouss(k)%marker, mbuf, st)
            if (.not. st%ok()) cycle
            if (mbuf%index == 0) cycle
            if (.not. same_window(mbuf, positions%buffer)) cycle
            position = position_of(positions%previouss(k))
            call set_marker(positions%previouss(k)%marker, position, st)
        end do

        call goto_character(original_point)

    contains

        !> Go to the saved file, else to the saved line's file; return
        !> point.
        integer(int64) function position_of(saved) result(p)
            type(dired_saved_position_type), intent(in) :: saved
            integer(int64) :: q
            type(status_type) :: s2

            q = 0
            if (saved%has_file) q = dired_goto_file(saved%file, s2)

            if (q == 0) then
                call goto_character(point_minimum())
                call fwd_line(saved%line - 1)
                q = dired_move_to_filename(s2)
            end if

            p = point()
        end function position_of
    end subroutine dired_restore_positions

    !> Return the files and their marks, from BEGINNING to END (the last first).
    !> Port of dired.el:dired-remember-marks.
    subroutine dired_remember_marks(beginning, end, files, chars)
        integer(int64), intent(in) :: beginning, end
        type(string_type), allocatable, intent(out) :: files(:)
        character, allocatable, intent(out) :: chars(:)
        type(string_type), allocatable :: gf(:)
        character, allocatable :: gc(:)
        character(len=:), allocatable :: fil
        type(status_type) :: st
        integer(int64) :: original_point
        logical :: found

        allocate (files(0), chars(0))
        original_point = point()
        call goto_character(beginning)

        do while (is_regular_expression_search_forward(dired_regular_expression_mark, end))
            call dired_get_filename(fil, found, st, no_error_if_not_filep=.true.)
            if (.not. found) cycle
            allocate (gf(size(files) + 1), gc(size(chars) + 1))
            gf(1)%text = fil
            gf(2:) = files
            gc(1) = character_before_point()
            gc(2:) = chars
            call move_alloc(gf, files)
            call move_alloc(gc, chars)
        end do

        call goto_character(original_point)
    end subroutine dired_remember_marks

    !> Mark all files remembered in FILES with their CHARS.  Port of
    !> dired.el:dired-mark-remembered.
    subroutine dired_mark_remembered(files, chars)
        type(string_type), intent(in) :: files(:)
        character, intent(in) :: chars(:)
        type(status_type) :: st
        integer(int64) :: original_point
        integer :: k

        original_point = point()

        do k = 1, size(files)
            if (dired_goto_file(files(k)%text, st) /= 0) then
                call beginning_of_line()
                call replace_character_at_point(chars(k), st)
            end if
        end do

        call goto_character(original_point)
    end subroutine dired_mark_remembered

    ! ------------------------------------------------------------------
    ! Going to a file
    ! ------------------------------------------------------------------

    !> Go to line describing file FILE in this Dired buffer.  Return the
    !> position, or 0.  FILE must be an absolute file name.  Port of
    !> dired.el:dired-goto-file.
    function dired_goto_file(file_in, status) result(found)
        character(len=*), intent(in) :: file_in
        type(status_type), intent(out) :: status
        integer(int64) :: found
        character(len=:), allocatable :: file, directory
        integer(int64) :: original_point
        logical :: saved_fold, ok

        status = status_type()
        found = 0

        if (.not. is_file_name_absolute(file_in)) then
            call set_error(status, GENERIC_ERROR_CODE, format_message("File name `")//file_in// &
                           format_message("' is not absolute"))
            return
        end if

        ! does no harm if not a directory
        file = directory_file_name(file_in)
        saved_fold = case_fold_search%value
        case_fold_search%value = .false.
        directory = file_name_directory(file)
        original_point = point()
        ! First, look for a listing under the absolute name.
        call goto_character(point_minimum())
        found = dired_goto_file_1(file, file, point_maximum())

        if (found == 0) then
            ! Next, look for it as a relative name with leading
            ! subdirectories.
            call goto_character(point_minimum())
            found = dired_goto_file_1(file_relative_name(file, default_directory()), file, point_maximum())
        end if

        if (found == 0) then
            ! Otherwise, look for it as a relative name, a base name only.
            if (directory == expand(default_directory())) then
                call goto_character(point_minimum())
                ok = .true.
            else
                ok = .false.
                if (table_size() > 1) ok = dired_goto_subdirectory(directory) /= 0
            end if

            if (ok) found = dired_goto_file_1(file_name_nondirectory(file), file, dired_subdirectory_maximum())
        end if

        case_fold_search%value = saved_fold

        ! Return buffer position, if found.
        if (found /= 0) then
            call goto_character(found)
        else
            call goto_character(original_point)
        end if
    end function dired_goto_file

    !> Advance to the Dired listing labeled by FILE; return its position.
    !> Return 0 if the listing is not found.  If FILE contains characters
    !> that would not appear in a Dired buffer, search using the quoted
    !> forms of those characters.
    !>
    !> FULL_NAME specifies the actual file name the listing must have,
    !> as returned by `dired_get_filename'.  LIMIT is the search limit.
    !> Port of dired.el:dired-goto-file-1.
    function dired_goto_file_1(file, full_name, limit) result(found)
        character(len=*), intent(in) :: file, full_name
        integer(int64), intent(in) :: limit
        integer(int64) :: found
        character(len=:), allocatable :: string, fn
        integer(int64) :: position
        type(status_type) :: st
        logical :: have

        string = string_replace(achar(13), "\^m", file)
        string = string_replace("\", "\\", string)

        if (is_dired_switches_escape(dired_actual_switches_of()) .and. scan(string, " "//TAB//LF) > 0) then
            string = string_replace(" ", "\ ", string)
            string = string_replace(TAB, "\t", string)
            string = string_replace(LF, "\n", string)
        end if

        found = 0

        do while (found == 0)
            ! filenames are preceded by SPC, this makes the search faster
            ! (e.g. for the filename "-").
            call search_forward(" "//string, position, st, bound=limit, noerror=SEARCH_MOVE)
            if (position <= 0) exit
            ! Check that we are in the right place.  Match could have BASE
            ! just as initial substring or in permission bits etc.
            call dired_get_filename(fn, have, st, no_error_if_not_filep=.true.)

            if (have .and. fn == full_name .and. len(fn) == len(full_name)) then
                found = dired_move_to_filename(st)
            else
                call fwd_line(1_int64)
            end if
        end do
    end function dired_goto_file_1

    !> Go to line describing file FILE in this Dired buffer.  Port of
    !> dired.el:dired-goto-file (interactive).
    recursive subroutine execute_dired_goto_file(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file, directory
        integer(int64) :: position

        directory = dired_current_directory(status)
        if (.not. status%ok()) return
        call read_file_name("Goto file: ", file, status, directory=directory)
        if (.not. status%ok()) return
        ! let push-mark display its message
        call push_mark()
        position = dired_goto_file(expand(file), status)
    end subroutine execute_dired_goto_file

    ! ------------------------------------------------------------------
    ! Moving
    ! ------------------------------------------------------------------

    !> The prefix argument of a command as a number (1 if none).
    !> Not a port.
    integer(int64) function prefix_n()
        prefix_n = prefix_numeric_value(current_prefix_argument)
    end function prefix_n

    !> Move down ARGUMENT lines, then position at filename.  Port of
    !> dired.el:dired--trivial-next-line (no invisible text).
    subroutine dired_trivial_next_line(argument, status)
        integer(int64), intent(in) :: argument
        type(status_type), intent(out) :: status
        logical :: saved_visual
        integer(int64) :: saved_goal, position

        saved_visual = line_move_visual
        saved_goal = goal_column
        line_move_visual = .false.
        goal_column = -1
        call line_move(argument, status, noerror=.true.)
        line_move_visual = saved_visual
        goal_column = saved_goal
        position = dired_move_to_filename(status)
    end subroutine dired_trivial_next_line

    !> Goto ARGth next directory file line.  Port of
    !> dired.el:dired--trivial-next-dirline.
    subroutine dired_trivial_next_directory_line(argument, status, original_point_in)
        integer(int64), intent(in) :: argument
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: original_point_in
        integer(int64) :: original_point, position
        logical :: found

        status = status_type()
        original_point = point()
        if (present(original_point_in)) original_point = original_point_in

        if (argument > 0) then
            found = is_regular_expression_search_forward(dired_regular_expression_directory, count=argument)
        else
            call beginning_of_line()
            found = is_regular_expression_search_backward(dired_regular_expression_directory, count=-argument)
        end if

        if (found) then
            ! user may type `i' or `f'
            position = dired_move_to_filename(status)
        else
            call goto_character(original_point)

            if (dired_movement_style%is_off()) then
                call set_error(status, GENERIC_ERROR_CODE, "No more subdirectories")
            end if
        end if
    end subroutine dired_trivial_next_directory_line

    !> Move ARGUMENT lines (JUMP 1) or directory lines (JUMP 2), as
    !> `dired_movement_style' says.  Port of
    !> dired.el:dired--move-to-next-line.
    subroutine dired_move_to_next_line(argument_in, jump, status)
        integer(int64), intent(in) :: argument_in
        integer, intent(in) :: jump
        type(status_type), intent(out) :: status
        integer(int64) :: argument, old_argument, old_position, moving_down, position
        logical :: wrapped, cycle_style, files_style

        status = status_type()
        argument = argument_in
        wrapped = .false.
        old_argument = argument
        ! It's always true that we should move to the filename when
        ! possible.
        position = dired_move_to_filename(status)
        old_position = point()
        ! Up/Down indicates the direction.
        moving_down = -1
        if (argument > 0) moving_down = 1
        cycle_style = dired_movement_style%choice == DIRED_MOVEMENT_STYLE_CYCLE .or. &
                      dired_movement_style%choice == DIRED_MOVEMENT_STYLE_CYCLE_FILES
        files_style = dired_movement_style%choice == DIRED_MOVEMENT_STYLE_CYCLE_FILES .or. &
                      dired_movement_style%choice == DIRED_MOVEMENT_STYLE_BOUNDED_FILES

        ! Line by line in case we forget to skip empty lines.
        do while (argument /= 0)
            call jumpfun(moving_down)

            if (old_position == point()) then
                ! Now point is at beginning/end of movable area, but it
                ! still wants to move farther.
                if (cycle_style) then
                    ! `cycle': go to the other end.  Argument not changing
                    ! on the second wrap means infinite loop with no files
                    ! found.
                    if (wrapped .and. old_argument == argument) then
                        argument = 0
                    else if (moving_down > 0) then
                        call goto_character(point_minimum())
                    else
                        call goto_character(point_maximum())
                    end if

                    wrapped = .true.
                else
                    ! `bounded': go back to the last non-empty line.
                    do while (is_empty_line() .and. argument /= 0)
                        call jumpfun(-moving_down)

                        ! Point not moving means infinite loop.
                        if (old_position == point()) then
                            argument = 0
                        else
                            old_position = point()
                        end if
                    end do

                    ! Encountered a boundary, so let's stop movement.
                    if (is_empty_line()) then
                        argument = 0
                    else
                        argument = moving_down
                    end if
                end if
            end if

            if (.not. is_empty_line()) then
                ! Has moved to a non-empty line.  This movement does make
                ! sense.
                argument = argument - moving_down
            end if

            old_position = point()
        end do

    contains

        subroutine jumpfun(n)
            integer(int64), intent(in) :: n
            type(status_type) :: s2

            if (jump == 1) then
                call dired_trivial_next_line(n, s2)
            else
                call dired_trivial_next_directory_line(n, s2)
            end if
        end subroutine jumpfun

        !> (and (dired-between-files) (or FILES-STYLE (not (dired-get-subdir))))
        logical function is_empty_line()
            is_empty_line = .false.
            if (.not. dired_between_files()) return

            if (files_style) then
                is_empty_line = .true.
            else
                is_empty_line = len(dired_get_subdirectory()) == 0
            end if
        end function is_empty_line
    end subroutine dired_move_to_next_line

    !> Move down ARGUMENT lines, then position at filename.
    !> The argument ARGUMENT (interactively, prefix argument) says how many lines
    !> to move; the default is one line.
    !>
    !> Whether to skip empty lines and how to move from last line
    !> is controlled by `dired_movement_style'.  Port of
    !> dired.el:dired-next-line.
    subroutine dired_next_line(argument, status)
        integer(int64), intent(in) :: argument
        type(status_type), intent(out) :: status

        if (.not. dired_movement_style%is_off()) then
            call dired_move_to_next_line(argument, 1, status)
        else
            call dired_trivial_next_line(argument, status)
        end if
    end subroutine dired_next_line

    !> Move up ARGUMENT lines, then position at filename.
    !> The argument ARGUMENT (interactively, prefix argument) says how many lines
    !> to move; the default is one line.
    !>
    !> Whether to skip empty lines and how to move from first line
    !> is controlled by `dired_movement_style'.  Port of
    !> dired.el:dired-previous-line.
    subroutine dired_previous_line(argument, status)
        integer(int64), intent(in) :: argument
        type(status_type), intent(out) :: status

        call dired_next_line(-argument, status)
    end subroutine dired_previous_line

    !> Goto ARGth next directory file line.
    !>
    !> Whether to skip empty lines and how to move from last line
    !> is controlled by `dired_movement_style'.  Port of
    !> dired.el:dired-next-dirline.
    subroutine dired_next_directory_line(argument, status)
        integer(int64), intent(in) :: argument
        type(status_type), intent(out) :: status

        if (.not. dired_movement_style%is_off()) then
            call dired_move_to_next_line(argument, 2, status)
        else
            call dired_trivial_next_directory_line(argument, status)
        end if
    end subroutine dired_next_directory_line

    !> Goto ARGth previous directory file line.
    !>
    !> Whether to skip empty lines and how to move from last line
    !> is controlled by `dired_movement_style'.  Port of
    !> dired.el:dired-prev-dirline.
    subroutine dired_previous_directory_line(argument, status)
        integer(int64), intent(in) :: argument
        type(status_type), intent(out) :: status

        call dired_next_directory_line(-argument, status)
    end subroutine dired_previous_directory_line

    !> Move down ARGUMENT lines, then position at filename.
    !> The argument ARGUMENT (interactively, prefix argument) says how many lines
    !> to move; the default is one line.
    !>
    !> Whether to skip empty lines and how to move from last line
    !> is controlled by `dired_movement_style'.  Port of
    !> dired.el:dired-next-line (interactive).
    subroutine execute_dired_next_line(status)
        type(status_type), intent(out) :: status

        call dired_next_line(prefix_n(), status)
    end subroutine execute_dired_next_line

    !> Move up ARGUMENT lines, then position at filename.
    !> The argument ARGUMENT (interactively, prefix argument) says how many lines
    !> to move; the default is one line.
    !>
    !> Whether to skip empty lines and how to move from first line
    !> is controlled by `dired_movement_style'.  Port of
    !> dired.el:dired-previous-line (interactive).
    subroutine execute_dired_previous_line(status)
        type(status_type), intent(out) :: status

        call dired_previous_line(prefix_n(), status)
    end subroutine execute_dired_previous_line

    !> Goto ARGth next directory file line.
    !>
    !> Whether to skip empty lines and how to move from last line
    !> is controlled by `dired_movement_style'.  Port of
    !> dired.el:dired-next-dirline (interactive).
    subroutine execute_dired_next_directory_line(status)
        type(status_type), intent(out) :: status

        call dired_next_directory_line(prefix_n(), status)
    end subroutine execute_dired_next_directory_line

    !> Goto ARGth previous directory file line.
    !>
    !> Whether to skip empty lines and how to move from last line
    !> is controlled by `dired_movement_style'.  Port of
    !> dired.el:dired-prev-dirline (interactive).
    subroutine execute_dired_previous_directory_line(status)
        type(status_type), intent(out) :: status

        call dired_previous_directory_line(prefix_n(), status)
    end subroutine execute_dired_previous_directory_line

    !> Go to next subdirectory, regardless of level.  Port of
    !> dired.el:dired-next-subdir (interactive).
    subroutine execute_dired_next_subdirectory(status)
        type(status_type), intent(out) :: status
        integer(int64) :: position

        position = dired_next_subdirectory(prefix_n(), status)
    end subroutine execute_dired_next_subdirectory

    !> Go to previous subdirectory, regardless of level.
    !> When called interactively and not on a subdir line, go to this
    !> subdir's line.  Port of dired-aux.el:dired-prev-subdir
    !> (interactive).
    subroutine execute_dired_previous_subdirectory(status)
        type(status_type), intent(out) :: status
        integer(int64) :: position, argument

        if (current_prefix_argument%kind /= PREFIX_NONE) then
            argument = prefix_n()
        else if (len(dired_get_subdirectory()) > 0) then
            ! if on subdir start already, don't stay there!
            argument = 1
        else
            argument = 0
        end if

        position = dired_previous_subdirectory(argument, status)
    end subroutine execute_dired_previous_subdirectory

    !> Run Dired on parent directory of current directory.
    !> Find the parent directory either in this buffer or another buffer.
    !> Creates a buffer if necessary.
    !> If OTHER_WINDOW (the optional prefix arg), display the parent
    !> directory in another window.  Port of dired.el:dired-up-directory.
    recursive subroutine dired_up_directory(status, other_window)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: other_window
        character(len=:), allocatable :: directory, up
        integer(int64) :: position
        logical :: other

        other = .false.
        if (present(other_window)) other = other_window
        directory = dired_current_directory(status)
        if (.not. status%ok()) return
        up = file_name_directory(directory_file_name(directory))
        if (dired_goto_file(directory_file_name(directory), status) /= 0) return
        if (.not. status%ok()) return

        ! Only try dired-goto-subdir if buffer has more than one dir.
        if (table_size() > 1) then
            if (dired_goto_subdirectory(up) /= 0) return
        end if

        if (other) then
            call dired_other_window(up, status)
        else
            call dired_find_possibly_alternative_file(up, status)
        end if

        if (.not. status%ok()) return
        position = dired_goto_file(directory, status)
    end subroutine dired_up_directory

    !> Run Dired on parent directory of current directory.
    !> Find the parent directory either in this buffer or another buffer.
    !> Creates a buffer if necessary.
    !> If OTHER-WINDOW (the optional prefix arg), display the parent
    !> directory in another window.  Port of dired.el:dired-up-directory
    !> (interactive).
    recursive subroutine execute_dired_up_directory(status)
        type(status_type), intent(out) :: status

        call dired_up_directory(status, current_prefix_argument%kind /= PREFIX_NONE)
    end subroutine execute_dired_up_directory

    ! ------------------------------------------------------------------
    ! Visiting
    ! ------------------------------------------------------------------

    !> Get the current line's file name, with an error if file does not
    !> exist.  Port of dired.el:dired-get-file-for-visit.
    recursive subroutine dired_get_file_for_visit(file_name, status)
        character(len=:), allocatable, intent(out) :: file_name
        type(status_type), intent(out) :: status
        logical :: found

        ! We pass t for second arg so that we don't get error for `.' and
        ! `..'.
        call dired_get_filename(file_name, found, status, no_error_if_not_filep=.true.)
        if (.not. status%ok()) return

        if (.not. found) then
            call set_error(status, USER_ERROR_CODE, "No file on this line")
            return
        end if

        ! (file-name-sans-versions raw t) keeps backup versions: the name
        ! is unchanged.
        if (file_exists(file_name)) return

        if (is_file_symlink(file_name)) then
            call set_error(status, GENERIC_ERROR_CODE, "File is a symlink to a nonexistent target")
        else
            call set_error(status, GENERIC_ERROR_CODE, substitute_command_keys( &
                           "File no longer exists; type \<dired_mode_map>\[revert_buffer] to update Dired buffer"))
        end if
    end subroutine dired_get_file_for_visit

    !> Find FILE (with `find_file_run_dired' bound to true).  Port of
    !> dired.el:dired--find-possibly-alternative-file with dired--find-file
    !> (`dired_close_when_opening_new_dired_buffer' .false.).
    recursive subroutine dired_find_possibly_alternative_file(file, status)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        logical :: saved

        saved = find_file_run_dired
        find_file_run_dired = .true.
        call find_file(file, status)
        find_file_run_dired = saved
    end subroutine dired_find_possibly_alternative_file

    !> In Dired, visit the file or directory named on this line.  Port of
    !> dired.el:dired-find-file.
    recursive subroutine dired_find_file(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file

        call dired_get_file_for_visit(file, status)
        if (.not. status%ok()) return
        call dired_find_possibly_alternative_file(file, status)
    end subroutine dired_find_file

    !> In Dired, visit this file or directory in another window.
    !> If this command needs to split the current window, it by default obeys
    !> the user options `split_height_threshold' and `split_width_threshold',
    !> when it decides whether to split the window horizontally or vertically.
    !> Port of dired.el:dired-find-file-other-window.
    recursive subroutine dired_find_file_other_window(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        logical :: saved

        call dired_get_file_for_visit(file, status)
        if (.not. status%ok()) return
        saved = find_file_run_dired
        find_file_run_dired = .true.
        call find_file_other_window(file, status)
        find_file_run_dired = saved
    end subroutine dired_find_file_other_window

    !> In Dired, display this file or directory in another window.  Port of
    !> dired.el:dired-display-file.
    recursive subroutine dired_display_file(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        type(handle_type) :: buffer, window
        type(display_action_parameters_type) :: table

        call dired_get_file_for_visit(file, status)
        if (.not. status%ok()) return
        call find_file_noselect(file, buffer, status)
        if (.not. status%ok()) return
        ! (display-buffer BUFFER t)
        table%inhibit_same_window = .true.
        call display_buffer(buffer, window, status, table=table)
    end subroutine dired_display_file

    ! ------------------------------------------------------------------
    ! Marked files
    ! ------------------------------------------------------------------

    !> Return a string suitable for use in a Dired prompt.
    !> ARGUMENT_KIND and ARGUMENT_N are normally the prefix argument for the calling
    !> command.  FILES should be a list of file names.
    !>
    !> The return value has a form like "foo.txt", "[next 3 files]",
    !> or "* [3 files]".  Port of dired.el:dired-mark-prompt.
    function dired_marker_prompt(argument_kind, argument_n, files) result(prompt)
        integer, intent(in) :: argument_kind
        integer(int64), intent(in) :: argument_n
        type(string_type), intent(in) :: files(:)
        character(len=:), allocatable :: prompt
        character(len=40) :: text

        if (size(files) == 1) then
            prompt = files(1)%text
        else if (argument_kind == DIRED_ARGUMENT_INTEGER) then
            ! abs(arg) = count
            write (text, '("[next ", i0, " files]")') argument_n
            prompt = trim(text)
        else
            write (text, '(" [", i0, " files]")') size(files)
            prompt = dired_marker_character//trim(text)
        end if
    end function dired_marker_prompt

    !> The kind and number of the prefix argument, as the commands on
    !> marked files take it ("P"): none, a number (or `-'), or another
    !> value (C-u).  Not a port.
    subroutine prefix_kind(argument_kind, argument_n)
        integer, intent(out) :: argument_kind
        integer(int64), intent(out) :: argument_n

        argument_n = 0

        if (current_prefix_argument%kind == PREFIX_NONE) then
            argument_kind = DIRED_ARGUMENT_NONE
        else if (current_prefix_argument%kind == PREFIX_RAW) then
            argument_kind = DIRED_ARGUMENT_OTHER
        else
            argument_kind = DIRED_ARGUMENT_INTEGER
            argument_n = prefix_n()
        end if
    end subroutine prefix_kind

    !> The regexp matching a line marked with `dired_marker_character'.
    !> Port of dired.el:dired-marker-regexp.
    function dired_marker_regular_expression() result(regular_expression)
        character(len=:), allocatable :: regular_expression

        regular_expression = "^"//regular_expression_quote(dired_marker_character)
    end function dired_marker_regular_expression

    !> This version skips non-file lines.  Call FUNCTION at the start of
    !> the next ARGUMENT file lines (previous -ARGUMENT if negative).  Port of
    !> dired.el:dired-repeat-over-lines.
    recursive subroutine dired_repeat_over_lines(argument_in, function, status)
        integer(int64), intent(in) :: argument_in
        interface
            recursive subroutine function(status)
                import :: status_type
                implicit none(type, external)
                type(status_type), intent(out) :: status
            end subroutine function
        end interface
        type(status_type), intent(out) :: status
        type(handle_type) :: position
        type(status_type) :: st
        integer(int64) :: argument, original_point, p

        status = status_type()
        argument = argument_in
        call make_marker(position)
        call beginning_of_line()

        do while (argument > 0 .and. .not. is_end_of_buffer())
            argument = argument - 1
            call beginning_of_line()

            do while (.not. is_end_of_buffer() .and. dired_between_files())
                call fwd_line(1_int64)
            end do

            original_point = point()
            call fwd_line(1_int64)
            call set_marker(position, point() + 1, st)
            call goto_character(original_point)
            call function(status)
            call goto_character(original_point)
            if (.not. status%ok()) exit
            ! Advance to the next line--actually, to the line that *was*
            ! next.  (If FUNCTION inserted some new lines in between, skip
            ! them.)
            call goto_character(marked_position(position))
        end do

        do while (status%ok() .and. argument < 0 .and. .not. is_beginning_of_buffer())
            argument = argument + 1
            call fwd_line(-1_int64)

            do while (.not. is_beginning_of_buffer() .and. dired_between_files())
                call fwd_line(-1_int64)
            end do

            call beginning_of_line()
            original_point = point()
            call function(status)
            call goto_character(original_point)
        end do

        call destroy_marker_if_live(position)
        p = dired_move_to_filename(st)
    end subroutine dired_repeat_over_lines

    !> Run BODY with point on each marked line (or the next ARGUMENT_N lines,
    !> or the current line), as dired.el's `dired_map_over_marks' macro
    !> does.  FOUND tells whether marked lines were found (or, for an
    !> integer or other ARGUMENT, true); BODY_RUNS counts the calls.  UPWARD
    !> means the calls went upwards (a negative integer ARGUMENT).  Port of
    !> dired.el:dired-map-over-marks (SHOW-PROGRESS .false.).
    recursive subroutine dired_map_over_marks(body, argument_kind, argument_n, status, found, upward)
        interface
            recursive subroutine body(status)
                import :: status_type
                implicit none(type, external)
                type(status_type), intent(out) :: status
            end subroutine body
        end interface
        integer, intent(in) :: argument_kind
        integer(int64), intent(in) :: argument_n
        type(status_type), intent(out) :: status
        logical, intent(out) :: found, upward
        type(value_type) :: saved_ro
        type(handle_type) :: next_position
        type(status_type) :: st
        integer(int64) :: original_point, p
        logical :: saved_fold

        status = status_type()
        found = .false.
        upward = .false.
        call inhibit_read_only_begin(saved_ro)
        saved_fold = case_fold_search%value
        case_fold_search%value = .false.

        if (argument_kind == DIRED_ARGUMENT_INTEGER) then
            ! no save-excursion, want to move point.
            call dired_repeat_over_lines(argument_n, body, status)
            upward = argument_n < 0
            found = .true.
        else if (argument_kind == DIRED_ARGUMENT_OTHER) then
            ! a non-numeric prefix ARGUMENT, not marked, means use current file:
            call body(status)
            found = .true.
        else
            original_point = point()
            call goto_character(point_minimum())

            ! remember position of next marked file before BODY can insert
            ! lines before the just found file.
            if (is_regular_expression_search_forward(dired_marker_regular_expression())) then
                found = .true.
                call make_marker(next_position)
                call set_marker(next_position, point(), st)

                do while (next_position%index /= 0)
                    call goto_character(marked_position(next_position))
                    call body(status)
                    if (.not. status%ok()) exit
                    ! move after last match
                    call goto_character(marked_position(next_position))
                    call fwd_line(1_int64)

                    if (is_regular_expression_search_forward(dired_marker_regular_expression())) then
                        call set_marker(next_position, point(), st)
                    else
                        call destroy_marker_if_live(next_position)
                        next_position = null_handle()
                    end if
                end do

                if (next_position%index /= 0) call destroy_marker_if_live(next_position)
            end if

            call goto_character(original_point)
            if (.not. found .and. argument_kind /= DIRED_ARGUMENT_MARKED .and. status%ok()) call body(status)
        end if

        case_fold_search%value = saved_fold
        call inhibit_read_only_end(saved_ro)
        ! save-excursion loses, again
        p = dired_move_to_filename(st)
    end subroutine dired_map_over_marks

    !> Collect the file name of this line into the `map_files' results
    !> (`map_localp', `map_no_error').  Not a port.
    recursive subroutine collect_filename(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        logical :: found

        call dired_get_filename(file, found, status, map_localp, map_no_error)
        if (.not. status%ok()) return
        call append_result(file, found)
    end subroutine collect_filename

    !> Collect the file name and a marker at point (dired-do-delete's
    !> body).  Not a port.
    recursive subroutine collect_filename_and_marker(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        type(handle_type), allocatable :: grown(:)
        type(status_type) :: st
        logical :: found
        integer :: n

        call dired_get_filename(file, found, status)
        if (.not. status%ok()) return
        call append_result(file, found)
        n = map_n
        allocate (grown(n))
        if (n > 1) grown(1:n - 1) = map_markers(1:n - 1)
        call make_marker(grown(n))
        call set_marker(grown(n), point(), st)
        call move_alloc(grown, map_markers)
    end subroutine collect_filename_and_marker

    !> Append FILE (FOUND .false. for none) to the `map_files' results.
    !> Not a port.
    subroutine append_result(file, found)
        character(len=*), intent(in) :: file
        logical, intent(in) :: found
        type(string_type), allocatable :: gf(:)
        logical, allocatable :: gl(:)

        allocate (gf(map_n + 1), gl(map_n + 1))

        if (map_n > 0) then
            gf(1:map_n) = map_files(1:map_n)
            gl(1:map_n) = map_found(1:map_n)
        end if

        gf(map_n + 1)%text = file
        gl(map_n + 1) = found
        call move_alloc(gf, map_files)
        call move_alloc(gl, map_found)
        map_n = map_n + 1
    end subroutine append_result

    !> Start collecting results.  Not a port.
    subroutine reset_results()
        if (allocated(map_files)) deallocate (map_files)
        if (allocated(map_found)) deallocate (map_found)
        if (allocated(map_markers)) deallocate (map_markers)
        allocate (map_files(0), map_found(0), map_markers(0))
        map_n = 0
    end subroutine reset_results

    !> Return the marked files' names as an array of strings.
    !> The list is in the same order as the Dired buffer text, that is, the first
    !>   element is the first marked file.
    !> Values returned are normally absolute file names.
    !> LOCALP is as in `dired_get_filename'.
    !> ARGUMENT_KIND and ARGUMENT_N, if not DIRED_ARGUMENT_NONE, specify files near point
    !>  to return instead of marked files.  They usually come from the
    !>  prefix argument of the caller.
    !>   If ARGUMENT is an integer, return the next ARGUMENT files (previous if ARGUMENT is
    !>    negative).
    !>   If ARGUMENT is DIRED_ARGUMENT_MARKED, return only marked files; return none
    !>    if none are marked.
    !>   If ARGUMENT is DIRED_ARGUMENT_OTHER, return the current file name.
    !>   If no files are marked, and ARGUMENT is DIRED_ARGUMENT_NONE, also return the
    !>   current file name.
    !>
    !> If DISTINGUISH_ONE_MARKED is true, then if we find just one marked
    !> file, ONE_MARKED is true.
    !>
    !> If ERROR is present, an error condition with it as the message occurs when the list of found
    !> files is empty.  Port of dired.el:dired-get-marked-files (no
    !> FILTER).
    recursive subroutine dired_get_marked_files(files, status, localp, argument_kind, argument_n, distinguish_one_marked, &
                                                one_marked, error)
        type(string_type), allocatable, intent(out) :: files(:)
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: localp, argument_kind
        integer(int64), intent(in), optional :: argument_n
        logical, intent(in), optional :: distinguish_one_marked
        logical, intent(out), optional :: one_marked
        character(len=*), intent(in), optional :: error
        integer :: kind, k, n
        integer(int64) :: number
        integer(int64) :: original_point
        logical :: found, upward, distinguish

        kind = DIRED_ARGUMENT_NONE
        if (present(argument_kind)) kind = argument_kind
        number = 0
        if (present(argument_n)) number = argument_n
        distinguish = .false.
        if (present(distinguish_one_marked)) distinguish = distinguish_one_marked
        if (present(one_marked)) one_marked = .false.
        call reset_results()
        map_localp = DIRED_LOCALP_ABSOLUTE
        if (present(localp)) map_localp = localp
        map_no_error = .true.
        original_point = point()
        call dired_map_over_marks(collect_filename, kind, number, status, found, upward)
        if (kind /= DIRED_ARGUMENT_INTEGER) call goto_character(original_point)
        allocate (files(0))
        if (.not. status%ok()) return
        ! (delq nil ...) in buffer order
        n = count(map_found(1:map_n))
        deallocate (files)
        allocate (files(n))
        n = 0

        if (upward) then
            do k = map_n, 1, -1
                if (.not. map_found(k)) cycle
                n = n + 1
                files(n)%text = map_files(k)%text
            end do
        else
            do k = 1, map_n
                if (.not. map_found(k)) cycle
                n = n + 1
                files(n)%text = map_files(k)%text
            end do
        end if

        if (distinguish .and. found .and. kind == DIRED_ARGUMENT_NONE .and. map_n == 1 .and. present(one_marked)) then
            one_marked = n == 1
        end if

        if (size(files) == 0 .and. present(error)) then
            if (len(error) > 0) then
                call set_error(status, USER_ERROR_CODE, error)
            else
                call set_error(status, USER_ERROR_CODE, "No files specified")
            end if
        end if
    end subroutine dired_get_marked_files

    ! ------------------------------------------------------------------
    ! Marking
    ! ------------------------------------------------------------------

    !> Whether Dired marking commands should act on region.  Port of
    !> dired.el:dired-mark--region-use-p.
    logical function is_dired_mark_region_use() result(use_region)
        integer(int64) :: b, e
        type(status_type) :: st

        use_region = .false.
        if (dired_mark_region%is_off()) return
        if (.not. is_region_active()) return
        call region_beginning(b, st)
        call region_end(e, st)
        use_region = e > b
    end function is_dired_mark_region_use

    !> The value of the region beginning aligned to Dired file lines.
    !> Port of dired.el:dired-mark--region-beginning.
    integer(int64) function dired_mark_region_beginning() result(position)
        integer(int64) :: original_point, b
        type(status_type) :: st

        position = point_minimum()
        if (.not. is_dired_mark_region_use()) return
        original_point = point()
        call region_beginning(b, st)
        call goto_character(b)
        position = line_beginning()
        call goto_character(original_point)
    end function dired_mark_region_beginning

    !> The value of the region end aligned to Dired file lines.  Port of
    !> dired.el:dired-mark--region-end.
    integer(int64) function dired_mark_region_end() result(position)
        integer(int64) :: original_point, e
        type(status_type) :: st

        position = point_maximum()
        if (.not. is_dired_mark_region_use()) return
        original_point = point()
        call region_end(e, st)
        call goto_character(e)
        position = region_end_line_position()
        call goto_character(original_point)
    end function dired_mark_region_end

    !> At the region's end: the end of its line when the file there is in
    !> the region, else the start of its line (dired-mark-region).
    !> Not a port.
    integer(int64) function region_end_line_position() result(position)
        type(value_type) :: v
        type(status_type) :: st
        logical :: whole

        if (dired_mark_region%choice == DIRED_MARK_REGION_LINE) then
            whole = .not. is_beginning_of_line()
        else
            whole = .false.

            if (point() > point_minimum()) then
                call get_text_property(point() - 1, value_type(name="dired_filename"), v, st)
                whole = allocated(v%a)
            end if
        end if

        if (whole) then
            position = line_end()
        else
            position = line_beginning()
        end if
    end function region_end_line_position

    !> Mark files for TEST, according to `dired_marker_character'.
    !> TEST is called on each line, with point at beginning of line.
    !> MESSAGE is a noun phrase for the type of files being marked.
    !> It should end with a noun that can be pluralized by adding `s'.
    !>
    !> In Transient Mark mode, if the mark is active, operate on the contents
    !> of the region if `dired_mark_region' is true.  Otherwise, operate
    !> on the whole buffer.
    !>
    !> Return value is the number of files marked (0 if none were marked).
    !> Port of dired.el:dired-mark-if.
    recursive function dired_mark_if(test, message_text, status) result(count)
        interface
            recursive logical function test()
                implicit none(type, external)
            end function test
        end interface
        character(len=*), intent(in) :: message_text
        type(status_type), intent(out) :: status
        integer(int64) :: count
        type(value_type) :: saved_ro
        type(handle_type) :: end_marker
        type(status_type) :: st
        character(len=:), allocatable :: action, region
        character(len=20) :: digits
        integer(int64) :: original_point, beginning
        logical :: is_use_region

        status = status_type()
        call inhibit_read_only_begin(saved_ro)
        is_use_region = is_dired_mark_region_use()
        beginning = dired_mark_region_beginning()
        call make_marker(end_marker)
        call set_marker(end_marker, dired_mark_region_end(), st)
        original_point = point()
        count = 0
        region = ""
        if (is_use_region) region = " in region"

        if (len(message_text) > 0) then
            if (dired_marker_character == " ") then
                action = "Unmarking"
            else if (dired_delete_marker == dired_marker_character) then
                action = "Flagging"
            else
                action = "Marking"
            end if

            if (dired_delete_marker == dired_marker_character) then
                call message(action//" "//message_text//"s for deletion"//region//"...")
            else
                call message(action//" "//message_text//"s"//region//"...")
            end if
        end if

        call goto_character(beginning)

        do while (point() < marked_position(end_marker))
            if (test()) then
                if (character_at() /= dired_marker_character) then
                    call replace_character_at_point(dired_marker_character, status)
                    if (.not. status%ok()) exit
                    count = count + 1
                end if
            end if

            call fwd_line(1_int64)
        end do

        if (len(message_text) > 0 .and. status%ok()) then
            if (dired_marker_character == " ") then
                action = "un"
            else
                action = ""
            end if

            if (dired_marker_character == dired_delete_marker) then
                action = action//"flagged"
            else
                action = action//"marked"
            end if

            write (digits, "(i0)") count
            call message(trim(digits)//" "//message_text//dired_plural_s(count)//" "//action//region)
        end if

        call destroy_marker_if_live(end_marker)
        call goto_character(original_point)
        call inhibit_read_only_end(saved_ro)
    end function dired_mark_if

    !> Mark (with `dired_marker_character') the files of the lines from START
    !> to END, except `.' and `..'.  Port of
    !> dired.el:dired-mark-files-in-region.
    recursive subroutine dired_mark_files_in_region(start, end, status)
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status
        type(value_type) :: saved_ro
        type(handle_type) :: end_marker
        character(len=:), allocatable :: fn
        type(status_type) :: st
        logical :: found

        status = status_type()

        if (start > end) then
            call set_error(status, GENERIC_ERROR_CODE, "Start > End")
            return
        end if

        call inhibit_read_only_begin(saved_ro)
        call make_marker(end_marker)
        call set_marker(end_marker, end, st)
        ! assumed at beginning of line
        call goto_character(start)

        do while (point() < marked_position(end_marker))
            ! Skip subdir line and following garbage like the `total' line:
            do while (point() < marked_position(end_marker) .and. dired_between_files())
                call fwd_line(1_int64)
            end do

            if (.not. is_looking_at(dired_regular_expression_dot)) then
                call dired_get_filename(fn, found, st, no_error_if_not_filep=.true.)

                if (found) then
                    call replace_character_at_point(dired_marker_character, status)
                    if (.not. status%ok()) exit
                end if
            end if

            call fwd_line(1_int64)
        end do

        call destroy_marker_if_live(end_marker)
        call inhibit_read_only_end(saved_ro)
    end subroutine dired_mark_files_in_region

    !> The body of `dired_mark' for one line: mark it with the current
    !> `dired_marker_character' unless it is `.' or `..' (unless a symlink).
    !> Not a port.
    recursive subroutine mark_this_line(status)
        type(status_type), intent(out) :: status
        integer(int64) :: original_point
        logical :: dot_ok, found

        status = status_type()
        dot_ok = .not. is_looking_at(dired_regular_expression_dot)

        if (.not. dot_ok) then
            ! Don't skip symlinks to ".", "..", etc.
            original_point = point()
            found = is_regular_expression_search_forward(dired_permission_flags_regular_expression)
            dot_ok = character_at(mbeg(1)) == "l"
            call goto_character(original_point)
        end if

        if (dot_ok .or. dired_marker_character /= dired_delete_marker) then
            call replace_character_at_point(repeat_character, status)
        end if
    end subroutine mark_this_line

    !> Mark the file at point in the Dired buffer.
    !> If the region is active in Transient Mark mode, mark all files
    !> in the region if `dired_mark_region' is true.
    !> Otherwise, with a prefix arg, mark files on the next ARGUMENT lines.
    !>
    !> If on a subdir headerline, mark all its files except `.' and `..'.
    !>
    !> Use \[dired_unmark_all_files] to remove all marks
    !> and \[dired_unmark] on a subdir to remove the marks in
    !> this subdir.  Port of dired.el:dired-mark (ARGUMENT a number;
    !> INTERACTIVE for the region).
    recursive subroutine dired_mark_1(argument, interactive, status)
        integer(int64), intent(in) :: argument
        logical, intent(in) :: interactive
        type(status_type), intent(out) :: status
        type(value_type) :: saved_ro
        integer(int64) :: original_point, b, e, end

        status = status_type()

        if (interactive .and. is_dired_mark_region_use()) then
            ! Mark files in the active region.
            original_point = point()
            call region_beginning(b, status)
            call region_end(e, status)
            call goto_character(e)
            end = region_end_line_position()
            call goto_character(b)
            call dired_mark_files_in_region(line_beginning(), end, status)
            call goto_character(original_point)
        else if (len(dired_get_subdirectory()) > 0) then
            ! Mark subdir files from the subdir headerline.
            original_point = point()
            call dired_mark_subdirectory_files(status)
            call goto_character(original_point)
        else
            ! Mark the current (or next ARGUMENT) files.
            call inhibit_read_only_begin(saved_ro)
            repeat_character = dired_marker_character
            call dired_repeat_over_lines(argument, mark_this_line, status)
            call inhibit_read_only_end(saved_ro)
        end if
    end subroutine dired_mark_1

    !> Mark the file at point in the Dired buffer.
    !> If the region is active in Transient Mark mode, mark all files
    !> in the region if `dired_mark_region' is true.
    !> Otherwise, with a prefix arg, mark files on the next ARGUMENT lines.
    !>
    !> If on a subdir headerline, mark all its files except `.' and `..'.
    !>
    !> Use \[dired_unmark_all_files] to remove all marks
    !> and \[dired_unmark] on a subdir to remove the marks in
    !> this subdir.  Port of dired.el:dired-mark (interactive).
    recursive subroutine execute_dired_mark(status)
        type(status_type), intent(out) :: status

        call dired_mark_1(prefix_n(), .true., status)
    end subroutine execute_dired_mark

    !> Unmark the file at point in the Dired buffer.
    !> If the region is active, unmark all files in the region.
    !> Otherwise, with a prefix arg, unmark files on the next ARGUMENT lines.
    !>
    !> If looking at a subdir, unmark all its files except `.' and `..'.
    !> If the region is active in Transient Mark mode, unmark all files
    !> in the active region.  Port of dired.el:dired-unmark.
    recursive subroutine dired_unmark(argument, interactive, status)
        integer(int64), intent(in) :: argument
        logical, intent(in) :: interactive
        type(status_type), intent(out) :: status
        character :: saved

        saved = dired_marker_character
        dired_marker_character = " "
        call dired_mark_1(argument, interactive, status)
        dired_marker_character = saved
    end subroutine dired_unmark

    !> Unmark the file at point in the Dired buffer.
    !> If the region is active, unmark all files in the region.
    !> Otherwise, with a prefix arg, unmark files on the next ARGUMENT lines.
    !>
    !> If looking at a subdir, unmark all its files except `.' and `..'.
    !> If the region is active in Transient Mark mode, unmark all files
    !> in the active region.  Port of dired.el:dired-unmark (interactive).
    recursive subroutine execute_dired_unmark(status)
        type(status_type), intent(out) :: status

        call dired_unmark(prefix_n(), .true., status)
    end subroutine execute_dired_unmark

    !> In Dired, flag the current line's file for deletion.
    !> If the region is active, flag all files in the region.
    !> Otherwise, with a prefix arg, flag files on the next ARGUMENT lines.
    !>
    !> If on a subdir headerline, flag all its files except `.' and `..'.
    !> If the region is active in Transient Mark mode, flag all files
    !> in the active region.  Port of dired.el:dired-flag-file-deletion.
    recursive subroutine dired_flag_file_deletion(argument, interactive, status)
        integer(int64), intent(in) :: argument
        logical, intent(in) :: interactive
        type(status_type), intent(out) :: status
        character :: saved

        saved = dired_marker_character
        dired_marker_character = dired_delete_marker
        call dired_mark_1(argument, interactive, status)
        dired_marker_character = saved
    end subroutine dired_flag_file_deletion

    !> In Dired, flag the current line's file for deletion.
    !> If the region is active, flag all files in the region.
    !> Otherwise, with a prefix arg, flag files on the next ARGUMENT lines.
    !>
    !> If on a subdir headerline, flag all its files except `.' and `..'.
    !> If the region is active in Transient Mark mode, flag all files
    !> in the active region.  Port of dired.el:dired-flag-file-deletion
    !> (interactive).
    recursive subroutine execute_dired_flag_file_deletion(status)
        type(status_type), intent(out) :: status

        call dired_flag_file_deletion(prefix_n(), .true., status)
    end subroutine execute_dired_flag_file_deletion

    !> In Dired, move up lines and remove marks or deletion flags there.
    !> Optional prefix ARGUMENT says how many lines to unmark/unflag; default
    !> is one line.
    !> If the region is active in Transient Mark mode, unmark all files
    !> in the active region.  Port of dired.el:dired-unmark-backward.
    recursive subroutine execute_dired_unmark_backward(status)
        type(status_type), intent(out) :: status

        call dired_unmark(-prefix_n(), .true., status)
    end subroutine execute_dired_unmark_backward

    !> Toggle marks: marked files become unmarked, and vice versa.
    !> Flagged files (indicated with flags such as `C' and `D', not
    !> with `*') are not affected, and `.' and `..' are never toggled.
    !> As always, hidden subdirs are not affected.
    !>
    !> In Transient Mark mode, if the mark is active, operate on the contents
    !> of the region if `dired_mark_region' is true.  Otherwise, operate
    !> on the whole buffer.  Port of dired.el:dired-toggle-marks.
    recursive subroutine dired_toggle_marks(status)
        type(status_type), intent(out) :: status
        type(value_type) :: saved_ro
        type(handle_type) :: end_marker
        type(status_type) :: st
        integer(int64) :: original_point

        status = status_type()
        original_point = point()
        call inhibit_read_only_begin(saved_ro)
        call make_marker(end_marker)
        call set_marker(end_marker, dired_mark_region_end(), st)
        call goto_character(dired_mark_region_beginning())

        do while (point() < marked_position(end_marker))
            if (.not. (dired_between_files() .or. is_looking_at(dired_regular_expression_dot))) then
                ! use subst instead of insdel because it does not move the
                ! gap and thus should be faster and because other characters
                ! are left alone automatically
                if (character_at() == " ") then
                    call substitute_character_in_region(point(), point() + 1, iachar(" "), iachar(dired_marker_character), status)
                else
                    call substitute_character_in_region(point(), point() + 1, iachar(dired_marker_character), iachar(" "), status)
                end if

                if (.not. status%ok()) exit
            end if

            call fwd_line(1_int64)
        end do

        call destroy_marker_if_live(end_marker)
        call inhibit_read_only_end(saved_ro)
        call goto_character(original_point)
    end subroutine dired_toggle_marks

    !> The test of `dired_mark_files_regular_expression'.  Not a port.
    recursive logical function is_regular_expression_file() result(p)
        character(len=:), allocatable :: fn
        type(status_type) :: st
        logical :: found

        p = .false.
        if (is_looking_at(dired_regular_expression_dot)) return
        ! empty line
        if (is_end_of_line()) return
        if (is_dired_hidden()) return
        call dired_get_filename(fn, found, st, DIRED_LOCALP_RELATIVE, .true.)
        if (.not. found) return
        p = string_matches(mark_regular_expression, fn)
    end function is_regular_expression_file

    !> Mark all files matching REGULAR_EXPRESSION for use in later commands.
    !> A prefix argument means to unmark them instead.
    !> `.' and `..' are never marked.
    !>
    !> If the region is active in Transient Mark mode, mark files
    !> only in the active region if `dired_mark_region' is true.
    !>
    !> REGULAR_EXPRESSION is an Emacs regexp, not a shell wildcard.  Thus, use `\.o$' for
    !> object files--just `.o' will mark more than you might think.
    !> Port of dired.el:dired-mark-files-regexp.
    recursive subroutine dired_mark_files_regular_expression(regular_expression, status, marker_character)
        character(len=*), intent(in) :: regular_expression
        type(status_type), intent(out) :: status
        character, intent(in), optional :: marker_character
        character :: saved
        integer(int64) :: count

        saved = dired_marker_character
        if (present(marker_character)) dired_marker_character = marker_character
        mark_regular_expression = regular_expression
        count = dired_mark_if(is_regular_expression_file, "matching file", status)
        dired_marker_character = saved
    end subroutine dired_mark_files_regular_expression

    !> Read a regexp in the minibuffer for the marking commands, with
    !> `dired_regular_expression_history'.  Not a port.
    recursive subroutine read_dired_regular_expression(prompt, regular_expression, status, file_defaults)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: regular_expression
        type(status_type), intent(out) :: status
        logical, intent(in) :: file_defaults
        type(string_type), allocatable :: defaults(:)
        character(len=:), allocatable :: fn, ext
        type(status_type) :: st
        logical :: found

        if (file_defaults) then
            ! Add more suggestions into the default list
            call append_string(defaults, "")
            call dired_get_filename(fn, found, st, DIRED_LOCALP_RELATIVE, .true.)
            if (found) call append_string(defaults, fn)
            call dired_get_filename(fn, found, st, DIRED_LOCALP_ABSOLUTE, .true.)

            if (found) then
                call file_name_extension(fn, ext, found, period=.true.)
                call append_string(defaults, regular_expression_quote(ext)//"\'")
            end if

            call read_regular_expression(prompt, regular_expression, status, defaults=defaults, &
                                         history="dired_regular_expression_history")
        else
            call read_regular_expression(prompt, regular_expression, status, history="dired_regular_expression_history")
        end if
    end subroutine read_dired_regular_expression

    !> Mark all files matching REGULAR_EXPRESSION for use in later commands.
    !> A prefix argument means to unmark them instead.
    !> `.' and `..' are never marked.
    !>
    !> If the region is active in Transient Mark mode, mark files
    !> only in the active region if `dired_mark_region' is true.
    !>
    !> REGULAR_EXPRESSION is an Emacs regexp, not a shell wildcard.  Thus, use `\.o$' for
    !> object files--just `.o' will mark more than you might think.
    !> Port of dired.el:dired-mark-files-regexp (interactive).
    recursive subroutine execute_dired_mark_files_regular_expression(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regular_expression, verb
        logical :: unmark

        unmark = current_prefix_argument%kind /= PREFIX_NONE
        verb = "Mark"
        if (unmark) verb = "Unmark"
        call read_dired_regular_expression(verb//" files (regexp): ", regular_expression, status, .true.)
        if (.not. status%ok()) return

        if (unmark) then
            call dired_mark_files_regular_expression(regular_expression, status, " ")
        else
            call dired_mark_files_regular_expression(regular_expression, status)
        end if
    end subroutine execute_dired_mark_files_regular_expression

    !> The test of `dired_mark_files_containing_regular_expression'.  Not a port.
    recursive logical function is_containing_regular_expression() result(p)
        character(len=:), allocatable :: fn
        type(handle_type) :: prebuf, old, temporary
        type(status_type) :: st
        integer(int64) :: original_point, position, inserted
        logical :: found

        p = .false.
        if (is_looking_at(dired_regular_expression_dot)) return
        ! empty line
        if (is_end_of_line()) return
        if (is_dired_hidden()) return
        call dired_get_filename(fn, found, st, DIRED_LOCALP_ABSOLUTE, .true.)
        if (.not. found) return
        if (.not. is_file_readable(fn) .or. is_file_directory(fn)) return
        prebuf = get_file_buffer(expand(fn))
        call message("Checking "//fn)
        old = current_buffer()

        ! For now we do it inside emacs.  Grep might be better if there
        ! are a lot of files.
        if (prebuf%index /= 0 .and. .not. dired_always_read_filesystem) then
            call set_buffer(prebuf, st)
            original_point = point()
            call goto_character(point_minimum())
            call regular_expression_search_forward(mark_regular_expression, position, st, noerror=SEARCH_NOERROR)
            p = st%ok() .and. position > 0
            call goto_character(original_point)
        else
            call get_or_make_buffer(generate_new_buffer_name(" *temp*"), temporary, st)
            call set_buffer(temporary, st)
            call insert_file_contents(fn, .false., inserted, st)
            call goto_character(point_minimum())
            call regular_expression_search_forward(mark_regular_expression, position, st, noerror=SEARCH_NOERROR)
            p = st%ok() .and. position > 0
            call close_buffer(temporary, st)
        end if

        call set_buffer(old, st)
    end function is_containing_regular_expression

    !> Mark all files with contents containing REGULAR_EXPRESSION for use in later commands.
    !> A prefix argument means to unmark them instead.
    !> `.' and `..' are never marked.
    !>
    !> If the region is active in Transient Mark mode, mark files
    !> only in the active region if `dired_mark_region' is true.
    !>
    !> Note that if a file is visited in a Fortmacs buffer, and
    !> `dired_always_read_filesystem' is false, this command will
    !> look in the buffer without revisiting the file, so the results might
    !> be inconsistent with the file on disk if its contents has changed
    !> since it was last visited.  Port of
    !> dired.el:dired-mark-files-containing-regexp.
    recursive subroutine dired_mark_files_containing_regular_expression(regular_expression, status, marker_character)
        character(len=*), intent(in) :: regular_expression
        type(status_type), intent(out) :: status
        character, intent(in), optional :: marker_character
        character :: saved
        integer(int64) :: count

        saved = dired_marker_character
        if (present(marker_character)) dired_marker_character = marker_character
        mark_regular_expression = regular_expression
        count = dired_mark_if(is_containing_regular_expression, "matching file", status)
        dired_marker_character = saved
    end subroutine dired_mark_files_containing_regular_expression

    !> Mark all files with contents containing REGULAR_EXPRESSION for use in later commands.
    !> A prefix argument means to unmark them instead.
    !> `.' and `..' are never marked.  Port of
    !> dired.el:dired-mark-files-containing-regexp (interactive).
    recursive subroutine execute_dired_mark_files_containing_regular_expression(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regular_expression, verb
        logical :: unmark

        unmark = current_prefix_argument%kind /= PREFIX_NONE
        verb = "Mark"
        if (unmark) verb = "Unmark"
        call read_dired_regular_expression(verb//" files containing (regexp): ", regular_expression, status, .false.)
        if (.not. status%ok()) return

        if (unmark) then
            call dired_mark_files_containing_regular_expression(regular_expression, status, " ")
        else
            call dired_mark_files_containing_regular_expression(regular_expression, status)
        end if
    end subroutine execute_dired_mark_files_containing_regular_expression

    !> In Dired, flag all files containing the specified REGULAR_EXPRESSION for deletion.
    !> The match is against the non-directory part of the filename.  Use `^'
    !>   and `$' to anchor matches.  Exclude subdirs by hiding them.
    !> `.' and `..' are never flagged.  Port of
    !> dired.el:dired-flag-files-regexp (interactive).
    recursive subroutine execute_dired_flag_files_regular_expression(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regular_expression

        call read_dired_regular_expression("Flag for deletion (regexp): ", regular_expression, status, .false.)
        if (.not. status%ok()) return
        call dired_mark_files_regular_expression(regular_expression, status, dired_delete_marker)
    end subroutine execute_dired_flag_files_regular_expression

    !> Flag for deletion all files that match `dired_garbage_files_regular_expression'.
    !> Port of dired.el:dired-flag-garbage-files.
    recursive subroutine execute_dired_flag_garbage_files(status)
        type(status_type), intent(out) :: status

        call dired_mark_files_regular_expression(dired_garbage_files_regular_expression, status, dired_delete_marker)
    end subroutine execute_dired_flag_garbage_files

    ! Not a port.
    recursive logical function is_symlink_line()
        is_symlink_line = is_looking_at(dired_regular_expression_symbol)
    end function is_symlink_line

    ! Not a port.
    recursive logical function is_directory_line()
        is_directory_line = is_looking_at(dired_regular_expression_directory) .and. .not. is_looking_at( &
                            dired_regular_expression_dot)
    end function is_directory_line

    ! Not a port.
    recursive logical function is_executable_line()
        is_executable_line = is_looking_at(dired_regular_expression_exe)
    end function is_executable_line

    !> Run `dired_mark_if' with TEST and MESSAGE, unmarking with a prefix
    !> argument, else with marker MARKER.  Not a port.
    recursive subroutine mark_if_command(test, message, marker, status)
        interface
            recursive logical function test()
                implicit none(type, external)
            end function test
        end interface
        character(len=*), intent(in) :: message
        character, intent(in) :: marker
        type(status_type), intent(out) :: status
        character :: saved
        integer(int64) :: count

        saved = dired_marker_character

        if (current_prefix_argument%kind == PREFIX_NONE) then
            dired_marker_character = marker
        else
            dired_marker_character = " "
        end if

        count = dired_mark_if(test, message, status)
        dired_marker_character = saved
    end subroutine mark_if_command

    !> Mark all symbolic links.
    !> With prefix argument, unmark or unflag all those files.
    !> If the region is active in Transient Mark mode, mark files
    !> only in the active region if `dired_mark_region' is true.  Port of
    !> dired.el:dired-mark-symlinks.
    recursive subroutine execute_dired_mark_symlinks(status)
        type(status_type), intent(out) :: status

        call mark_if_command(is_symlink_line, "symbolic link", dired_marker_character, status)
    end subroutine execute_dired_mark_symlinks

    !> Mark all directory file lines except `.' and `..'.
    !> With prefix argument, unmark or unflag all those files.
    !> If the region is active in Transient Mark mode, mark files
    !> only in the active region if `dired_mark_region' is true.  Port of
    !> dired.el:dired-mark-directories.
    recursive subroutine execute_dired_mark_directories(status)
        type(status_type), intent(out) :: status

        call mark_if_command(is_directory_line, "directory file", dired_marker_character, status)
    end subroutine execute_dired_mark_directories

    !> Mark all executable files.
    !> With prefix argument, unmark or unflag all those files.
    !> If the region is active in Transient Mark mode, mark files
    !> only in the active region if `dired_mark_region' is true.  Port of
    !> dired.el:dired-mark-executables.
    recursive subroutine execute_dired_mark_executables(status)
        type(status_type), intent(out) :: status

        call mark_if_command(is_executable_line, "executable file", dired_marker_character, status)
    end subroutine execute_dired_mark_executables

    !> The test of `dired_flag_auto_save_files'.  Not a port.
    recursive logical function is_auto_save_line() result(p)
        character(len=:), allocatable :: fn
        type(status_type) :: st
        integer(int64) :: original_point
        logical :: found

        p = .false.
        ! It is less than general to check for # here, but it's the only
        ! way this runs fast enough.
        original_point = point()
        call end_of_line()

        if (character_before_point() == "#") then
            p = .true.
        else if (character_before_point() == "*") then
            ! Handle executables in case of -F option.
            call goto_character(point() - 1)
            p = character_before_point() == "#"
        end if

        call goto_character(original_point)
        if (.not. p) return
        p = .not. is_looking_at(dired_regular_expression_directory)
        if (.not. p) return
        call dired_get_filename(fn, found, st, DIRED_LOCALP_RELATIVE, .true.)
        p = .false.
        if (found) p = is_auto_save_file_name(file_name_nondirectory(fn))
    end function is_auto_save_line

    !> Flag for deletion files whose names suggest they are auto save files.
    !> A prefix argument says to unmark or unflag those files instead.
    !> If the region is active in Transient Mark mode, flag files
    !> only in the active region if `dired_mark_region' is true.  Port of
    !> dired.el:dired-flag-auto-save-files.
    recursive subroutine execute_dired_flag_auto_save_files(status)
        type(status_type), intent(out) :: status

        call mark_if_command(is_auto_save_line, "auto save file", dired_delete_marker, status)
    end subroutine execute_dired_flag_auto_save_files

    !> The test of `dired_flag_backup_files'.  Not a port.
    recursive logical function is_backup_line() result(p)
        character(len=:), allocatable :: fn
        type(status_type) :: st
        integer(int64) :: original_point
        logical :: found

        ! Don't call backup-file-name-p unless the last character looks
        ! like it might be the end of a backup file name.
        original_point = point()
        call end_of_line()
        ! Handle executables in case of -F option.
        if (character_before_point() == "*") call goto_character(point() - 1)
        p = character_before_point() == "~"
        call goto_character(original_point)
        if (.not. p) return
        p = .not. is_looking_at(dired_regular_expression_directory)
        if (.not. p) return
        call dired_get_filename(fn, found, st, DIRED_LOCALP_RELATIVE, .true.)
        p = .false.
        if (found) p = is_backup_file_name(fn)
    end function is_backup_line

    !> Flag all backup files (names ending with `~') for deletion.
    !> With prefix argument, unmark or unflag these files.
    !> If the region is active in Transient Mark mode, flag files
    !> only in the active region if `dired_mark_region' is true.  Port of
    !> dired.el:dired-flag-backup-files.
    recursive subroutine execute_dired_flag_backup_files(status)
        type(status_type), intent(out) :: status

        call mark_if_command(is_backup_line, "backup file", dired_delete_marker, status)
    end subroutine execute_dired_flag_backup_files

    !> Read a character in the echo area after PROMPT (read-char): C-g
    !> quits, other non-characters are an error.  Not a port.
    subroutine read_character(prompt, c, status)
        character(len=*), intent(in) :: prompt
        integer(int64), intent(out) :: c
        type(status_type), intent(out) :: status
        type(value_type) :: ev
        logical :: have

        c = 0
        if (len(prompt) > 0) call message(prompt)
        cursor_in_echo_area = .true.
        call read_event(ev, have, status)
        cursor_in_echo_area = .false.
        if (.not. status%ok()) return

        if (.not. have) then
            call set_error(status, QUIT_CODE)
        else if (vtag(ev) /= V_INTEGER) then
            call set_error(status, GENERIC_ERROR_CODE, "Non-character input-event")
        else if (xfixnum(ev) == 7) then
            call set_error(status, QUIT_CODE)
        else if (xfixnum(ev) < 0 .or. xfixnum(ev) > int(z'3FFFFF', int64)) then
            call set_error(status, GENERIC_ERROR_CODE, "Non-character input-event")
        else
            c = xfixnum(ev)
        end if
    end subroutine read_character

    !> Change all OLD marks to NEW marks.
    !> OLD and NEW are both characters used to mark files.  Port of
    !> dired.el:dired-change-marks (ASCII marks).
    recursive subroutine dired_change_marks(old, new, status)
        character, intent(in) :: old, new
        type(status_type), intent(out) :: status
        type(value_type) :: saved_ro
        character(len=:), allocatable :: fn
        type(status_type) :: st
        integer(int64) :: original_point, found_position
        logical :: ok

        status = status_type()

        if (new == achar(13) .or. old == achar(13)) then
            if (new == achar(13)) then
                call set_error(status, USER_ERROR_CODE, format_message("Invalid mark character: `")//new// &
                               format_message("'"))
            else
                call set_error(status, USER_ERROR_CODE, format_message("Invalid mark character: `")//old// &
                               format_message("'"))
            end if

            return
        end if

        original_point = point()
        call inhibit_read_only_begin(saved_ro)
        call goto_character(point_minimum())

        do
            call search_forward(LF//old, found_position, st, noerror=SEARCH_NOERROR)
            if (found_position <= 0) exit
            ok = .true.
            if (old == " ") call dired_get_filename(fn, ok, st, DIRED_LOCALP_NO_DIRECTORY, .true.)
            if (ok) call substitute_character_in_region(mbeg(0), mend(0), iachar(old), iachar(new), status)
        end do

        call inhibit_read_only_end(saved_ro)
        call goto_character(original_point)
    end subroutine dired_change_marks

    !> Change all OLD marks to NEW marks.
    !> OLD and NEW are both characters used to mark files.  Port of
    !> dired.el:dired-change-marks (interactive).
    recursive subroutine execute_dired_change_marks(status)
        type(status_type), intent(out) :: status
        integer(int64) :: old, new

        call read_character("Change (old mark): ", old, status)
        if (.not. status%ok()) return
        call read_character("Change "//achar(min(old, 127_int64))//" marks to (new mark): ", new, status)
        if (.not. status%ok()) return

        if (old > 127 .or. new > 127) then
            call set_error(status, USER_ERROR_CODE, "Invalid mark character")
            return
        end if

        call dired_change_marks(achar(old), achar(new), status)
    end subroutine execute_dired_change_marks

    !> Remove a specific mark (or any mark) from every file.
    !> After this command, type the mark character to remove,
    !> or type RET to remove all marks.  Port of
    !> With ARGUMENT true, query for each marked file.  Port of
    !> dired.el:dired-unmark-all-files.
    recursive subroutine dired_unmark_all_files(mark, status, argument)
        character, intent(in) :: mark
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: argument
        character(len=:), allocatable :: file, help
        integer :: query
        logical :: ask, answer, file_found
        type(value_type) :: saved_ro
        type(status_type) :: st
        integer(int64) :: original_point, count, found_position
        logical :: saved_fold, found
        character(len=40) :: text

        status = status_type()
        original_point = point()
        count = 0
        ask = .false.
        if (present(argument)) ask = argument
        query = 0
        help = substitute_command_keys("Type \`SPC' or \`y' to unmark one file, \`DEL' or \`n' to skip to next,"//LF// &
                                       "\`!' to unmark all remaining files with no more questions.")
        call inhibit_read_only_begin(saved_ro)
        saved_fold = case_fold_search%value
        case_fold_search%value = .false.
        call goto_character(point_minimum())

        do
            if (mark == achar(13)) then
                found = is_regular_expression_search_forward(dired_regular_expression_mark)
            else
                call search_forward(LF//mark, found_position, st, noerror=SEARCH_NOERROR)
                found = found_position > 0
            end if

            if (.not. found) exit

            if (ask) then
                found_position = point()
                call dired_get_filename(file, file_found, st, DIRED_LOCALP_RELATIVE, .true.)
                call goto_character(found_position)
                if (.not. (st%ok() .and. file_found)) cycle
                call dired_query(query, format_message("Unmark file `")//file//format_message("'? "), answer, status, &
                                 help_procedure=help)
                call goto_character(found_position)
                if (.not. status%ok()) exit
                if (.not. answer) cycle
            end if

            call substitute_character_in_region(point() - 1, point(), iachar(character_before_point()), iachar(" "), status)
            count = count + 1
        end do

        case_fold_search%value = saved_fold
        call inhibit_read_only_end(saved_ro)

        if (count == 1) then
            call message("1 mark removed")
        else
            write (text, '(i0, " marks removed")') count
            call message(trim(text))
        end if

        call goto_character(original_point)
    end subroutine dired_unmark_all_files

    !> Remove a specific mark (or any mark) from every file.
    !> After this command, type the mark character to remove,
    !> or type RET to remove all marks.
    !> With prefix arg, query for each marked file.
    !> Type \[help_command] at that time for help.  Port of
    !> dired.el:dired-unmark-all-files (interactive).
    recursive subroutine execute_dired_unmark_all_files(status)
        type(status_type), intent(out) :: status
        integer(int64) :: mark

        call read_character("Remove marks (RET means all): ", mark, status)
        if (.not. status%ok()) return

        if (mark > 127) then
            call set_error(status, GENERIC_ERROR_CODE, "Non-ASCII mark character")
            return
        end if

        call dired_unmark_all_files(achar(mark), status, current_prefix_argument%kind /= PREFIX_NONE)
    end subroutine execute_dired_unmark_all_files

    !> Remove all marks from all files in the Dired buffer.  Port of
    !> dired.el:dired-unmark-all-marks.
    recursive subroutine execute_dired_unmark_all_marks(status)
        type(status_type), intent(out) :: status

        call dired_unmark_all_files(achar(13), status)
    end subroutine execute_dired_unmark_all_marks

    !> Move to the ARGth next marked file.
    !> ARGUMENT is the numeric prefix argument and defaults to 1.
    !> If WRAP is true, which happens interactively, wrap around
    !> to the beginning of the buffer and search from there, if no
    !> marked file is found after this line.
    !> Optional argument ORIGINAL_POINT specifies the buffer position to
    !> return to if no ARGth marked file is found; it defaults to
    !> the position where this command was invoked.  Port of
    !> dired.el:dired-next-marked-file.
    recursive subroutine dired_next_marked_file(argument, wrap, status, original_point_in)
        integer(int64), intent(in) :: argument
        logical, intent(in) :: wrap
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: original_point_in
        integer(int64) :: original_point, position
        logical :: found

        status = status_type()
        ! return to where interactively started
        original_point = point()
        if (present(original_point_in)) original_point = original_point_in

        if (argument > 0) then
            found = is_regular_expression_search_forward(dired_regular_expression_mark, count=argument)
        else
            call beginning_of_line()
            found = is_regular_expression_search_backward(dired_regular_expression_mark, count=-argument)
        end if

        if (found) then
            position = dired_move_to_filename(status)
        else if (.not. wrap) then
            call goto_character(original_point)
            call set_error(status, GENERIC_ERROR_CODE, "No next marked file")
        else
            call message("(Wraparound for next marked file)")

            if (argument > 0) then
                call goto_character(point_minimum())
            else
                call goto_character(point_maximum())
            end if

            call dired_next_marked_file(argument, .false., status, original_point)
        end if
    end subroutine dired_next_marked_file

    !> Move to the ARGth next marked file.
    !> ARGUMENT is the numeric prefix argument and defaults to 1.
    !> Interactively, wrap around to the beginning of the buffer and
    !> search from there, if no marked file is found after this line.
    !> Port of dired.el:dired-next-marked-file (interactive).
    recursive subroutine execute_dired_next_marked_file(status)
        type(status_type), intent(out) :: status

        call dired_next_marked_file(prefix_n(), .true., status)
    end subroutine execute_dired_next_marked_file

    !> Move to the ARGth previous marked file.
    !> ARGUMENT is the numeric prefix argument and defaults to 1.
    !> Interactively, wrap around to the end of the buffer and search
    !> backwards from there, if no ARGth marked file is found before this
    !> line.  Port of dired.el:dired-prev-marked-file.
    recursive subroutine execute_dired_previous_marked_file(status)
        type(status_type), intent(out) :: status

        call dired_next_marked_file(-prefix_n(), .true., status)
    end subroutine execute_dired_previous_marked_file

    !> Display the number and total size of the marked files.  Port of
    !> dired.el:dired-number-of-marked-files.
    recursive subroutine dired_number_of_marked_files(status)
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: files(:)
        type(file_status_type) :: st
        integer(int64) :: nmarked
        real(real64) :: total_size
        logical :: one_marked, found
        integer :: k
        character(len=20) :: digits

        call dired_get_marked_files(files, status, distinguish_one_marked=.true., one_marked=one_marked)
        if (.not. status%ok()) return

        if (one_marked) then
            nmarked = 1
        else if (size(files) <= 1) then
            nmarked = 0
        else
            nmarked = size(files)
        end if

        total_size = 0

        do k = 1, size(files)
            call file_attributes(files(k)%text, st, found, nofollow=.true.)
            if (found) total_size = total_size + real(st%size, real64)
        end do

        if (nmarked == 0) then
            call message("No marked files")
        else if (nmarked == 1) then
            write (digits, "(i0)") nmarked
            call message(trim(digits)//" marked file ("//byte_count_to_string(total_size)//" total size)")
        else
            write (digits, "(i0)") nmarked
            call message(trim(digits)//" marked files ("//byte_count_to_string(total_size)//" total size)")
        end if
    end subroutine dired_number_of_marked_files

    ! ------------------------------------------------------------------
    ! Deleting files
    ! ------------------------------------------------------------------

    !> Delete FILE or directory (possibly recursively if RECURSIVE is
    !> not .false.).  RECURSIVE determines what to do with a
    !> non-empty directory.  The effect of its possible values is:
    !>
    !>   .false.                -- do not delete.
    !>   .true.                 -- delete recursively without asking.
    !>   DIRED_RECURSIVE_TOP    -- ask for each directory at top level.
    !>   Anything else          -- ask for each sub-directory.
    !>
    !> The answer may change RECURSIVE (and `dired_recursive_deletes').
    !> Port of dired.el:dired-delete-file (TRASH deferred).
    recursive subroutine dired_delete_file(file, recursive, status)
        character(len=*), intent(in) :: file
        type(choice_type), intent(inout) :: recursive
        type(status_type), intent(out) :: status
        type(read_answer_type) :: answers(4)
        type(value_type) :: files
        type(value_type), allocatable :: elements(:)
        character(len=:), allocatable :: answer, name
        type(status_type) :: st
        logical :: empty_is_directory
        integer :: k

        status = status_type()

        ! This test is equivalent to (and (file-directory-p fn)
        ! (not (file-symlink-p fn))) but more efficient.
        if (.not. (is_file_directory(file) .and. .not. is_file_symlink(file))) then
            call delete_file(file, status)
            return
        end if

        empty_is_directory = .true.
        call directory_files(file, files, st, full=.true.)

        if (st%ok()) then
            call list_elements(files, elements)

            do k = 1, size(elements)
                name = file_name_nondirectory(xstring(elements(k)))
                if (name /= "." .and. name /= "..") empty_is_directory = .false.
            end do
        end if

        if (.not. recursive%is_off() .and. .not. empty_is_directory) then
            if (.not. recursive%value) then
                answers(1) = read_answer_type("yes", iachar("y"), "delete recursively the current directory")
                answers(2) = read_answer_type("no", iachar("n"), "skip to next")
                answers(3) = read_answer_type("all", iachar("!"), "delete all remaining directories with no more questions")
                answers(4) = read_answer_type("quit", iachar("q"), "exit")
                call read_answer("Recursively delete "//dired_make_relative(file)//"? ", answers, answer, status)
                if (.not. status%ok()) return

                select case (answer)
                case ("all")
                    recursive = .true.
                    dired_recursive_deletes = .true.
                case ("yes")
                    if (recursive%choice == DIRED_RECURSIVE_TOP) recursive = .true.
                case ("no")
                    recursive = .false.
                case default
                    ! quit, and catch all unknown answers
                    call set_error(status, QUIT_CODE)
                    return
                end select
            end if
        else
            ! Empty dir or recursive is nil.
            recursive = .false.
        end if

        call delete_directory(file, status,.not. recursive%is_off())
    end subroutine dired_delete_file

    !> In Dired, delete the files flagged for deletion.
    !> If NOMESSAGE is true, we don't display any message
    !> if there are no flagged files.
    !> `dired_recursive_deletes' controls whether deletion of
    !> non-empty directories is allowed.  Port of
    !> dired.el:dired-do-flagged-delete.
    recursive subroutine dired_do_flagged_delete(status, nomessage)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: nomessage
        character :: saved
        integer(int64) :: original_point
        logical :: found, upward, saved_fold, quiet

        status = status_type()
        quiet = .false.
        if (present(nomessage)) quiet = nomessage
        saved = dired_marker_character
        dired_marker_character = dired_delete_marker
        saved_fold = case_fold_search%value
        case_fold_search%value = .false.
        original_point = point()
        call goto_character(point_minimum())
        found = is_regular_expression_search_forward(dired_marker_regular_expression())
        call goto_character(original_point)

        if (found) then
            call reset_results()
            call dired_map_over_marks(collect_filename_and_marker, DIRED_ARGUMENT_NONE, 0_int64, status, found, upward)
            if (status%ok()) call dired_internal_do_deletions(DIRED_ARGUMENT_NONE, 0_int64, upward, status)
            call destroy_map_markers()
        else if (.not. quiet) then
            call message("(No deletions requested)")
        end if

        case_fold_search%value = saved_fold
        dired_marker_character = saved
    end subroutine dired_do_flagged_delete

    !> In Dired, delete the files flagged for deletion.
    !> `dired_recursive_deletes' controls whether deletion of
    !> non-empty directories is allowed.  Port of
    !> dired.el:dired-do-flagged-delete (interactive).
    recursive subroutine execute_dired_do_flagged_delete(status)
        type(status_type), intent(out) :: status

        call dired_do_flagged_delete(status)
    end subroutine execute_dired_do_flagged_delete

    !> Delete all marked (or next ARGUMENT) files.
    !> `dired_recursive_deletes' controls whether deletion of
    !> non-empty directories is allowed.
    !>
    !> If ARGUMENT_KIND is DIRED_ARGUMENT_MARKED, delete only the marked files, or
    !> none if no files are marked.  Port of dired.el:dired-do-delete.
    recursive subroutine dired_do_delete(argument_kind, argument_n, status)
        integer, intent(in) :: argument_kind
        integer(int64), intent(in) :: argument_n
        type(status_type), intent(out) :: status
        logical :: found, upward

        call reset_results()
        ! this may move point if ARGUMENT is an integer
        call dired_map_over_marks(collect_filename_and_marker, argument_kind, argument_n, status, found, upward)
        if (status%ok()) call dired_internal_do_deletions(argument_kind, argument_n, upward, status)
        call destroy_map_markers()
    end subroutine dired_do_delete

    !> Delete all marked (or next ARGUMENT) files.
    !> `dired_recursive_deletes' controls whether deletion of
    !> non-empty directories is allowed.  Port of dired.el:dired-do-delete
    !> (interactive).
    recursive subroutine execute_dired_do_delete(status)
        type(status_type), intent(out) :: status
        integer :: kind
        integer(int64) :: n

        call prefix_kind(kind, n)
        call dired_do_delete(kind, n, status)
    end subroutine execute_dired_do_delete

    !> Free the markers the deletion bodies made.  Not a port.
    subroutine destroy_map_markers()
        integer :: k

        if (.not. allocated(map_markers)) return

        do k = 1, size(map_markers)
            if (map_markers(k)%index /= 0) call destroy_marker_if_live(map_markers(k))
        end do

        deallocate (map_markers)
        allocate (map_markers(0))
    end subroutine destroy_map_markers

    !> Delete the files collected in `map_files' (with `map_markers' at
    !> their lines), after confirmation.  ARGUMENT is the prefix argument;
    !> UPWARD says the files were collected upwards.  Port of
    !> dired.el:dired-internal-do-deletions (TRASH deferred).
    recursive subroutine dired_internal_do_deletions(argument_kind, argument_n, upward, status)
        integer, intent(in) :: argument_kind
        integer(int64), intent(in) :: argument_n
        logical, intent(in) :: upward
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: files(:), relative(:), failures(:)
        type(handle_type), allocatable :: markers(:)
        type(handle_type), allocatable :: buffers(:)
        type(handle_type) :: old
        type(value_type) :: saved_ro
        type(progress_reporter_type) :: reporter
        type(status_type) :: st
        character(len=:), allocatable :: fn, prompt
        character(len=64) :: text
        integer(int64) :: original_point, count, succ, p
        integer :: k, i, j
        type(choice_type) :: recursive
        type(choice_setting_type) :: saved_recursive
        logical :: answer, canceled

        status = status_type()
        ! L is a table of files to delete, with their buffer positions;
        ! filenames are absolute.  (In buffer order.)
        allocate (files(map_n), markers(map_n))

        do k = 1, map_n
            if (upward) then
                j = map_n + 1 - k
            else
                j = k
            end if

            files(k)%text = map_files(j)%text
            markers(k) = map_markers(j)
        end do

        count = map_n
        succ = 0
        ! Bind `dired_recursive_deletes' so that we can change it locally
        ! according with the user answer within `dired_delete_file'.
        saved_recursive = dired_recursive_deletes
        ! canonicalize file list for pop up
        allocate (relative(size(files)))

        do k = 1, size(files)
            relative(k)%text = dired_make_relative(files(k)%text)
        end do

        prompt = "Delete "//dired_marker_prompt(argument_kind, argument_n, relative)//" "
        call dired_mark_pop_up(" *Deletions*", "delete", relative, prompt, answer, status)

        if (.not. status%ok()) then
            dired_recursive_deletes = saved_recursive
            return
        end if

        if (answer) then
            original_point = point()
            allocate (failures(0))
            canceled = .false.
            call make_progress_reporter(reporter, "Deleting...", 0_int64, count)

            do k = 1, size(files)
                call goto_character(marked_position(markers(k)))
                p = dired_move_to_filename(st)
                call inhibit_read_only_begin(saved_ro)
                fn = files(k)%text
                recursive = choice_state(dired_recursive_deletes)
                call dired_delete_file(fn, recursive, st)

                if (st%ok()) then
                    ! if we get here, removing worked
                    succ = succ + 1
                    call progress_reporter_update(reporter, succ)
                    ! dired-fun-in-all-buffers
                    buffers = dired_buffers_for_directory(file_name_directory(fn), file_name_nondirectory(fn))
                    old = current_buffer()

                    do i = 1, size(buffers)
                        call set_buffer(buffers(i), st)
                        call dired_delete_entry(fn, st)
                    end do

                    call set_buffer(old, st)
                    ! For when FN's directory name is different from the
                    ! current buffer's dired-directory.
                    call dired_delete_entry(fn, st)
                    call inhibit_read_only_end(saved_ro)
                else if (condition_matches(st%code, QUIT_CODE)) then
                    call inhibit_read_only_end(saved_ro)
                    call message("OK, canceled")
                    canceled = .true.
                    exit
                else
                    call inhibit_read_only_end(saved_ro)
                    ! catch errors from failed deletions
                    call dired_log(error_name(st%code)//": "//format_error(st)//LF)
                    call prepend_string(failures, fn)
                end if
            end do

            if (.not. canceled) then
                if (size(failures) == 0) then
                    call progress_reporter_done(reporter)
                else if (count == 1) then
                    write (text, '(i0, " of ", i0, " deletion failed")') size(failures), count
                    call dired_log_summary(trim(text), failures)
                else
                    write (text, '(i0, " of ", i0, " deletions failed")') size(failures), count
                    call dired_log_summary(trim(text), failures)
                end if
            end if

            call goto_character(original_point)
        else
            call message("(No deletions performed)")
        end if

        dired_recursive_deletes = saved_recursive
        p = dired_move_to_filename(st)
    end subroutine dired_internal_do_deletions

    !> Remove entry FILE in the current Dired buffer.
    !> Note this doesn't delete FILE in the file system.
    !> See `dired_delete_file' in case you wish that.  Port of
    !> dired.el:dired-remove-entry.
    subroutine dired_remove_entry(file)
        character(len=*), intent(in) :: file
        type(value_type) :: saved_ro
        type(status_type) :: st
        integer(int64) :: original_point, b

        original_point = point()

        if (dired_goto_file(file, st) /= 0) then
            call inhibit_read_only_begin(saved_ro)
            call beginning_of_line()
            b = point()
            call delete_region(b, line_beginning(2_int64), st)
            call inhibit_read_only_end(saved_ro)
        end if

        call goto_character(min(original_point, point_maximum()))
    end subroutine dired_remove_entry

    !> Remove entry FILE in the current Dired buffer.
    !> Like `dired_remove_entry' followed by `dired_clean_up_after_deletion'.
    !> Note this doesn't delete FILE in the file system.
    !> See `dired_delete_file' in case you wish that.  Port of
    !> dired.el:dired-delete-entry.
    recursive subroutine dired_delete_entry(file, status)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status

        call dired_remove_entry(file)
        call dired_clean_up_after_deletion(file, status)
    end subroutine dired_delete_entry

    !> Clean up after a deleted file or directory FN.
    !> Removes any expanded subdirectory of deleted directory.  Port of
    !> dired.el:dired-clean-up-after-deletion (no dired-x: no buffers are
    !> killed).
    recursive subroutine dired_clean_up_after_deletion(fn, status)
        character(len=*), intent(in) :: fn
        type(status_type), intent(out) :: status
        integer(int64) :: original_point

        status = status_type()
        original_point = point()

        if (table_size() > 1) then
            if (dired_goto_subdirectory(fn) /= 0) call dired_remove_subdirectory(status)
        end if

        call goto_character(min(original_point, point_maximum()))
    end subroutine dired_clean_up_after_deletion

    !> Return FUNCTION's result after showing which files are marked:
    !> ask PROMPT with `dired_deletion_confirmer' (ANSWER).  Displays the
    !> file names in a window showing a buffer named BUFFER_NAME.  The
    !> window is not shown if there is just one file, `dired_no_confirm'
    !> has `all' set, or OP_SYMBOL is among its `commands'.
    !>
    !> By default, Dired shrinks the display buffer to fit the marked
    !> files.  Port of dired.el:dired-mark-pop-up (the function is the
    !> confirmer).
    recursive subroutine dired_mark_pop_up(buffer_name, op_symbol, files, prompt, answer, status)
        character(len=*), intent(in) :: buffer_name, op_symbol, prompt
        type(string_type), intent(in) :: files(:)
        logical, intent(out) :: answer
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, window, owindow, old
        type(display_action_parameters_type) :: table
        type(status_type) :: st
        integer :: saved_dedicated
        logical :: quiet

        answer = .false.
        if (.not. associated(dired_deletion_confirmer)) dired_deletion_confirmer => read_yes_or_no
        quiet = dired_no_confirm%all .or. size(files) == 1

        if (allocated(dired_no_confirm%commands)) then
            if (any(dired_no_confirm%commands == op_symbol)) quiet = .true.
        end if

        if (quiet) then
            call dired_deletion_confirmer(prompt, answer, status)
            return
        end if

        old = current_buffer()
        call temporary_buffer_window_setup(buffer_name, buffer, status)
        if (.not. status%ok()) return
        ! Mark *Marked Files* window as softly-dedicated, to prevent other
        ! buffers e.g. *Completions* from reusing it (bug#17554).
        saved_dedicated = display_buffer_mark_dedicated
        display_buffer_mark_dedicated = DEDICATED_WEAK
        ! (display-buffer-below-selected (window-height . fit-window-to-buffer)
        !  (preserve-size . (nil . t)) (body-function ...))
        pop_up_files = files
        table%window_height%procedure => fit_window
        table%body_function => insert_marked_files
        call temporary_buffer_window_show(buffer, window, status, [DISPLAY_ACTION_BELOW_SELECTED], table)
        display_buffer_mark_dedicated = saved_dedicated
        call set_buffer(old, st)
        if (.not. status%ok()) return

        if (.not. is_window_live(window)) then
            call dired_deletion_confirmer(prompt, answer, status)
            return
        end if

        ! (with-selected-window window (unwind-protect (apply function args)
        !    (quit-restore-window window 'kill)))
        owindow = selected_window()
        call select_window(window, st, norecord=.true.)
        call dired_deletion_confirmer(prompt, answer, status)
        if (is_window_live(window)) call quit_restore_window(window, BK_CLOSE, st)
        if (is_window_live(owindow)) call select_window(owindow, st, norecord=.true.)
        if (is_buffer_live(old)) call set_buffer(old, st)
    end subroutine dired_mark_pop_up

    !> The `window_height' of `dired_mark_pop_up': fit-window-to-buffer.
    !> Not a port.
    subroutine fit_window(window)
        type(handle_type), intent(in) :: window

        call fit_window_to_buffer(window)
    end subroutine fit_window

    !> The `body_function' of `dired_mark_pop_up': insert the files in
    !> columns (dired-format-columns-of-files).
    !> Port of dired.el:dired-format-columns-of-files.
    subroutine insert_marked_files(window)
        type(handle_type), intent(in) :: window
        type(string_type), allocatable :: strings(:)
        type(status_type) :: st
        integer(int64) :: beginning
        logical :: changed
        integer :: k

        if (.not. is_window_live(window)) return
        allocate (strings(size(pop_up_files)))

        do k = 1, size(pop_up_files)
            strings(k)%text = pop_up_files(k)%text
        end do

        beginning = point()
        call completion_insert_strings(strings)
        call remove_text_properties(beginning, point(), list(value_type(name="mouse_face"), value_type(), &
                                                             value_type(name="help_echo"), &
                                                             value_type()), changed, st)
    end subroutine insert_marked_files

    !> Set up temporary buffer BUFFER_NAME: no local variables, writable,
    !> no file or undo, empty.  Port of window.el:temp-buffer-window-setup
    !> (no `temporary_buffer_window_setup_hook').
    subroutine temporary_buffer_window_setup(buffer_name, buffer, status)
        character(len=*), intent(in) :: buffer_name
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: old_directory
        type(value_type) :: saved_ro
        logical :: saved_hooks

        old_directory = default_directory()
        call get_or_make_buffer(buffer_name, buffer, status)
        if (.not. status%ok()) return
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        call clear_all_local_variables()
        buffers(current)%directory = old_directory
        buffers(current)%read_only = .false.
        if (allocated(buffers(current)%filename)) deallocate (buffers(current)%filename)
        buffers(current)%undo_enabled = .false.
        buffers(current)%undo_count = 0
        call inhibit_read_only_begin(saved_ro)
        saved_hooks = inhibit_modification_hooks%value
        inhibit_modification_hooks = .true.
        call erase_buffer(status)
        inhibit_modification_hooks = saved_hooks
        call inhibit_read_only_end(saved_ro)
    end subroutine temporary_buffer_window_setup

    ! ------------------------------------------------------------------
    ! Logging failures
    ! ------------------------------------------------------------------

    !> Log a message (TEXT, usually ending with a newline) in the Dired log
    !> buffer.  Port of dired.el:dired-log (a string).
    subroutine dired_log(text)
        character(len=*), intent(in) :: text
        type(handle_type) :: old, buffer
        type(value_type) :: saved_ro
        type(status_type) :: st

        old = current_buffer()
        call get_or_make_buffer(dired_log_buffer, buffer, st)
        call set_buffer(buffer, st)
        call goto_character(point_maximum())
        call inhibit_read_only_begin(saved_ro)
        call insert(text, st)
        call inhibit_read_only_end(saved_ro)
        call set_buffer(old, st)
    end subroutine dired_log

    !> End a bunch of errors: insert the current time and buffer at the
    !> start of the page, and a formfeed at the end.  Port of
    !> dired.el:dired-log (LOG t).
    subroutine dired_log_end()
        type(handle_type) :: old, buffer
        type(value_type) :: saved_ro
        type(status_type) :: st
        character(len=:), allocatable :: output_buffer_name

        old = current_buffer()
        output_buffer_name = buffers(current)%name
        call get_or_make_buffer(dired_log_buffer, buffer, st)
        call set_buffer(buffer, st)
        call goto_character(point_maximum())
        call inhibit_read_only_begin(saved_ro)
        call backward_page()
        if (.not. is_beginning_of_line()) call insert(LF, st)
        call insert(current_time_string()//format_message(TAB//"Buffer `")//output_buffer_name//format_message("'")//LF, st)
        call goto_character(point_maximum())
        call insert(achar(12)//LF, st)
        call inhibit_read_only_end(saved_ro)
        call set_buffer(old, st)
    end subroutine dired_log_end

    !> Move backward to page boundary (page-delimiter "^\f").  Port of
    !> textmodes/page.el:backward-page (COUNT 1).
    subroutine backward_page()
        logical :: found
        integer(int64) :: original_point

        if (is_beginning_of_buffer()) return
        original_point = point()

        if (is_regular_expression_search_backward("^"//achar(12))) then
            if (mend(0) == original_point) then
                call goto_character(mbeg(0))
            else
                call goto_character(original_point)
            end if
        end if

        call goto_character(point() - 1)
        found = is_regular_expression_search_backward("^"//achar(12))

        if (found) then
            call goto_character(mend(0))
        else
            call goto_character(point_minimum())
        end if
    end subroutine backward_page

    !> State a summary of a command's failures, in echo area and log buffer.
    !> STRING is an overall summary of the failures.
    !> FAILURES is a list of file names that we failed to operate on.
    !> Port of dired.el:dired-log-summary.
    subroutine dired_log_summary(string, failures)
        character(len=*), intent(in) :: string
        type(string_type), intent(in) :: failures(:)
        type(handle_type) :: old, buffer
        character(len=:), allocatable :: text, list_text
        type(status_type) :: st
        integer :: k

        if (size(failures) == 1) then
            old = current_buffer()
            buffer = get_buffer(dired_log_buffer)
            call set_buffer(buffer, st)
            call goto_character(point_maximum())
            call backward_page()
            if (is_end_of_line()) call fwd_line(1_int64)
            call buffer_substring(point(), point_maximum(), text, st)
            call set_buffer(old, st)
            call message(text)
        else if (size(failures) == 0) then
            call message(string//"--type ? for details")
        else
            ! (format "%s" failures): a list of strings.
            list_text = "("

            do k = 1, size(failures)
                if (k > 1) list_text = list_text//" "
                list_text = list_text//failures(k)%text
            end do

            list_text = list_text//")"
            call message(string//"--type ? for details "//list_text)
        end if

        ! Log a summary describing a bunch of errors.
        call dired_log(LF//string//LF)
        call dired_log_end()
    end subroutine dired_log_summary

    !> Pop up a buffer with error log output from Dired.
    !> A group of errors from a single command ends with a formfeed.
    !> Thus, use \[backward_page] to find the beginning of a group of
    !> errors.  Port of dired.el:dired-why.
    recursive subroutine dired_why(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: owindow, window, buffer
        type(status_type) :: st

        status = status_type()
        buffer = get_buffer(dired_log_buffer)
        if (buffer%index == 0) return
        owindow = selected_window()
        call display_buffer(buffer, window, status)
        if (.not. status%ok() .or. .not. is_window_live(window)) return
        call select_window(window, st)
        call goto_character(point_maximum())
        call fwd_line(-1_int64)
        call backward_page()
        call recenter(prefix_argument_type(PREFIX_NUMBER, 0_int64), .false., st)
        call select_window(owindow, st)
    end subroutine dired_why

    !> Summarize basic Dired commands and show recent Dired errors.
    !> Port of dired.el:dired-summary.
    recursive subroutine dired_summary(status)
        type(status_type), intent(out) :: status

        call dired_why(status)
        call message(substitute_command_keys("\`d'-elete, \`u'-ndelete, \`x'-punge, \`f'-ind, "// &
                                             "\`o'-ther window, \`R'-ename, \`C'-opy, \`h'-elp"))
    end subroutine dired_summary

    !> Undo in a Dired buffer.
    !> A numeric ARGUMENT serves as a repeat count.
    !> This doesn't recover lost files, it just undoes changes in the buffer itself.
    !> You can use it to recover marks, killed lines or subdirs.  Port of
    !> dired.el:dired-undo.
    recursive subroutine execute_dired_undo(status)
        type(status_type), intent(out) :: status
        type(value_type) :: saved_ro
        type(status_type) :: st

        ! undo-ignore-read-only
        call inhibit_read_only_begin(saved_ro)
        call undo(current_prefix_argument, status)
        call inhibit_read_only_end(saved_ro)
        if (.not. status%ok()) return
        call dired_build_subdirectories(st)
        call message("Change in Dired buffer undone."//LF//"Actual changes in files cannot be undone by Fortmacs.")
    end subroutine execute_dired_undo

    !> Copy names of marked (or next ARGUMENT) files into the clipboard history.
    !> If there are several names, they will be separated by a space,
    !> and file names that have spaces or quote characters in them will
    !> be quoted (with double quotes).  (When there's a single file, no
    !> quoting is done.)
    !>
    !> With a zero prefix arg, use the absolute file name of each marked file.
    !> With \[universal_argument], use the file name relative to the Dired buffer's
    !> `default_directory'.  (This still may contain slashes if in a subdirectory.)
    !>
    !> If on a subdir headerline, use absolute subdirname instead;
    !> prefix arg and marked files are ignored in this case.
    !>
    !> You can then feed the file name(s) to other commands with \[paste].
    !> Port of dired.el:dired-copy-filename-as-kill (a prefix value of 1,
    !> names relative to the project root, needs project.el: deferred).
    recursive subroutine execute_dired_copy_filename(status)
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: files(:)
        character(len=:), allocatable :: subdirectory, string
        integer :: k

        status = status_type()
        subdirectory = dired_get_subdirectory()

        if (len(subdirectory) > 0) then
            call append_string(files, subdirectory)
        else if (current_prefix_argument%kind == PREFIX_NONE) then
            call dired_get_marked_files(files, status, DIRED_LOCALP_NO_DIRECTORY)
        else if (current_prefix_argument%kind == PREFIX_RAW) then
            call dired_get_marked_files(files, status, DIRED_LOCALP_RELATIVE)
        else if (prefix_n() == 0) then
            call dired_get_marked_files(files, status)
        else if (prefix_n() == 1 .and. current_prefix_argument%kind == PREFIX_NUMBER) then
            call set_error(status, GENERIC_ERROR_CODE, "Names relative to the project root (project.el) are "// &
                           "not supported yet")
            return
        else
            call dired_get_marked_files(files, status, DIRED_LOCALP_NO_DIRECTORY, DIRED_ARGUMENT_INTEGER, prefix_n())
        end if

        if (.not. status%ok()) return

        if (size(files) == 1) then
            string = files(1)%text
        else
            string = ""

            do k = 1, size(files)
                if (k > 1) string = string//" "

                if (scan(files(k)%text, " ""'") > 0) then
                    string = string//readable_text(value_type(text=files(k)%text))
                else
                    string = string//files(k)%text
                end if
            end do
        end if

        if (len(string) > 0) then
            if (associated(last_command, commands%cut_region)) then
                call append_to_clipboard_entry(string, .false.)
            else
                call copy_to_clipboard_history(string)
            end if

            call message(string)
        end if

    contains

        logical function symbol_is(v, name)
            type(value_type), intent(in) :: v
            character(len=*), intent(in) :: name

            symbol_is = value_eq(v, value_type(name=name))
        end function symbol_is
    end subroutine execute_dired_copy_filename

    ! ------------------------------------------------------------------
    ! Sorting
    ! ------------------------------------------------------------------

    !> Set mode-line according to option `dired_switches_in_mode_line'.
    !> Port of dired.el:dired-sort-set-mode-line.
    subroutine dired_sort_set_mode_line()
        character(len=:), allocatable :: sw, xs
        logical :: saved

        if (.not. allocated(buffers(current)%major_mode)) return
        if (buffers(current)%major_mode /= "dired_mode") return
        sw = dired_actual_switches_of()
        saved = case_fold_search%value
        case_fold_search%value = .false.

        if (associated(dired_switches_in_mode_line%procedure)) then
            call dired_switches_in_mode_line%procedure(sw, xs)
            buffers(current)%mode_name = "Dired "//xs
        else if (dired_switches_in_mode_line%characters >= 0) then
            xs = sw(1:min(len(sw), dired_switches_in_mode_line%characters))

            if (len(xs) == 0) then
                buffers(current)%mode_name = "Dired"
            else if (len(xs) < len(sw)) then
                buffers(current)%mode_name = "Dired "//xs//char(226)//char(128)//char(166)
            else
                buffers(current)%mode_name = "Dired "//xs
            end if
        else if (dired_switches_in_mode_line%choice == DIRED_SWITCHES_IN_MODE_LINE_SORT_ORDER) then
            if (string_matches(dired_sort_by_name_regular_expression, sw)) then
                buffers(current)%mode_name = "Dired by name"
            else if (string_matches(dired_sort_by_date_regular_expression, sw)) then
                buffers(current)%mode_name = "Dired by date"
            else
                buffers(current)%mode_name = "Dired "//sw
            end if
        else
            buffers(current)%mode_name = "Dired "//sw
        end if

        case_fold_search%value = saved
    end subroutine dired_sort_set_mode_line

    !> Toggle sorting by date, and refresh the Dired buffer.
    !> With a prefix argument, edit the current listing switches instead.
    !> Port of dired.el:dired-sort-toggle-or-edit.
    recursive subroutine execute_dired_sort_toggle_or_edit(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: switches

        status = status_type()

        if (dired_sort_inhibit) then
            call set_error(status, GENERIC_ERROR_CODE, "Cannot sort this Dired buffer")
            return
        end if

        if (current_prefix_argument%kind /= PREFIX_NONE) then
            call read_string("ls switches (must contain -l): ", switches, status, initial_input=dired_actual_switches_of())
            if (.not. status%ok()) return
            call dired_sort_other(switches, status)
        else
            call dired_sort_toggle(status)
        end if
    end subroutine execute_dired_sort_toggle_or_edit

    !> Toggle between sort by date/name.  Reverts the buffer.  Port of
    !> dired.el:dired-sort-toggle.
    recursive subroutine dired_sort_toggle(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: sw, m2, m4, newsw
        character(len=*), parameter :: switch_regular_expression = "\(\`\| \)-\([a-su-zA-Z]*\)\(t\)\([^ ]*\)"
        type(status_type) :: st
        logical :: sorting_by_date, saved
        integer :: k

        status = status_type()
        sw = dired_actual_switches_of()
        saved = case_fold_search%value
        case_fold_search%value = .false.
        sorting_by_date = string_matches(dired_sort_by_date_regular_expression, sw)

        ! Remove the -t switch.
        do while (string_match(switch_regular_expression, sw, st) > 0)
            m2 = match_string(2, sw)
            m4 = match_string(4, sw)

            if (len(m2) == 0 .and. len(m4) == 0) then
                ! Remove a stand-alone -t switch.
                call replace_match("", st, fixedcase=.true., literal=.true., string=sw, result=newsw)
            else
                ! Remove a switch of the form -XtY for some X and Y.
                call replace_match("", st, fixedcase=.true., literal=.true., string=sw, subexp=3, result=newsw)
            end if

            sw = newsw
        end do

        ! Now, if we weren't sorting by date before, add the -t switch.
        ! Some simple-minded ls implementations (eg ftp servers) only allow
        ! a single option string, so try not to add " -t" if possible.
        if (.not. sorting_by_date) then
            if (string_matches("\`-[[:alnum:]]+\'", sw)) then
                sw = sw//"t"
            else
                sw = sw//" -t"
            end if
        end if

        case_fold_search%value = saved
        call make_local_entry(k)
        locals(k)%actual_switches = sw
        call dired_sort_set_mode_line()
        call revert_buffer(status)
    end subroutine dired_sort_toggle

    !> Specify new `ls' SWITCHES for current Dired buffer.
    !> Values matching `dired_sort_by_date_regular_expression' or `dired_sort_by_name_regular_expression'
    !> set the minor mode accordingly, others appear literally in the mode line.
    !> With NO_REVERT, don't refresh the listing afterwards.  Port of
    !> dired.el:dired-sort-other.
    recursive subroutine dired_sort_other(switches, status, no_revert)
        character(len=*), intent(in) :: switches
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_revert
        integer :: k
        logical :: nr

        status = status_type()
        nr = .false.
        if (present(no_revert)) nr = no_revert
        call dired_sort_r_check(switches)
        call make_local_entry(k)
        locals(k)%actual_switches = switches
        call dired_sort_set_mode_line()
        if (.not. nr) call revert_buffer(status)
    end subroutine dired_sort_other

    !> Additional processing of -R in ls option string SWITCHES.
    !> Saves `dired_subdirectories' when R is set and restores saved value
    !> minus any directories explicitly deleted when R is cleared.
    !> To be called first in body of `dired_sort_other', etc.  Port of
    !> dired.el:dired-sort-R-check.
    subroutine dired_sort_r_check(switches)
        character(len=*), intent(in) :: switches
        type(dired_subdirectory_type), allocatable :: kept(:)
        integer :: k, i, j, n

        call make_local_entry(k)

        if (is_dired_switches_recursive(switches) .and. .not. is_dired_switches_recursive(dired_actual_switches_of())) then
            ! Adding -R to ls switches -- save `dired_subdirectories':
            locals(k)%has_pre_r = .true.
            if (allocated(locals(k)%subdirectory_table_pre_r)) deallocate (locals(k)%subdirectory_table_pre_r)
            allocate (locals(k)%subdirectory_table_pre_r(table_size()))

            do i = 1, table_size()
                locals(k)%subdirectory_table_pre_r(i)%text = locals(k)%subdirectory_table(i)%directory
            end do
        else if (is_dired_switches_recursive(dired_actual_switches_of()) .and. &
                 .not. is_dired_switches_recursive(switches)) then
            ! Deleting -R from ls switches -- revert to pre-R subdirs that
            ! are still present:
            if (.not. allocated(locals(k)%subdirectory_table)) return
            allocate (kept(0))

            if (locals(k)%has_pre_r .and. size(locals(k)%subdirectory_table_pre_r) > 0) then
                do i = 1, size(locals(k)%subdirectory_table_pre_r)
                    do j = 1, size(locals(k)%subdirectory_table)
                        if (locals(k)%subdirectory_table(j)%directory == locals(k)%subdirectory_table_pre_r(i)%text) then
                            ! subdir still present...
                            kept = [kept, locals(k)%subdirectory_table(j)]
                            locals(k)%subdirectory_table(j)%marker = null_handle()
                            exit
                        end if
                    end do
                end do

                deallocate (locals(k)%subdirectory_table_pre_r)
                allocate (locals(k)%subdirectory_table_pre_r(0))
            else if (size(locals(k)%subdirectory_table) > 0) then
                ! No pre-R subdir table, so revert to main directory listing:
                n = size(locals(k)%subdirectory_table)
                kept = [kept, locals(k)%subdirectory_table(n)]
                locals(k)%subdirectory_table(n)%marker = null_handle()
            end if

            call destroy_table(locals(k)%subdirectory_table)
            call move_alloc(kept, locals(k)%subdirectory_table)
        end if
    end subroutine dired_sort_r_check

    ! ------------------------------------------------------------------
    ! Font Lock
    ! ------------------------------------------------------------------

    ! Not a port.
    subroutine face_mark(face)
        character(len=:), allocatable, intent(out) :: face

        face = dired_mark_face
    end subroutine face_mark

    ! Not a port.
    subroutine face_marked(face)
        character(len=:), allocatable, intent(out) :: face

        face = dired_marked_face
    end subroutine face_marked

    ! Not a port.
    subroutine face_flagged(face)
        character(len=:), allocatable, intent(out) :: face

        face = dired_flagged_face
    end subroutine face_flagged

    ! Not a port.
    subroutine face_permission_write(face)
        character(len=:), allocatable, intent(out) :: face

        face = dired_permission_write_face
    end subroutine face_permission_write

    ! Not a port.
    subroutine face_directory(face)
        character(len=:), allocatable, intent(out) :: face

        face = dired_directory_face
    end subroutine face_directory

    ! Not a port.
    subroutine face_symlink(face)
        character(len=:), allocatable, intent(out) :: face

        face = dired_symlink_face
    end subroutine face_symlink

    ! Not a port.
    subroutine face_ignored(face)
        character(len=:), allocatable, intent(out) :: face

        face = dired_ignored_face
    end subroutine face_ignored

    ! Not a port.
    subroutine face_header(face)
        character(len=:), allocatable, intent(out) :: face

        face = dired_header_face
    end subroutine face_header

    !> PRE-MATCH-FORM (dired-move-to-filename).  Not a port.
    subroutine pre_move_to_filename(limit)
        integer(int64), intent(out) :: limit
        type(status_type) :: st

        limit = dired_move_to_filename(st)
    end subroutine pre_move_to_filename

    !> PRE-MATCH-FORM of the -F ignored-extension keyword: if the last
    !> character is not part of the filename, move back to the start of the
    !> filename so it can be fontified; otherwise, leave point at the end of
    !> the line; that way, nothing is fontified.  Not a port.
    subroutine pre_ignored_f(limit)
        integer(int64), intent(out) :: limit
        type(value_type) :: v
        type(status_type) :: st

        call end_of_line()
        limit = 0
        call get_text_property(point() - 1, value_type(name="mouse_face"), v, st)
        if (.not. allocated(v%a)) limit = dired_move_to_filename(st)
    end subroutine pre_ignored_f

    !> The truename of the file at point, if it can be found (TRUENAME
    !> "" and FOUND false otherwise); NAME_FOUND tells whether there is
    !> a file name.  Not a port.
    subroutine truename_at_point(truename, found, name_found)
        character(len=:), allocatable, intent(out) :: truename
        logical, intent(out) :: found, name_found
        character(len=:), allocatable :: file
        type(status_type) :: st

        found = .false.
        truename = ""
        call dired_file_name_at_point(file, name_found)
        if (.not. name_found) return
        ! (ignore-errors (file-truename file))
        call file_truename(file, truename, st)
        found = st%ok()
    end subroutine truename_at_point

    !> Search forward for REGULAR_EXPRESSION up to LIMIT, setting FOUND.  Not a port.
    subroutine search_to(regular_expression, limit, found, status)
        character(len=*), intent(in) :: regular_expression
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64) :: position

        call regular_expression_search_forward(regular_expression, position, status, bound=limit, noerror=SEARCH_NOERROR)
        found = status%ok() .and. position > 0
    end subroutine search_to

    !> Matcher of a broken symbolic link: either not existent target or
    !> circular link.  Not a port.
    subroutine broken_symlink_matcher(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: truename
        logical :: have, name_found

        status = status_type()
        found = .false.
        if (.not. dired_check_symlinks) return
        call truename_at_point(truename, have, name_found)

        if (have) then
            if (file_exists(truename)) return
        end if

        call search_to("\(.+\) \(->\) ?\(.+\)", limit, found, status)
    end subroutine broken_symlink_matcher

    !> Matcher of a symbolic link to a directory.  Not a port.
    subroutine directory_symlink_matcher(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: truename
        logical :: have, name_found

        status = status_type()
        found = .false.
        if (.not. dired_check_symlinks) return
        call truename_at_point(truename, have, name_found)
        if (.not. have) return
        if (.not. is_file_directory(truename)) return
        call search_to("\(.+-> ?\)\(.+\)", limit, found, status)
    end subroutine directory_symlink_matcher

    !> Matcher of a symbolic link to a non-directory, or no check at all.
    !> Not a port.
    subroutine other_symlink_matcher(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: truename
        logical :: have, name_found

        status = status_type()
        found = .false.

        if (.not. dired_check_symlinks) then
            call search_to("\(.+-> ?\)\(.+\)", limit, found, status)
            return
        end if

        call truename_at_point(truename, have, name_found)
        if (.not. name_found) return

        if (have) then
            if (is_file_directory(truename)) return
        end if

        call search_to("\(.+-> ?\)\(.+\)", limit, found, status)
    end subroutine other_symlink_matcher

    !> The keywords Dired gives Font Lock: marks, marked and flagged
    !> files, group- and world-writable and set-id permissions,
    !> directories, files with extensions in
    !> `completion_ignored_extensions' (the alternation `regular_expression_optimization' makes,
    !> written out), symbolic links (broken, to directories, to other
    !> files), special files, file names ending in a colon, and directory
    !> headers.  Port of dired.el dired-font-lock-keywords (made for each
    !> buffer by `dired_mode', so the `eval' parts see the current value).
    subroutine dired_font_lock_keywords(kw)
        type(font_lock_keyword_type), allocatable, intent(out) :: kw(:)
        type(font_lock_highlight_type) :: h1(1), h2(2), h3(3), a(1)
        character(len=:), allocatable :: exts, prefix
        character(len=64) :: properties(1)
        integer :: k

        properties(1) = "dired_symlink_filename"
        prefix = dired_regular_expression_maybe_mark//dired_regular_expression_inode_size
        allocate (kw(16))
        ! Dired marks.
        h1(1) = font_lock_highlight(0, face_function=face_mark)
        kw(1) = font_lock_keyword(dired_regular_expression_mark, highlights=h1)
        ! Marked files.
        h1(1) = font_lock_highlight(0, face_function=face_marked)
        a(1) = font_lock_anchored(h1, regular_expression=".+", pre_match_form=pre_move_to_filename)
        kw(2) = font_lock_keyword("^[*]", highlights=a)
        ! Flagged files.
        h1(1) = font_lock_highlight(0, face_function=face_flagged)
        a(1) = font_lock_anchored(h1, regular_expression=".+", pre_match_form=pre_move_to_filename)
        kw(3) = font_lock_keyword("^[D]", highlights=a)
        ! Files that are group or world writable: we don't need to
        ! highlight the file name, only the permissions, to win generally.
        h1(1) = font_lock_highlight(1, face_function=face_permission_write)
        kw(4) = font_lock_keyword(prefix//"[-d]....\(w\)....", highlights=h1)
        kw(5) = font_lock_keyword(prefix//"[-d].......\(w\).", highlights=h1)
        ! suid and guid
        h1(1) = font_lock_highlight(1, face="dired_set_id")
        kw(6) = font_lock_keyword(prefix//"[-d]..\(s\)......", highlights=h1)
        kw(7) = font_lock_keyword(prefix//"[-d].....\([sS]\)...", highlights=h1)
        ! Subdirectories.
        h1(1) = font_lock_highlight(0, face_function=face_directory)
        a(1) = font_lock_anchored(h1, regular_expression=".+", pre_match_form=pre_move_to_filename)
        kw(8) = font_lock_keyword(dired_regular_expression_directory, highlights=a)
        ! Files suffixed with `completion_ignored_extensions'.  It is
        ! quicker to first find just an extension, then go back to the
        ! start of that file name.
        exts = "\(?:"

        do k = 1, size(completion_ignored_extensions)
            if (k > 1) exts = exts//"\|"
            exts = exts//regular_expression_quote(trim(completion_ignored_extensions(k)))
        end do

        exts = exts//"\)"
        h1(1) = font_lock_highlight(0, face_function=face_ignored)
        a(1) = font_lock_anchored(h1, regular_expression=".+", pre_match_form=pre_move_to_filename)
        kw(9) = font_lock_keyword("\("//exts//"\|#\|\.#.+\)$", highlights=a)
        ! Files suffixed with `completion_ignored_extensions' plus a
        ! character put in by -F.
        a(1) = font_lock_anchored(h1, regular_expression=".+", pre_match_form=pre_ignored_f)
        kw(10) = font_lock_keyword("\("//exts//"\|#\|\.#.+\)[*=|]$", highlights=a)
        ! Broken Symbolic link.
        h3(1) = font_lock_highlight(1, face="dired_broken_symlink")
        h3(2) = font_lock_highlight(2, face_function=face_symlink)
        h3(3) = font_lock_highlight(3, face="dired_broken_symlink", true_properties=properties)
        a(1) = font_lock_anchored(h3, matcher=broken_symlink_matcher, pre_match_form=pre_move_to_filename)
        kw(11) = font_lock_keyword(dired_regular_expression_symbol, highlights=a)
        ! Symbolic link to a directory.
        h2(1) = font_lock_highlight(1, face_function=face_symlink)
        h2(2) = font_lock_highlight(2, face_function=face_directory, true_properties=properties)
        a(1) = font_lock_anchored(h2, matcher=directory_symlink_matcher, pre_match_form=pre_move_to_filename)
        kw(12) = font_lock_keyword(dired_regular_expression_symbol, highlights=a)
        ! Symbolic link to a non-directory.  Or no check at all.
        h2(1) = font_lock_highlight(1, face_function=face_symlink)
        h2(2) = font_lock_highlight(2, face="default", true_properties=properties)
        a(1) = font_lock_anchored(h2, matcher=other_symlink_matcher, pre_match_form=pre_move_to_filename)
        kw(13) = font_lock_keyword(dired_regular_expression_symbol, highlights=a)
        ! Sockets, pipes, block devices, char devices.
        h1(1) = font_lock_highlight(0, face="dired_special")
        a(1) = font_lock_anchored(h1, regular_expression=".+", pre_match_form=pre_move_to_filename)
        kw(14) = font_lock_keyword(dired_regular_expression_special, highlights=a)
        ! Explicitly put the default face on file names ending in a colon
        ! to avoid fontifying them as directory header.
        h1(1) = font_lock_highlight(0, face="default")
        a(1) = font_lock_anchored(h1, regular_expression=".+", pre_match_form=pre_move_to_filename)
        kw(15) = font_lock_keyword(prefix//dired_regular_expression_permissions//".*:$", highlights=a)
        ! Directory headers.
        h1(1) = font_lock_highlight(1, face_function=face_header)
        kw(16) = font_lock_keyword(dired_subdirectory_regular_expression, highlights=h1)
    end subroutine dired_font_lock_keywords

    !> Define the Dired faces.  Not a port.
    subroutine initialize_dired_faces()
        integer, parameter :: C = FACE_CLASS_COLOR
        type(named_face_type), pointer :: f

        f => dired_header
        call f%define("dired_header", "Face used for directory headers.")
        call f%specification_entry(display_when(), face_attributes(inherit="font_lock_type_face"))
        f => dired_mark
        call f%define("dired_mark", "Face used for Dired marks.")
        call f%specification_entry(display_when(), face_attributes(inherit="font_lock_constant_face"))
        f => dired_marked
        call f%define("dired_marked", "Face used for marked files.")
        call f%specification_entry(display_when(), face_attributes(inherit="warning"))
        f => dired_flagged
        call f%define("dired_flagged", "Face used for files flagged for deletion.")
        call f%specification_entry(display_when(), face_attributes(inherit="error"))
        f => dired_warning
        call f%define("dired_warning", "Face used to highlight a part of a buffer that needs user attention.")
        call f%specification_entry(display_when(), face_attributes(inherit="font_lock_warning_face"))
        f => dired_permission_write
        call f%define("dired_permission_write", "Face used to highlight permissions of group- and world-writable files.")
        call f%specification_entry(display_when(types=[character(len=3) :: "w32", "pc"]), face_attributes(inherit="default"))
        call f%specification_entry(display_when(), face_attributes(inherit="font_lock_comment_delimiter_face"))
        f => dired_set_id
        call f%define("dired_set_id", "Face used to highlight permissions of suid and guid files.")
        call f%specification_entry(display_when(types=[character(len=3) :: "w32", "pc"]), face_attributes(inherit="default"))
        call f%specification_entry(display_when(), face_attributes(inherit="font_lock_warning_face"))
        f => dired_directory
        call f%define("dired_directory", "Face used for subdirectories.")
        call f%specification_entry(display_when(), face_attributes(inherit="font_lock_function_name_face"))
        f => dired_symlink
        call f%define("dired_symlink", "Face used for symbolic links.")
        call f%specification_entry(display_when(), face_attributes(inherit="font_lock_keyword_face"))
        f => dired_broken_symlink
        call f%define("dired_broken_symlink", "Face used for broken symbolic links.")
        call f%specification_entry(display_when(classes=[C]), face_attributes(foreground="yellow1", background="red1", &
                                                                              weight="bold"))
        call f%specification_entry(display_when(), face_attributes(weight="bold", slant="italic", underline=.true.))
        f => dired_special
        call f%define("dired_special", "Face used for sockets, pipes, block devices and char devices.")
        call f%specification_entry(display_when(), face_attributes(inherit="font_lock_variable_name_face"))
        f => dired_ignored
        call f%define("dired_ignored", "Face used for files suffixed with `completion_ignored_extensions'.")
        call f%specification_entry(display_when(), face_attributes(inherit="shadow"))
    end subroutine initialize_dired_faces

    ! ------------------------------------------------------------------
    ! Commands that wrap the functions
    ! ------------------------------------------------------------------

    !> Build `dired_subdirectories' by parsing the buffer.
    !> Returns the new value of `dired_subdirectories'.
    !> Port of dired.el:dired-build-subdir-alist (interactive).
    recursive subroutine execute_dired_build_subdirectories(status)
        type(status_type), intent(out) :: status

        call dired_build_subdirectories(status, called_interactively=.true.)
    end subroutine execute_dired_build_subdirectories

    !> In Dired, visit the file or directory named on this line.  Port of
    !> dired.el:dired-find-file (interactive).
    recursive subroutine execute_dired_find_file(status)
        type(status_type), intent(out) :: status

        call dired_find_file(status)
    end subroutine execute_dired_find_file

    !> In Dired, visit this file or directory in another window.
    !> If this command needs to split the current window, it by default obeys
    !> the user options `split_height_threshold' and `split_width_threshold',
    !> when it decides whether to split the window horizontally or vertically.
    !> Port of dired.el:dired-find-file-other-window (interactive).
    recursive subroutine execute_dired_find_file_other_window(status)
        type(status_type), intent(out) :: status

        call dired_find_file_other_window(status)
    end subroutine execute_dired_find_file_other_window

    !> In Dired, display this file or directory in another window.  Port of
    !> dired.el:dired-display-file (interactive).
    recursive subroutine execute_dired_display_file(status)
        type(status_type), intent(out) :: status

        call dired_display_file(status)
    end subroutine execute_dired_display_file

    !> Toggle marks: marked files become unmarked, and vice versa.
    !> Flagged files (indicated with flags such as `C' and `D', not
    !> with `*') are not affected, and `.' and `..' are never toggled.
    !> As always, hidden subdirs are not affected.
    !>
    !> In Transient Mark mode, if the mark is active, operate on the contents
    !> of the region if `dired_mark_region' is true.  Otherwise, operate
    !> on the whole buffer.  Port of dired.el:dired-toggle-marks
    !> (interactive).
    recursive subroutine execute_dired_toggle_marks(status)
        type(status_type), intent(out) :: status

        call dired_toggle_marks(status)
    end subroutine execute_dired_toggle_marks

    !> Display the number and total size of the marked files.  Port of
    !> dired.el:dired-number-of-marked-files (interactive).
    recursive subroutine execute_dired_number_of_marked_files(status)
        type(status_type), intent(out) :: status

        call dired_number_of_marked_files(status)
    end subroutine execute_dired_number_of_marked_files

    !> Pop up a buffer with error log output from Dired.
    !> A group of errors from a single command ends with a formfeed.
    !> Thus, use \[backward_page] to find the beginning of a group of
    !> errors.  Port of dired.el:dired-why (interactive).
    recursive subroutine execute_dired_why(status)
        type(status_type), intent(out) :: status

        call dired_why(status)
    end subroutine execute_dired_why

    !> Summarize basic Dired commands and show recent Dired errors.
    !> Port of dired.el:dired-summary (interactive).
    recursive subroutine execute_dired_summary(status)
        type(status_type), intent(out) :: status

        call dired_summary(status)
    end subroutine execute_dired_summary

    ! ------------------------------------------------------------------
    ! Setup
    ! ------------------------------------------------------------------

    !> Bind KEYS (key-parse syntax) to COMMAND in `dired_mode_map'.
    !> Not a port.
    subroutine bind(keys, command)
        character(len=*), intent(in) :: keys, command
        type(status_type) :: st

        call keymap_set(dired_mode_map, keys, command, st)
    end subroutine bind

    !> Bind [remap FROM] to COMMAND in `dired_mode_map'.  Not a port.
    subroutine bind_remap(from, command)
        character(len=*), intent(in) :: from, command
        type(value_type), allocatable :: events(:)
        type(status_type) :: st

        call list_elements(list(value_type(name="remap"), value_type(name=from)), events)
        call define_key(dired_mode_map, events, value_type(name=command), st)
    end subroutine bind_remap

    !> Define Dired: its options, faces, commands, Dired mode and
    !> `dired_mode_map', and the keys `C-x d' in CONTROL_X_MAP and `C-x 4 d'
    !> in CONTROL_X_4_MAP.  Not a port.
    subroutine initialize_dired_mode(control_x_map, control_x_4_map, control_x_5_map, tab_prefix_map)
        type(handle_type), intent(in), optional :: control_x_map, control_x_4_map, control_x_5_map, tab_prefix_map
        type(status_type) :: st

        if (.not. allocated(dired_listing_switches)) dired_listing_switches = "-al"
        if (.not. allocated(dired_trivial_filenames)) dired_trivial_filenames = "\`\.\.?\'\|\`\.?#"

        if (.not. allocated(dired_garbage_files_regular_expression)) then
            dired_garbage_files_regular_expression = "\(?:\.\(?:aux\|bak\|dvi\|log\|orig\|rej\|toc\)\)\'"
        end if

        if (.not. allocated(dired_header_face)) dired_header_face = "dired_header"
        if (.not. allocated(dired_mark_face)) dired_mark_face = "dired_mark"
        if (.not. allocated(dired_marked_face)) dired_marked_face = "dired_marked"
        if (.not. allocated(dired_flagged_face)) dired_flagged_face = "dired_flagged"
        if (.not. allocated(dired_warning_face)) dired_warning_face = "dired_warning"
        if (.not. allocated(dired_permission_write_face)) dired_permission_write_face = "dired_permission_write"
        if (.not. allocated(dired_directory_face)) dired_directory_face = "dired_directory"
        if (.not. allocated(dired_symlink_face)) dired_symlink_face = "dired_symlink"
        if (.not. allocated(dired_ignored_face)) dired_ignored_face = "dired_ignored"
        if (.not. associated(dired_deletion_confirmer)) dired_deletion_confirmer => read_yes_or_no
        if (.not. allocated(dired_buffers)) allocate (dired_buffers(0))
        call initialize_dired_faces()
        call append_locals_clearer(clear_dired_locals)
        find_directory_functions => dired_noselect_directory
        ! Commands.
        commands%dired%execute => execute_dired
        commands%dired_other_window%execute => execute_dired_other_window
        commands%dired_other_frame%execute => execute_dired_other_frame
        commands%dired_other_tab%execute => execute_dired_other_tab
        commands%dired_mode%execute => dired_mode_command
        commands%dired_mode%is_command = .false.
        commands%dired_next_line%execute => execute_dired_next_line
        commands%dired_previous_line%execute => execute_dired_previous_line
        commands%dired_next_directory_line%execute => execute_dired_next_directory_line
        commands%dired_previous_directory_line%execute => execute_dired_previous_directory_line
        commands%dired_next_subdirectory%execute => execute_dired_next_subdirectory
        commands%dired_previous_subdirectory%execute => execute_dired_previous_subdirectory
        commands%dired_up_directory%execute => execute_dired_up_directory
        commands%dired_find_file%execute => execute_dired_find_file
        commands%dired_find_file%advertised_binding = "RET"
        commands%dired_find_file_other_window%execute => execute_dired_find_file_other_window
        commands%dired_display_file%execute => execute_dired_display_file
        commands%dired_goto_file%execute => execute_dired_goto_file
        commands%dired_mark%execute => execute_dired_mark
        commands%dired_unmark%execute => execute_dired_unmark
        commands%dired_flag_file_deletion%execute => execute_dired_flag_file_deletion
        commands%dired_unmark_backward%execute => execute_dired_unmark_backward
        commands%dired_toggle_marks%execute => execute_dired_toggle_marks
        commands%dired_mark_files_regular_expression%execute => execute_dired_mark_files_regular_expression
        commands%dired_mark_files_containing_regular_expression%execute => execute_dired_mark_files_containing_regular_expression
        commands%dired_flag_files_regular_expression%execute => execute_dired_flag_files_regular_expression
        commands%dired_flag_garbage_files%execute => execute_dired_flag_garbage_files
        commands%dired_mark_symlinks%execute => execute_dired_mark_symlinks
        commands%dired_mark_directories%execute => execute_dired_mark_directories
        commands%dired_mark_executables%execute => execute_dired_mark_executables
        commands%dired_flag_auto_save_files%execute => execute_dired_flag_auto_save_files
        commands%dired_flag_backup_files%execute => execute_dired_flag_backup_files
        commands%dired_change_marks%execute => execute_dired_change_marks
        commands%dired_unmark_all_files%execute => execute_dired_unmark_all_files
        commands%dired_unmark_all_marks%execute => execute_dired_unmark_all_marks
        commands%dired_next_marked_file%execute => execute_dired_next_marked_file
        commands%dired_previous_marked_file%execute => execute_dired_previous_marked_file
        commands%dired_number_of_marked_files%execute => execute_dired_number_of_marked_files
        commands%dired_do_flagged_delete%execute => execute_dired_do_flagged_delete
        commands%dired_do_delete%execute => execute_dired_do_delete
        commands%dired_why%execute => execute_dired_why
        commands%dired_summary%execute => execute_dired_summary
        commands%dired_undo%execute => execute_dired_undo
        commands%dired_copy_filename%execute => execute_dired_copy_filename
        commands%dired_sort_toggle_or_edit%execute => execute_dired_sort_toggle_or_edit
        commands%dired_build_subdirectories%execute => execute_dired_build_subdirectories
        call initialize_dired_aux()
        ! Dired mode, with a full keymap whose parent is special-mode-map.
        if (.not. is_keymap_live(dired_mode_map)) call make_keymap(dired_mode_map)
        major_modes%dired_mode%map = dired_mode_map
        call major_modes%dired_mode%define("dired_mode", "Dired", dired_mode_command, dired_mode_hook)
        major_modes%dired_mode%special = .true.
        call set_keymap_parent(dired_mode_map, special_mode_map, st)
        ! Commands to mark or flag certain categories of files
        call bind("#", "dired_flag_auto_save_files")
        call bind("~", "dired_flag_backup_files")
        ! Upper case keys (except !) for operating on the marked files
        call bind("D", "dired_do_delete")
        ! Tree Dired commands
        call bind("M-DEL", "dired_unmark_all_files")
        call bind("C-M-n", "dired_next_subdirectory")
        call bind("C-M-p", "dired_previous_subdirectory")
        ! move to marked files
        call bind("M-{", "dired_previous_marked_file")
        call bind("M-}", "dired_next_marked_file")
        ! Make all regexp commands share a `%' prefix:
        call bind("% d", "dired_flag_files_regular_expression")
        call bind("% g", "dired_mark_files_containing_regular_expression")
        call bind("% m", "dired_mark_files_regular_expression")
        call bind("% &", "dired_flag_garbage_files")
        ! Commands for marking and unmarking.
        call bind("* *", "dired_mark_executables")
        call bind("* /", "dired_mark_directories")
        call bind("* @", "dired_mark_symlinks")
        call bind("* %", "dired_mark_files_regular_expression")
        call bind("* N", "dired_number_of_marked_files")
        call bind("* c", "dired_change_marks")
        call bind("* s", "dired_mark_subdirectory_files")
        call bind("* m", "dired_mark")
        call bind("* u", "dired_unmark")
        call bind("* ?", "dired_unmark_all_files")
        call bind("* !", "dired_unmark_all_marks")
        call bind("U", "dired_unmark_all_marks")
        call bind("* DEL", "dired_unmark_backward")
        call bind("* C-n", "dired_next_marked_file")
        call bind("* C-p", "dired_previous_marked_file")
        call bind("* t", "dired_toggle_marks")
        ! Lower keys for commands not operating on all the marked files
        call bind("d", "dired_flag_file_deletion")
        call bind("e", "dired_find_file")
        call bind("f", "dired_find_file")
        call bind("C-m", "dired_find_file")
        call bind("g", "revert_buffer")
        call bind("j", "dired_goto_file")
        call bind("l", "dired_do_redisplay")
        call bind("m", "dired_mark")
        call bind("n", "dired_next_line")
        call bind("o", "dired_find_file_other_window")
        call bind("C-o", "dired_display_file")
        call bind("p", "dired_previous_line")
        call bind("s", "dired_sort_toggle_or_edit")
        call bind("t", "dired_toggle_marks")
        call bind("u", "dired_unmark")
        call bind("w", "dired_copy_filename")
        call bind("x", "dired_do_flagged_delete")
        call bind("+", "dired_create_directory")
        ! moving
        call bind("<", "dired_previous_directory_line")
        call bind(">", "dired_next_directory_line")
        call bind("^", "dired_up_directory")
        call bind("SPC", "dired_next_line")
        call bind("S-SPC", "dired_previous_line")
        call bind_remap("next_line", "dired_next_line")
        call bind_remap("previous_line", "dired_previous_line")
        ! misc
        call bind("?", "dired_summary")
        call bind("DEL", "dired_unmark_backward")
        call bind_remap("undo", "dired_undo")
        call dired_aux_keys()
        call register_keymap_name("dired_mode_map", dired_mode_map)
        ! (keymap-set ctl-x-map "d" #'dired)
        if (present(control_x_map)) call keymap_set(control_x_map, "d", "dired", st)
        ! (keymap-set ctl-x-4-map "d" #'dired-other-window)
        if (present(control_x_4_map)) call keymap_set(control_x_4_map, "d", "dired_other_window", st)
        if (present(control_x_5_map)) call keymap_set(control_x_5_map, "d", "dired_other_frame", st)
        ! (keymap-set tab-prefix-map "d" #'dired-other-tab)
        if (present(tab_prefix_map)) call keymap_set(tab_prefix_map, "d", "dired_other_tab", st)
    end subroutine initialize_dired_mode

    !> Forget the procedure of `dired_do_what_i_mean_target', as any assignment to it does.
    !> Not a port.
    subroutine dired_do_what_i_mean_target_clear_kinds(self)
        class(dired_do_what_i_mean_target_choice_setting_type), intent(inout) :: self

        nullify (self%procedure)
    end subroutine dired_do_what_i_mean_target_clear_kinds

    !> The procedure of `dired_do_what_i_mean_target' as help shows it.  Not a port.
    function dired_do_what_i_mean_target_other_value(self) result(text)
        class(dired_do_what_i_mean_target_choice_setting_type), intent(in) :: self
        character(len=:), allocatable :: text

        text = ""
        if (associated(self%procedure)) text = "a procedure set by the configuration"
    end function dired_do_what_i_mean_target_other_value

    !> Forget the number and the procedure of `dired_switches_in_mode_line',
    !> as any assignment to it does.  Not a port.
    subroutine dired_switches_in_mode_line_clear_kinds(self)
        class(dired_switches_in_mode_line_choice_setting_type), intent(inout) :: self

        self%characters = -1
        nullify (self%procedure)
    end subroutine dired_switches_in_mode_line_clear_kinds

    !> The number or the procedure of `dired_switches_in_mode_line' as help
    !> shows it; "" when neither is set.  Not a port.
    function dired_switches_in_mode_line_other_value(self) result(text)
        class(dired_switches_in_mode_line_choice_setting_type), intent(in) :: self
        character(len=:), allocatable :: text
        character(len=40) :: digits

        text = ""

        if (associated(self%procedure)) then
            text = "a procedure set by the configuration"
        else if (self%characters >= 0) then
            write (digits, "(i0)") self%characters
            text = "%characters = "//trim(digits)
        end if
    end function dired_switches_in_mode_line_other_value

    !> The assignment that sets the number or procedure `dired_switches_in_mode_line' takes, for the error of a rejected
    !> value.  Not a port.
    function dired_switches_in_mode_line_kind_hint(self, number) result(text)
        class(dired_switches_in_mode_line_choice_setting_type), intent(in) :: self
        character(len=*), intent(in) :: number
        character(len=:), allocatable :: text

        text = "a number of characters is `"//trim(self%name)//"%characters = "//number//"'"
    end function dired_switches_in_mode_line_kind_hint

    !> The assignment that sets the number or procedure `dired_do_what_i_mean_target' takes, for the error of a rejected
    !> value.  Not a port.
    function dired_do_what_i_mean_target_kind_hint(self, number) result(text)
        class(dired_do_what_i_mean_target_choice_setting_type), intent(in) :: self
        character(len=*), intent(in) :: number
        character(len=:), allocatable :: text

        text = "a procedure is `"//trim(self%name)//"%procedure => mine'"
        if (len(number) < 0) text = number
    end function dired_do_what_i_mean_target_kind_hint
end module directory_editor_mode_mod
