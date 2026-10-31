! ibuffer_fm.f90 -- operate on buffers like dired.
! Ported from GNU Emacs 31.1 lisp/ibuffer.el.
!
! Copyright (C) 2000-2026 Free Software Foundation, Inc.
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

!> Ibuffer is an advanced replacement for the `buffer_menu' which is
!> normally distributed with Emacs.  Its interface is intended to be
!> analogous to that of Dired.  To start using it, type `M-x ibuffer'.
!>
!> The columns of `ibuffer_formats' are drawn by interpreting the format
!> (upstream compiles each format into a Lisp function; the result is
!> the same).  A line's buffer is its `ibuffer_properties' text property
!> (the buffer's serial number) and its mark the `ibuffer_mark' property
!> (upstream both in `ibuffer-properties').  ibuf-ext.el is the
!> submodule ibuf_ext_fm; ibuf-macs.el's definitions are the types of
!> ibuf_macs_fm.
!>
!> Deferred: the menus and mouse commands (`ibuffer_mouse_toggle_mark',
!> `ibuffer_mouse_visit_buffer', `ibuffer_mouse_popup_menu', the header
!> and name keymaps, among them the mode name's (RET and mouse-2 run
!> `ibuffer_interactive_filter_by_mode'), help-echo, the filter groups'
!> help-echo with their filters), frames
!> (`ibuffer_visit_buffer_other_frame', `ibuffer_do_view_other_frame'),
!> `ibuffer_visit_tags_table' (etags.el), locked buffers (emacs-lock.el:
!> the locked column is blank, `ibuffer_do_toggle_lock' and
!> `ibuffer_mark_by_locked' do nothing), `ibuffer_customize' (cus-edit.el),
!> `truncate_lines', the enlarging of a window too small to split for
!> `ibuffer_confirm_operation_on', a buffer name with newlines shown
!> with "^J" in the `escape_glyph' face (the "^J" takes the name's face),
!> and the obsolete `ibuffer_load_hook'.
module ibuffer_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR, FM_QUIT, format_message
    use handle_fm, only: handle_type, null_handle
    use lisp_fm, only: value_type, nilp, fixnump, xfixnum, make_fixnum, stringp, xstring, make_string, t_value, &
                       list, consp
    use lread_fm, only: sym
    use eval_fm, only: hook_type, change_major_mode_hook
    use data_fm, only: add_local_variables_killer
    use modes_fm, only: major_modes, minor_modes
    use commands_fm, only: commands
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, get_buffer_create, get_buffer, is_buffer_live, &
                         buffer_handle, buffer_list, buffer_serial_live, kill_buffer, is_buffer_modified, &
                         restore_buffer_modified, erase_buffer
    use insdel_fm, only: insert
    use editfns_fm, only: point, goto_char, point_min, point_max, is_eob, is_bob, pos_bol, pos_eol, delete_region, &
                          buffer_size, buffer_substring, following_char, point_marker
    use marker_fm, only: marker_position, free_marker
    use cmds_fm, only: forward_line, beginning_of_line
    use textprop_fm, only: get_text_property, put_text_property, next_single_property_change, &
                           previous_single_property_change
    use search_fm, only: string_match, regexp_quote, re_search_forward
    use keyboard_fm, only: read_event
    use callint_fm, only: current_prefix_arg, prefix_numeric_value
    use xdisp_fm, only: message, mode_line_process_text, cursor_in_echo_area, set_mode_line_process, &
                        mode_line_part_type
    use dispnew_fm, only: ding
    use help_fm, only: substitute_command_keys
    use files_fm, only: save_buffer, abbreviate_file_name, file_size_human_readable, revert_buffer_function, &
                        find_file
    use fileio_fm, only: expand_file_name
    use fill_fm, only: string_width
    use mule_util_fm, only: truncate_string_to_width
    use shell_fm, only: list_buffers_directory
    use dired_mode_fm, only: dired_directory_of
    use derived_fm, only: is_derived_mode
    use process_fm, only: get_buffer_process, process_status, process_name
    use frame_fm, only: selected_frame, selected_window, frame_lines, frame_cols
    use window_fm, only: windows, window_text_cols, switch_to_buffer, switch_to_buffer_other_window, display_buffer, &
                         display_action_table_type, delete_other_windows, split_window, other_window, bury_buffer, &
                         pop_to_buffer, select_window, get_buffer_window, fit_window_to_buffer, window_leaves, is_window_live, &
                         window_configuration_type, current_window_configuration, set_window_configuration
    use view_fm, only: view_buffer
    use subr_fm, only: read_y_or_n
    use minibuf_fm, only: is_minibuffer
    use minibuffer_fm, only: read_file_name
    use simple_fm, only: count_lines, is_use_region
    use keymap_fm, only: keymap_set
    use time_date_fm, only: seconds_to_string, SECONDS_TO_STRING_READABLE
    use sysdep_fm, only: emacs_clock_realtime
    use faces_fm, only: named_face_type, display_when, face_attrs, FACE_BACKGROUND_DARK
    use ibuf_macs_fm, only: ibuffer_column, ibuffer_text, ibuffer_formats_type, ibuffer_format_type, &
                            ibuffer_column_spec_type, ibuffer_column_type, ibuffer_cell_type, ibuffer_sorter_type, &
                            ibuffer_op_type, ibuffer_show_predicate_type, ibuffer_fontification_type, &
                            ibuffer_directory_abbrev_type, ibuffer_column_proc, ibuffer_summarizer_proc, &
                            ibuffer_sorter_proc, ibuffer_form_proc, IBUFFER_ALIGN_LEFT, IBUFFER_ALIGN_CENTER, &
                            IBUFFER_ALIGN_RIGHT, IBUFFER_OP_NIL, IBUFFER_OP_T, IBUFFER_OP_KILL, IBUFFER_MODIFIER_NIL, &
                            IBUFFER_MODIFIER_T, IBUFFER_MODIFIER_MAYBE, IBUFFER_NAME_LEN, ibuffer_filter_type, &
                            ibuffer_saved_filters_type, ibuffer_saved_filter_type, ibuffer_filter_groups_type, &
                            ibuffer_saved_filter_groups_type, ibuffer_filter_def_type, ibuffer_filter_formats_type, &
                            ibuffer_filter_format_type, ibuffer_or, ibuffer_and, ibuffer_not, ibuffer_major_mode, &
                            ibuffer_derived_mode, ibuffer_starred_name, ibuffer_filename, ibuffer_buffer_predicate_proc, &
                            ibuffer_filter_test_proc, IBUFFER_FILTER_SIMPLE, IBUFFER_FILTER_NOT, IBUFFER_FILTER_AND, &
                            IBUFFER_FILTER_OR, IBUFFER_FILTER_SAVED
    implicit none(type, external)
    private

    public :: ibuffer, ibuffer_mode, ibuffer_other_window, ibuffer_list_buffers, ibuffer_jump, ibuffer_update, &
              ibuffer_redisplay, ibuffer_forward_line, ibuffer_backward_line, ibuffer_current_buffer, &
              ibuffer_current_mark, ibuffer_set_mark, ibuffer_mark_forward, ibuffer_unmark_forward, &
              ibuffer_unmark_backward, ibuffer_mark_for_delete, ibuffer_mark_for_delete_backwards, ibuffer_unmark_all, &
              ibuffer_unmark_all_marks, ibuffer_toggle_marks, ibuffer_change_marks, ibuffer_map_lines, &
              ibuffer_buffer_names_with_mark, ibuffer_marked_buffer_names, ibuffer_deletion_marked_buffer_names, &
              ibuffer_count_marked_lines, ibuffer_get_marked_buffers, ibuffer_current_state_list, ibuffer_switch_format, &
              ibuffer_visit_buffer, ibuffer_visit_buffer_other_window, ibuffer_visit_buffer_other_window_noselect, &
              ibuffer_visit_buffer_1_window, ibuffer_bury_buffer, ibuffer_do_view, ibuffer_do_view_horizontally, &
              ibuffer_find_file, ibuffer_do_save, ibuffer_do_toggle_modified, ibuffer_do_toggle_read_only, &
              ibuffer_do_delete, ibuffer_do_kill_on_deletion_marks, ibuffer_recompile_formats, is_ibuffer_hidden_buffer, &
              ibuffer_operate, ibuffer_confirm_operation_on, ibuffer_buffer_file_name, ibuffer_save_marks_begin, &
              ibuffer_save_marks_end, ibuffer_state_type, syms_of_ibuffer
    public :: define_ibuffer_column, define_ibuffer_sorter, ibuffer_do_sort_by
    public :: define_ibuffer_filter, is_ibuffer_included_in_filters, is_ibuffer_included_in_filter, ibuffer_unary_operand, &
              ibuffer_format_qualifier, ibuffer_push_filter, ibuffer_filter_disable, ibuffer_pop_filter, &
              ibuffer_decompose_filter, ibuffer_exchange_filters, ibuffer_negate_filter, ibuffer_or_filter, &
              ibuffer_and_filter, ibuffer_save_filters, ibuffer_delete_saved_filters, ibuffer_add_saved_filters, &
              ibuffer_switch_to_saved_filters, ibuffer_filters_to_filter_group, ibuffer_set_filter_groups_by_mode, &
              ibuffer_pop_filter_group, ibuffer_decompose_filter_group, ibuffer_clear_filter_groups, &
              ibuffer_kill_filter_group, ibuffer_yank_filter_group, ibuffer_save_filter_groups, &
              ibuffer_delete_saved_filter_groups, ibuffer_switch_to_saved_filter_groups, ibuffer_filter_by_mode, &
              ibuffer_filter_by_used_mode, ibuffer_filter_by_derived_mode, ibuffer_filter_by_name, &
              ibuffer_filter_by_process, ibuffer_filter_by_starred_name, ibuffer_filter_by_filename, &
              ibuffer_filter_by_basename, ibuffer_filter_by_file_extension, ibuffer_filter_by_directory, &
              ibuffer_filter_by_size_gt, ibuffer_filter_by_size_lt, ibuffer_filter_by_modified, &
              ibuffer_filter_by_visiting_file, ibuffer_filter_by_content, ibuffer_filter_by_predicate, &
              ibuffer_list_buffer_modes, ibuffer_filtering_qualifiers, ibuffer_set_filtering_qualifiers, &
              ibuffer_filter_groups, ibuffer_set_filter_groups
    ! (Public for the submodule ibuf_ext_fm: gfortran emits no private
    ! procedure that only a submodule uses.)
    public :: buf_matches_predicates, buf_matches_regexps, form_compressed, has_prop, has_string_prop, &
              ibuffer_abbreviate_file_name, ibuffer_set_mark_1, jump_to_buffer_by_name, line_serial, map_marked_lines, &
              mode_name_of, serial_buffer, string_prop, update_mode_line_process, ibuffer_local_index, filter_symbol
    public :: ibuffer_column, ibuffer_text

    !> The values of `ibuffer_always_show_last_buffer'.
    integer, parameter, public :: IBUFFER_SHOW_LAST_NEVER = 0, IBUFFER_SHOW_LAST_ALWAYS = 1, &
                                  IBUFFER_SHOW_LAST_NOMINI = 2
    !> The values of `ibuffer_use_header_line'.
    integer, parameter, public :: IBUFFER_HEADER_LINE_NIL = 0, IBUFFER_HEADER_LINE_T = 1, &
                                  IBUFFER_HEADER_LINE_TITLE = 2
    !> The values of the SHRINK argument of `ibuffer'
    !> (`ibuffer-shrink-to-minimum-size').
    integer, parameter, public :: IBUFFER_SHRINK_NIL = 0, IBUFFER_SHRINK_T = 1, IBUFFER_SHRINK_ONEWINDOW = 2

    ! What a mapping function returns for a buffer (`ibuffer_map_lines').
    integer, parameter :: MAP_NIL = IBUFFER_OP_NIL, MAP_T = IBUFFER_OP_T, MAP_KILL = IBUFFER_OP_KILL

    !> A list of ways to display buffer lines.
    !>
    !> With Ibuffer, you are not limited to displaying just certain
    !> attributes of a buffer such as size, name, and mode in a particular
    !> order.  Through this variable, you can completely customize and
    !> control the appearance of an Ibuffer buffer.  See also
    !> `ibuffer_columns', which allows you to define your own columns
    !> for display.
    !>
    !> Each format added with `ibuffer_formats%add' is an array of
    !> entries: `ibuffer_column(NAME, MIN_SIZE, MAX_SIZE, ALIGN, ELIDE)'
    !> names a column, `ibuffer_text(STRING)' is a literal string to
    !> display.  Predefined columns are: mark modified read_only locked
    !> name size mode process filename.  MIN_SIZE and MAX_SIZE constrict
    !> the size of a column.  If MAX_SIZE is -1, there is no upper bound.
    !> The default values are 0 and -1, respectively.  If MIN_SIZE is
    !> negative, use the end of the string.  ALIGN describes the alignment
    !> of the column (IBUFFER_ALIGN_LEFT, IBUFFER_ALIGN_CENTER or
    !> IBUFFER_ALIGN_RIGHT); ELIDE describes whether or not to elide the
    !> column if it is too long.  The default is false (don't elide).
    !>
    !> Some examples of valid formats:
    !>
    !>  [ibuffer_column("mark"), ibuffer_text(" "), ibuffer_column("name")]
    !>   This format just displays the current mark (if any) and the name of
    !>   the buffer, separated by a space.
    !>  [ibuffer_column("mark"), ibuffer_column("modified"),
    !>   ibuffer_column("read_only"), ibuffer_text(" "),
    !>   ibuffer_column("name", 16, 16, IBUFFER_ALIGN_LEFT), ibuffer_text(" "),
    !>   ibuffer_column("size", 6, -1, IBUFFER_ALIGN_RIGHT)]
    !>   This format displays the current mark (if any), its modification
    !>   and read-only status, as well as the name of the buffer and its
    !>   size.  In this format, the name is restricted to 16 characters
    !>   (longer names will be truncated, and shorter names will be padded
    !>   with spaces), and the name is also aligned to the left.  The size
    !>   of the buffer will be padded with spaces up to a minimum of six
    !>   characters, but there is no upper limit on its size.  The size
    !>   will also be aligned to the right.
    !>
    !> Using \[ibuffer_switch_format], you can rotate the display between
    !> the specified formats in the list.  Port of ibuffer.el
    !> ibuffer-formats.
    type(ibuffer_formats_type), public :: ibuffer_formats

    !> If true, then use the byte-compiler to optimize `ibuffer_formats'.
    !> This will increase the redisplay speed, at the cost of loading the
    !> elisp byte-compiler.  (Fortmacs interprets the formats; the value
    !> has no effect.)  Port of ibuffer.el ibuffer-always-compile-formats.
    logical, public :: ibuffer_always_compile_formats = .false.

    !> An array describing how to fontify buffers.
    !> Each element has a PRIORITY, a FORM (a function evaluated in the
    !> buffer), and the FACE to use for fontification.  If the FORM
    !> returns true, then FACE will be put on the buffer name.  The
    !> element with the highest PRIORITY takes precedence.
    !>
    !> If you change this variable, you must kill the Ibuffer buffer and
    !> recreate it for the change to take effect.  Port of ibuffer.el
    !> ibuffer-fontification-alist.
    type(ibuffer_fontification_type), allocatable, public :: ibuffer_fontification_table(:)

    !> Show buffer sizes in human-readable format.
    !> Use the function `file_size_human_readable' for formatting.  Port
    !> of ibuffer.el ibuffer-human-readable-size.
    logical, public :: ibuffer_human_readable_size = .false.

    !> If true, display Ibuffer in another window by default.  Port of
    !> ibuffer.el ibuffer-use-other-window.
    logical, public :: ibuffer_use_other_window = .false.

    !> If true, minimize the size of the Ibuffer window by default.  Port
    !> of ibuffer.el ibuffer-default-shrink-to-minimum-size.
    logical, public :: ibuffer_default_shrink_to_minimum_size = .false.

    !> If true, summarize Ibuffer columns.  Port of ibuffer.el
    !> ibuffer-display-summary.
    logical, public :: ibuffer_display_summary = .true.

    !> If true, do not display continuation lines.  (`truncate_lines' is
    !> not ported.)  Port of ibuffer.el ibuffer-truncate-lines.
    logical, public :: ibuffer_truncate_lines = .true.

    !> If true, ignore case when searching.  Port of ibuffer.el
    !> ibuffer-case-fold-search.
    logical, public :: ibuffer_case_fold_search = .true.

    !> The criteria by which to sort the buffers: "recency" (last view
    !> time), "alphabetic" (lexicographic), "size" (buffer size),
    !> "filename/process" (file name) or "major_mode" (major mode).
    !>
    !> Note that this variable is local to each Ibuffer buffer.  Thus, you
    !> can have multiple Ibuffer buffers open, each with a different sorted
    !> view of the buffers.  Port of ibuffer.el ibuffer-default-sorting-mode.
    character(len=:), allocatable, public :: ibuffer_default_sorting_mode

    !> If true, reverse the default sorting order.  Port of ibuffer.el
    !> ibuffer-default-sorting-reversep.
    logical, public :: ibuffer_default_sorting_reversep = .false.

    !> The string to use for eliding long columns.  Port of ibuffer.el
    !> ibuffer-eliding-string.
    character(len=:), allocatable, public :: ibuffer_eliding_string

    !> An array of predicates for buffers to display conditionally.
    !>
    !> A predicate can be a regexp or a function.
    !> If a regexp, then it will be matched against the buffer's name.
    !> If a function, it will be called with the buffer as an argument, and
    !> should return true if this buffer should be shown.
    !>
    !> Viewing of buffers hidden because of these predicates may be customized
    !> via `ibuffer_default_display_maybe_show_predicates' and is toggled by
    !> giving a prefix argument to `ibuffer_update'.
    !> Note that this specialized filtering occurs before real filtering.
    !> Port of ibuffer.el ibuffer-maybe-show-predicates.
    type(ibuffer_show_predicate_type), allocatable, public :: ibuffer_maybe_show_predicates(:)

    !> True means show buffers that match `ibuffer_maybe_show_predicates'.
    !> Port of ibuffer.el ibuffer-default-display-maybe-show-predicates.
    logical, public :: ibuffer_default_display_maybe_show_predicates = .false.

    !> If true, then forward and backwards movement commands cycle.  Port
    !> of ibuffer.el ibuffer-movement-cycle.
    logical, public :: ibuffer_movement_cycle = .true.

    !> The character to display for modified buffers.  Port of ibuffer.el
    !> ibuffer-modified-char.
    character, public :: ibuffer_modified_char = "*"

    !> The character to display for read-only buffers.  Port of ibuffer.el
    !> ibuffer-read-only-char.
    character, public :: ibuffer_read_only_char = "%"

    !> The character to display for marked buffers.  Port of ibuffer.el
    !> ibuffer-marked-char.
    character, public :: ibuffer_marked_char = ">"

    !> The character to display for locked buffers.  Port of ibuffer.el
    !> ibuffer-locked-char.
    character, public :: ibuffer_locked_char = "L"

    !> The character to display for buffers marked for deletion.  Port of
    !> ibuffer.el ibuffer-deletion-char.
    character, public :: ibuffer_deletion_char = "D"

    !> If true, don't ask for confirmation of "dangerous" operations.  Port
    !> of ibuffer.el ibuffer-expert.
    logical, public :: ibuffer_expert = .false.

    !> If true, display the current Ibuffer buffer itself.
    !> Note that this has a drawback - the data about the current Ibuffer
    !> buffer will most likely be inaccurate.  This includes modification
    !> state, size, etc.  Port of ibuffer.el ibuffer-view-ibuffer.
    logical, public :: ibuffer_view_ibuffer = .false.

    !> If not IBUFFER_SHOW_LAST_NEVER, always display the previous buffer:
    !> IBUFFER_SHOW_LAST_ALWAYS always, IBUFFER_SHOW_LAST_NOMINI always
    !> except a minibuffer.  This variable takes precedence over
    !> filtering, and even `ibuffer_never_show_predicates'.  Port of
    !> ibuffer.el ibuffer-always-show-last-buffer.
    integer, public :: ibuffer_always_show_last_buffer = IBUFFER_SHOW_LAST_NEVER

    !> If true, only offer buffers visible in the Ibuffer buffer
    !> in completion lists of the `ibuffer_jump_to_buffer' command.  Port of
    !> ibuffer.el ibuffer-jump-offer-only-visible-buffers.
    logical, public :: ibuffer_jump_offer_only_visible_buffers = .false.

    !> If not IBUFFER_HEADER_LINE_NIL, display a header line.
    !> If the variable's value is IBUFFER_HEADER_LINE_T, the header line
    !> displays the current filters.  For IBUFFER_HEADER_LINE_TITLE,
    !> display the column titles.  Port of ibuffer.el
    !> ibuffer-use-header-line.
    integer, public :: ibuffer_use_header_line = IBUFFER_HEADER_LINE_T

    !> The default directory to use for a new Ibuffer buffer.
    !> If unallocated, inherit the directory of the buffer in which
    !> `ibuffer' was called.  Otherwise, this variable should be a string
    !> naming a directory, like `default_directory'.  Port of ibuffer.el
    !> ibuffer-default-directory.
    character(len=:), allocatable, public :: ibuffer_default_directory

    !> List of "Help" major modes.  Port of ibuffer.el
    !> ibuffer-help-buffer-modes.
    character(len=IBUFFER_NAME_LEN), allocatable, public :: ibuffer_help_buffer_modes(:)

    !> Regexp to match compressed file names.  Port of ibuffer.el
    !> ibuffer-compressed-file-name-regexp.
    character(len=:), allocatable, public :: ibuffer_compressed_file_name_regexp

    !> Hook run when `ibuffer' is called.  Port of ibuffer.el ibuffer-hook.
    type(hook_type), target, public :: ibuffer_hook = hook_type(name="ibuffer_hook")

    !> Hook run upon entry into `ibuffer_mode'.  Port of ibuffer.el
    !> ibuffer-mode-hook.
    type(hook_type), target, public :: ibuffer_mode_hook = hook_type(name="ibuffer_mode_hook")

    !> Face used by default in `ibuffer_marked_face'.  Port of ibuffer.el
    !> ibuffer-marked (the face `ibuffer_marked').
    type(named_face_type), target, public :: ibuffer_marked_face_def

    !> Face used by default in `ibuffer_deletion_face'.  Port of ibuffer.el
    !> ibuffer-deletion (the face `ibuffer_deletion').
    type(named_face_type), target, public :: ibuffer_deletion_face_def

    !> Face used by default in `ibuffer_title_face'.  Port of ibuffer.el
    !> ibuffer-title (the face `ibuffer_title').
    type(named_face_type), target, public :: ibuffer_title_face_def

    !> Face used by default in `ibuffer_filter_group_name_face'.  Port of
    !> ibuffer.el ibuffer-filter-group-name (the face
    !> `ibuffer_filter_group_name').
    type(named_face_type), target, public :: ibuffer_filter_group_name_face_def

    !> Face used for locked buffers in Ibuffer.  Port of ibuffer.el
    !> ibuffer-locked-buffer.
    type(named_face_type), target, public :: ibuffer_locked_buffer

    !> Face used for displaying marked buffers.  Port of ibuffer.el
    !> ibuffer-marked-face.
    character(len=:), allocatable, public :: ibuffer_marked_face

    !> Face used for displaying buffers marked for deletion.  Port of
    !> ibuffer.el ibuffer-deletion-face.
    character(len=:), allocatable, public :: ibuffer_deletion_face

    !> Face used for the title string.  Port of ibuffer.el
    !> ibuffer-title-face.
    character(len=:), allocatable, public :: ibuffer_title_face

    !> Face used for displaying filtering group names.  Port of ibuffer.el
    !> ibuffer-filter-group-name-face.
    character(len=:), allocatable, public :: ibuffer_filter_group_name_face

    !> An array of file name abbreviations like `directory_abbrev_table'.
    !> Port of ibuffer.el ibuffer-directory-abbrev-alist.
    type(ibuffer_directory_abbrev_type), allocatable, public :: ibuffer_directory_abbrev_table(:)

    !> A list of predicates (a regexp or function) for buffers not to
    !> display.  If a regexp, then it will be matched against the buffer's
    !> name.  If a function, it will be called with the buffer as an
    !> argument, and should return true if this buffer should not be
    !> shown.  Port of ibuf-ext.el ibuffer-never-show-predicates.
    type(ibuffer_show_predicate_type), allocatable, public :: ibuffer_never_show_predicates(:)

    !> A list of predicates (a regexp or function) for buffers to always
    !> display.  If a regexp, then it will be matched against the buffer's
    !> name.  If a function, it will be called with the buffer as an
    !> argument, and should return true if this buffer should be shown.
    !> Note that buffers matching one of these predicates will be shown
    !> regardless of any active filters in this buffer.  Port of
    !> ibuf-ext.el ibuffer-always-show-predicates.
    type(ibuffer_show_predicate_type), allocatable, public :: ibuffer_always_show_predicates(:)

    !> A list of regexps for buffers ignored by
    !> `ibuffer_mark_by_content_regexp'.  Buffers whose name matches a
    !> regexp in this list, are not searched.  Port of ibuf-ext.el
    !> ibuffer-never-search-content-name.
    character(len=256), allocatable, public :: ibuffer_never_search_content_name(:)

    !> A list of major modes ignored by `ibuffer_mark_by_content_regexp'.
    !> Buffers whose major mode is in this list, are not searched.  Port
    !> of ibuf-ext.el ibuffer-never-search-content-mode.
    character(len=IBUFFER_NAME_LEN), allocatable, public :: ibuffer_never_search_content_mode(:)

    !> The number of hours before a buffer is considered "old".  Port of
    !> ibuf-ext.el ibuffer-old-time.
    integer, public :: ibuffer_old_time = 72

    !> If true, then show the names of filter groups which are empty.
    !> Port of ibuf-ext.el ibuffer-show-empty-filter-groups.
    logical, public :: ibuffer_show_empty_filter_groups = .true.

    !> The saved filters: each a name and filter specifications.
    !>
    !> The filters defined here are joined with an implicit logical `and'
    !> and associated with NAME.  The combined specification can be used
    !> by name in other filter specifications via `ibuffer_saved'.  They
    !> can also be switched to by name (see the functions
    !> `ibuffer_switch_to_saved_filters' and `ibuffer_save_filters').  The
    !> variable `ibuffer_save_with_custom' affects how this information is
    !> saved for future sessions.  Add one with
    !> `ibuffer_saved_filters%add(NAME, F1, F2, ...)'.  Port of ibuf-ext.el
    !> ibuffer-saved-filters.
    type(ibuffer_saved_filters_type), public :: ibuffer_saved_filters

    !> The saved sets of filter groups to switch between: each a unique
    !> but arbitrary name and filter groups with the same structure as
    !> `ibuffer_filter_groups'.
    !>
    !> See also the functions `ibuffer_save_filter_groups' and
    !> `ibuffer_switch_to_saved_filter_groups' for saving and switching
    !> between sets of filter groups, and the variable
    !> `ibuffer_save_with_custom' that affects how this information is
    !> saved.  Port of ibuf-ext.el ibuffer-saved-filter-groups.
    type(ibuffer_saved_filter_groups_type), public :: ibuffer_saved_filter_groups

    !> The filters: each a name, a description, and the test of a buffer.
    !>
    !> You most likely do not want to modify this variable directly; see
    !> `define_ibuffer_filter'.  Port of ibuf-ext.el ibuffer-filtering-alist.
    type(ibuffer_filter_def_type), allocatable, public :: ibuffer_filtering_table(:)

    !> Special formats used when a filter is active.
    !>
    !> For example, suppose that when you add a filter for buffers whose
    !> major mode is `text_mode', you only want to see the mark and the
    !> name of the buffer.  You could accomplish that by adding
    !> `call fmts%add([ibuffer_column("mark"), ibuffer_text(" "),
    !> ibuffer_column("name")])' and `call
    !> ibuffer_filter_format_table%add("mode", fmts)'.  Port of ibuf-ext.el
    !> ibuffer-filter-format-alist.
    type(ibuffer_filter_formats_type), public :: ibuffer_filter_format_table

    !> If true, then use Custom to save interactively changed variables.
    !> Currently, this only applies to `ibuffer_saved_filters' and
    !> `ibuffer_saved_filter_groups'.  Port of ibuf-ext.el
    !> ibuffer-save-with-custom.
    logical, public :: ibuffer_save_with_custom = .true.

    !> Keymap for `ibuffer_mode'.  Port of ibuffer.el ibuffer-mode-map.
    type(handle_type), public :: ibuffer_mode_map

    !> Keymap of the filter group names.  Port of ibuffer.el
    !> ibuffer-mode-filter-group-map.
    type(handle_type), public :: ibuffer_mode_filter_group_map

    !> The columns for use with `ibuffer_formats'.  Port of ibuf-macs.el
    !> define-ibuffer-column (the columns defined).
    type(ibuffer_column_type), allocatable, public :: ibuffer_columns(:)

    !> The ways to sort buffers.  Port of ibuffer.el
    !> ibuffer-sorting-functions-alist.
    type(ibuffer_sorter_type), allocatable, public :: ibuffer_sorting_functions(:)

    !> A buffer and its mark (an element of `ibuffer_current_state_list').
    type, public :: ibuffer_state_type
        type(handle_type) :: buffer
        character :: mark = " "
        integer(int64) :: point = 0
    end type ibuffer_state_type

    abstract interface
        ! A function `ibuffer_map_lines' calls for each buffer.  Not a port.
        recursive integer function map_proc(buf, mark)
            import :: handle_type
            implicit none(type, external)
            type(handle_type), intent(in) :: buf
            character, intent(in) :: mark
        end function map_proc
    end interface

    ! The buffer-local variables of an Ibuffer buffer.
    type :: local_type
        integer(int64) :: serial = 0
        ! ibuffer-sorting-mode ("" for nil), ibuffer-last-sorting-mode,
        ! ibuffer-sorting-reversep.
        character(len=:), allocatable :: sorting_mode, last_sorting_mode
        logical :: sorting_reversep = .false.
        ! ibuffer-shrink-to-minimum-size.
        integer :: shrink = IBUFFER_SHRINK_NIL
        ! ibuffer-display-maybe-show-predicates.
        logical :: display_maybe_show_predicates = .false.
        ! ibuffer-current-format (0-based; -1 for nil).
        integer :: current_format = -1
        ! ibuffer-did-modification.
        logical :: did_modification = .false.
        ! ibuffer-tmp-hide-regexps, ibuffer-tmp-show-regexps.
        character(len=256), allocatable :: tmp_hide(:), tmp_show(:)
        ! ibuffer-hidden-filter-groups.
        character(len=256), allocatable :: hidden_groups(:)
        ! ibuffer-filtering-qualifiers (the top of the stack first),
        ! ibuffer-filter-groups, ibuffer-filter-group-kill-ring (the last
        ! killed first).
        type(ibuffer_filter_type), allocatable :: qualifiers(:)
        type(ibuffer_filter_groups_type) :: filter_groups
        type(ibuffer_saved_filter_type), allocatable :: group_kill_ring(:)
        ! A buffer-local `ibuffer_formats' (the FORMATS of `ibuffer').
        logical :: has_formats = .false.
        type(ibuffer_formats_type) :: formats
    end type local_type

    type(local_type), allocatable :: locals(:)

    ! The texts of the columns with a summarizer since the list was last
    ! drawn (upstream the `ibuffer-column-summary' property of the
    ! column's function).
    type :: summary_type
        type(ibuffer_cell_type), allocatable :: cells(:)
        integer :: n = 0
    end type summary_type

    type(summary_type), allocatable :: summaries(:)

    ! The operation `ibuffer_operate' runs, and its arguments.
    type(ibuffer_op_type) :: current_op

    ! State of the mapping functions (upstream's dynamically bound
    ! variables and closures).
    character :: map_mark = " ", map_new = " "
    character(len=256), allocatable :: map_names(:)
    integer :: map_nnames = 0
    type(ibuffer_state_type), allocatable :: map_states(:)
    integer :: map_nstates = 0
    logical :: map_with_pos = .false.

    ! ibuffer-save-marks: the marks saved.
    type(ibuffer_state_type), allocatable :: saved_marks(:)

    ! The ARG of `ibuffer_do_toggle_read_only' (huge for none).
    integer(int64) :: read_only_arg = huge(0_int64)

    ! The function and mark of `map_on_mark'.
    procedure(map_proc), pointer :: on_mark_fn => null()
    character :: on_mark_mark = " "

    interface
        !> Whether BUF should be shown (ibuffer-ext-visible-p).  (In
        !> ibuf_ext_fm.)  Port of ibuf-ext.el:ibuffer-ext-visible-p.
        recursive module logical function is_ibuffer_ext_visible(buf, all, ibuffer_buf, status) result(visible)
            implicit none(type, external)
            type(handle_type), intent(in) :: buf, ibuffer_buf
            logical, intent(in) :: all
            type(status_type), intent(out) :: status
        end function is_ibuffer_ext_visible

        !> Divide BMARKLIST into the filter groups: their NAMES (in reverse
        !> order, as upstream returns them) and the buffers of each,
        !> GROUP_OF(k) being the index in NAMES of the group of entry k
        !> (0 for none).  NOEMPTY leaves out the empty groups, NODEFAULT
        !> the group "Default".  Port of
        !> ibuf-ext.el:ibuffer-generate-filter-groups.  (In ibuf_ext_fm.)
        recursive module subroutine ibuffer_generate_filter_groups(bmarklist, names, group_of, status, noempty, &
                                                                   nodefault)
            implicit none(type, external)
            type(ibuffer_state_type), intent(in) :: bmarklist(:)
            character(len=256), allocatable, intent(out) :: names(:)
            integer, allocatable, intent(out) :: group_of(:)
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: noempty, nodefault
        end subroutine ibuffer_generate_filter_groups

        !> Return true if BUF passes all FILTERS.
        !>
        !> BUF is a buffer, and FILTERS is a list of filter specifications
        !> with the same structure as `ibuffer_filtering_qualifiers'.
        !> Port of ibuf-ext.el:ibuffer-included-in-filters-p.  (In
        !> ibuf_ext_fm.)
        recursive module logical function is_ibuffer_included_in_filters(buf, filters, status) result(included)
            implicit none(type, external)
            type(handle_type), intent(in) :: buf
            type(ibuffer_filter_type), intent(in) :: filters(:)
            type(status_type), intent(out) :: status
        end function is_ibuffer_included_in_filters

        !> Return true if BUF pass FILTER.
        !>
        !> BUF is a buffer, and FILTER is a filter specification, with the
        !> same structure as an element of the list
        !> `ibuffer_filtering_qualifiers'.  Port of
        !> ibuf-ext.el:ibuffer-included-in-filter-p.  (In ibuf_ext_fm.)
        recursive module logical function is_ibuffer_included_in_filter(buf, filter, status) result(included)
            implicit none(type, external)
            type(handle_type), intent(in) :: buf
            type(ibuffer_filter_type), intent(in) :: filter
            type(status_type), intent(out) :: status
        end function is_ibuffer_included_in_filter

        !> Extract operand from a unary compound FILTER specification
        !> (`ibuffer_not').  Port of ibuf-ext.el:ibuffer-unary-operand.  (In
        !> ibuf_ext_fm.)
        module function ibuffer_unary_operand(filter) result(operand)
            implicit none(type, external)
            type(ibuffer_filter_type), intent(in) :: filter
            type(ibuffer_filter_type) :: operand
        end function ibuffer_unary_operand

        !> The text of the filter specification QUALIFIER, as shown in the
        !> mode line: " [DESCRIPTION: QUALIFIER]".  Port of
        !> ibuf-ext.el:ibuffer-format-qualifier.  (In ibuf_ext_fm.)
        recursive module function ibuffer_format_qualifier(qualifier) result(s)
            implicit none(type, external)
            type(ibuffer_filter_type), intent(in) :: qualifier
            character(len=:), allocatable :: s
        end function ibuffer_format_qualifier

        !> Add FILTER_SPECIFICATION to `ibuffer_filtering_qualifiers'.
        !> If FILTER_SPECIFICATION is already in the list then return false.
        !> Otherwise, return true.  Port of ibuf-ext.el:ibuffer-push-filter.
        !> (In ibuf_ext_fm.)
        module logical function ibuffer_push_filter(filter_specification) result(pushed)
            implicit none(type, external)
            type(ibuffer_filter_type), intent(in) :: filter_specification
        end function ibuffer_push_filter

        !> Disable all filters currently in effect in this buffer.
        !> With optional arg DELETE_FILTER_GROUPS true, delete all filter
        !> group definitions by setting `ibuffer_filter_groups' to empty.
        !> Port of ibuf-ext.el:ibuffer-filter-disable.  (In ibuf_ext_fm.)
        recursive module subroutine ibuffer_filter_disable(status, delete_filter_groups)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: delete_filter_groups
        end subroutine ibuffer_filter_disable

        !> Remove the top filter in this buffer.  Port of
        !> ibuf-ext.el:ibuffer-pop-filter.  (In ibuf_ext_fm.)
        recursive module subroutine ibuffer_pop_filter(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine ibuffer_pop_filter

        !> Separate this buffer's top compound filter (AND, OR, NOT, or
        !> SAVED).
        !>
        !> This means that the topmost filter on the filtering stack, which
        !> must be a complex filter like (OR [name: foo] [mode: bar_mode]),
        !> will be turned into separate filters, like [name: foo] and [mode:
        !> bar_mode].  Port of ibuf-ext.el:ibuffer-decompose-filter.  (In
        !> ibuf_ext_fm.)
        recursive module subroutine ibuffer_decompose_filter(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine ibuffer_decompose_filter

        !> Exchange the top two filters on the stack in this buffer.  Port of
        !> ibuf-ext.el:ibuffer-exchange-filters.  (In ibuf_ext_fm.)
        recursive module subroutine ibuffer_exchange_filters(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine ibuffer_exchange_filters

        !> Negate the sense of the top filter in the current buffer.  Port of
        !> ibuf-ext.el:ibuffer-negate-filter.  (In ibuf_ext_fm.)
        recursive module subroutine ibuffer_negate_filter(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine ibuffer_negate_filter

        !> Replace the top two filters in this buffer with their logical
        !> OR.  If optional argument DECOMPOSE is true, instead break the
        !> top OR filter into parts.  Port of ibuf-ext.el:ibuffer-or-filter.
        !> (In ibuf_ext_fm.)
        recursive module subroutine ibuffer_or_filter(status, decompose)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: decompose
        end subroutine ibuffer_or_filter

        !> Replace the top two filters in this buffer with their logical
        !> AND.  If optional argument DECOMPOSE is true, instead break the
        !> top AND filter into parts.  Port of
        !> ibuf-ext.el:ibuffer-and-filter.  (In ibuf_ext_fm.)
        recursive module subroutine ibuffer_and_filter(status, decompose)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: decompose
        end subroutine ibuffer_and_filter

        !> Save FILTERS in this buffer with name NAME in
        !> `ibuffer_saved_filters'.  Interactively, prompt for NAME, and use
        !> the current filters.  Port of ibuf-ext.el:ibuffer-save-filters.
        !> (In ibuf_ext_fm.)
        recursive module subroutine ibuffer_save_filters(name, filters, status)
            implicit none(type, external)
            character(len=*), intent(in) :: name
            type(ibuffer_filter_type), intent(in) :: filters(:)
            type(status_type), intent(out) :: status
        end subroutine ibuffer_save_filters

        !> Delete saved filters with NAME from `ibuffer_saved_filters'.
        !> Port of ibuf-ext.el:ibuffer-delete-saved-filters.  (In
        !> ibuf_ext_fm.)
        recursive module subroutine ibuffer_delete_saved_filters(name, status)
            implicit none(type, external)
            character(len=*), intent(in) :: name
            type(status_type), intent(out) :: status
        end subroutine ibuffer_delete_saved_filters

        !> Add saved filters from `ibuffer_saved_filters' to this buffer's
        !> filters.  Port of ibuf-ext.el:ibuffer-add-saved-filters.  (In
        !> ibuf_ext_fm.)
        recursive module subroutine ibuffer_add_saved_filters(name, status)
            implicit none(type, external)
            character(len=*), intent(in) :: name
            type(status_type), intent(out) :: status
        end subroutine ibuffer_add_saved_filters

        !> Set this buffer's filters to filters with NAME from
        !> `ibuffer_saved_filters'.  Port of
        !> ibuf-ext.el:ibuffer-switch-to-saved-filters.  (In ibuf_ext_fm.)
        recursive module subroutine ibuffer_switch_to_saved_filters(name, status)
            implicit none(type, external)
            character(len=*), intent(in) :: name
            type(status_type), intent(out) :: status
        end subroutine ibuffer_switch_to_saved_filters

        !> Make the current filters into a filtering group.  Port of
        !> ibuf-ext.el:ibuffer-filters-to-filter-group.  (In ibuf_ext_fm.)
        recursive module subroutine ibuffer_filters_to_filter_group(name, status)
            implicit none(type, external)
            character(len=*), intent(in) :: name
            type(status_type), intent(out) :: status
        end subroutine ibuffer_filters_to_filter_group

        !> Set the current filter groups to filter by mode.  Port of
        !> ibuf-ext.el:ibuffer-set-filter-groups-by-mode.  (In ibuf_ext_fm.)
        recursive module subroutine ibuffer_set_filter_groups_by_mode(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine ibuffer_set_filter_groups_by_mode

        !> Remove the first filter group.  Port of
        !> ibuf-ext.el:ibuffer-pop-filter-group.  (In ibuf_ext_fm.)
        recursive module subroutine ibuffer_pop_filter_group(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine ibuffer_pop_filter_group

        !> Decompose the filter group GROUP into active filters.  Port of
        !> ibuf-ext.el:ibuffer-decompose-filter-group.  (In ibuf_ext_fm.)
        recursive module subroutine ibuffer_decompose_filter_group(group, status)
            implicit none(type, external)
            character(len=*), intent(in) :: group
            type(status_type), intent(out) :: status
        end subroutine ibuffer_decompose_filter_group

        !> Remove all filter groups.  Port of
        !> ibuf-ext.el:ibuffer-clear-filter-groups.  (In ibuf_ext_fm.)
        recursive module subroutine ibuffer_clear_filter_groups(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine ibuffer_clear_filter_groups

        !> Kill the filter group named NAME.
        !> The group will be added to `ibuffer_filter_group_kill_ring'.
        !> Port of ibuf-ext.el:ibuffer-kill-filter-group.  (In ibuf_ext_fm.)
        recursive module subroutine ibuffer_kill_filter_group(name, status)
            implicit none(type, external)
            character(len=*), intent(in) :: name
            type(status_type), intent(out) :: status
        end subroutine ibuffer_kill_filter_group

        !> Yank the last killed filter group before group named NAME.
        !> Port of ibuf-ext.el:ibuffer-yank-filter-group.  (In ibuf_ext_fm.)
        recursive module subroutine ibuffer_yank_filter_group(name, status)
            implicit none(type, external)
            character(len=*), intent(in) :: name
            type(status_type), intent(out) :: status
        end subroutine ibuffer_yank_filter_group

        !> Save all active filter groups GROUPS as NAME.
        !> They are added to `ibuffer_saved_filter_groups'.  Interactively,
        !> prompt for NAME, and use the current filters.  Port of
        !> ibuf-ext.el:ibuffer-save-filter-groups.  (In ibuf_ext_fm.)
        recursive module subroutine ibuffer_save_filter_groups(name, groups, status)
            implicit none(type, external)
            character(len=*), intent(in) :: name
            type(ibuffer_filter_groups_type), intent(in) :: groups
            type(status_type), intent(out) :: status
        end subroutine ibuffer_save_filter_groups

        !> Delete saved filter groups with NAME.
        !> They are removed from `ibuffer_saved_filter_groups'.  Port of
        !> ibuf-ext.el:ibuffer-delete-saved-filter-groups.  (In ibuf_ext_fm.)
        recursive module subroutine ibuffer_delete_saved_filter_groups(name, status)
            implicit none(type, external)
            character(len=*), intent(in) :: name
            type(status_type), intent(out) :: status
        end subroutine ibuffer_delete_saved_filter_groups

        !> Set this buffer's filter groups to saved version with NAME.
        !> The value from `ibuffer_saved_filter_groups' is used.  Port of
        !> ibuf-ext.el:ibuffer-switch-to-saved-filter-groups.  (In
        !> ibuf_ext_fm.)
        recursive module subroutine ibuffer_switch_to_saved_filter_groups(name, status)
            implicit none(type, external)
            character(len=*), intent(in) :: name
            type(status_type), intent(out) :: status
        end subroutine ibuffer_switch_to_saved_filter_groups

        !> Limit current view to buffers with major mode(s) specified by
        !> QUALIFIER.  QUALIFIER is the mode name or a list of names.
        !> Called interactively, accept a comma separated list of mode
        !> names.  Port of ibuf-ext.el:ibuffer-filter-by-mode.  (In
        !> ibuf_ext_fm.)
        recursive module subroutine ibuffer_filter_by_mode(qualifier, status)
            implicit none(type, external)
            character(len=*), intent(in) :: qualifier(:)
            type(status_type), intent(out) :: status
        end subroutine ibuffer_filter_by_mode

        !> Limit current view to buffers with major mode(s) specified by
        !> QUALIFIER.  QUALIFIER is the mode name or a list of names.
        !>
        !> Called interactively, accept a comma separated list of mode names
        !> currently used by buffers.  Port of
        !> ibuf-ext.el:ibuffer-filter-by-used-mode.  (In ibuf_ext_fm.)
        recursive module subroutine ibuffer_filter_by_used_mode(qualifier, status)
            implicit none(type, external)
            character(len=*), intent(in) :: qualifier(:)
            type(status_type), intent(out) :: status
        end subroutine ibuffer_filter_by_used_mode

        !> Limit current view to buffers with major mode(s) specified by
        !> QUALIFIER.  QUALIFIER is the mode name or a list of names.
        !>  Restrict the view to buffers whose major mode derivates
        !>  from modes specified by QUALIFIER.
        !> Called interactively, accept a comma separated list of mode
        !> names.  Port of ibuf-ext.el:ibuffer-filter-by-derived-mode.  (In
        !> ibuf_ext_fm.)
        recursive module subroutine ibuffer_filter_by_derived_mode(qualifier, status)
            implicit none(type, external)
            character(len=*), intent(in) :: qualifier(:)
            type(status_type), intent(out) :: status
        end subroutine ibuffer_filter_by_derived_mode

        !> Apply the simple filter NAME with the string QUALIFIER, the
        !> integer NUMBER or no qualifier: `ibuffer_filter_by_name',
        !> `ibuffer_filter_by_process', `ibuffer_filter_by_starred_name',
        !> `ibuffer_filter_by_filename', `ibuffer_filter_by_basename',
        !> `ibuffer_filter_by_file_extension', `ibuffer_filter_by_directory',
        !> `ibuffer_filter_by_size_gt', `ibuffer_filter_by_size_lt',
        !> `ibuffer_filter_by_modified', `ibuffer_filter_by_visiting_file' and
        !> `ibuffer_filter_by_content' (the commands
        !> ibuf-macs.el:define-ibuffer-filter defines).  (In ibuf_ext_fm.)
        !> Port of ibuf-macs.el:define-ibuffer-filter, in part (the commands
        !> it defines).
        recursive module subroutine ibuffer_filter_by(name, status, qualifier, number)
            implicit none(type, external)
            character(len=*), intent(in) :: name
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: qualifier
            integer(int64), intent(in), optional :: number
        end subroutine ibuffer_filter_by

        !> Limit current view to buffers for which QUALIFIER returns true.
        !> Port of ibuf-ext.el:ibuffer-filter-by-predicate.  (In
        !> ibuf_ext_fm.)
        recursive module subroutine ibuffer_filter_by_predicate(qualifier, status)
            implicit none(type, external)
            procedure(ibuffer_buffer_predicate_proc) :: qualifier
            type(status_type), intent(out) :: status
        end subroutine ibuffer_filter_by_predicate

        !> Create a completion table of buffer modes currently in use.
        !> If INCLUDE_PARENTS is true then include parent modes.  Port of
        !> ibuf-ext.el:ibuffer-list-buffer-modes.  (In ibuf_ext_fm.)
        module subroutine ibuffer_list_buffer_modes(modes, include_parents)
            implicit none(type, external)
            character(len=IBUFFER_NAME_LEN), allocatable, intent(out) :: modes(:)
            logical, intent(in), optional :: include_parents
        end subroutine ibuffer_list_buffer_modes

        !> Define ibuf-ext.el's commands, sorting methods and keys.  (In
        !> ibuf_ext_fm.)  Not a port.
        module subroutine syms_of_ibuf_ext()
            implicit none(type, external)
        end subroutine syms_of_ibuf_ext
    end interface

contains

    ! ------------------------------------------------------------------
    ! Buffer-local variables
    ! ------------------------------------------------------------------

    !> The index of the current buffer's entry in `locals', made if MAKE
    !> (0 if there is none).  Not a port.
    integer function ibuffer_local_index(make) result(k)
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
                call init_local(k)
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
        call init_local(k)
    end function ibuffer_local_index

    ! Not a port.
    subroutine init_local(k)
        integer, intent(in) :: k

        locals(k)%sorting_mode = ""
        locals(k)%last_sorting_mode = ""
        allocate (locals(k)%tmp_hide(0), locals(k)%tmp_show(0), locals(k)%hidden_groups(0))
        allocate (locals(k)%qualifiers(0), locals(k)%filter_groups%groups(0), locals(k)%group_kill_ring(0))
    end subroutine init_local

    ! kill-all-local-variables: forget the Ibuffer state.  Not a port.
    subroutine kill_ibuffer_locals()
        integer :: k

        k = ibuffer_local_index(.false.)
        if (k == 0) return
        locals(k) = local_type(serial=-1)
    end subroutine kill_ibuffer_locals

    !> The filters currently acting on the buffer list of the current
    !> Ibuffer buffer (empty if no filters are in effect): the top of the
    !> stack first.  All of the specified filters in the list are applied
    !> successively to the buffer list.
    !>
    !> Each filter specification can be simple, such as
    !> `ibuffer_major_mode("text_mode")', or compound: `ibuffer_not(F)',
    !> `ibuffer_and(F1, F2, ...)', `ibuffer_or(F1, F2, ...)' and
    !> `ibuffer_saved(NAME)' (the filter saved under the string NAME in
    !> `ibuffer_saved_filters'; it is an error to name a filter that has
    !> not been saved).
    !>
    !> This variable is local to each ibuffer buffer.  Port of
    !> ibuf-ext.el ibuffer-filtering-qualifiers (read).
    function ibuffer_filtering_qualifiers() result(filters)
        type(ibuffer_filter_type), allocatable :: filters(:)
        integer :: k

        k = ibuffer_local_index(.true.)
        allocate (filters(size(locals(k)%qualifiers)))
        filters = locals(k)%qualifiers
    end function ibuffer_filtering_qualifiers

    !> Set `ibuffer_filtering_qualifiers' of the current Ibuffer buffer to
    !> FILTERS.  Port of ibuf-ext.el ibuffer-filtering-qualifiers (set).
    subroutine ibuffer_set_filtering_qualifiers(filters)
        type(ibuffer_filter_type), intent(in) :: filters(:)
        integer :: k

        k = ibuffer_local_index(.true.)
        deallocate (locals(k)%qualifiers)
        allocate (locals(k)%qualifiers(size(filters)))
        locals(k)%qualifiers = filters
    end subroutine ibuffer_set_filtering_qualifiers

    !> The current Ibuffer buffer's active filter groups (none if empty).
    !>
    !> Each group has a label and one or more filter specifications of
    !> the same form as allowed as elements of
    !> `ibuffer_filtering_qualifiers'.
    !>
    !> Each filter group is displayed as a separate section in the
    !> ibuffer list, headed by LABEL and displaying only the buffers
    !> that pass through all the filters associated with NAME in this
    !> list.  Port of ibuf-ext.el ibuffer-filter-groups (read).
    function ibuffer_filter_groups() result(groups)
        type(ibuffer_filter_groups_type) :: groups
        integer :: k

        k = ibuffer_local_index(.true.)
        groups = locals(k)%filter_groups
    end function ibuffer_filter_groups

    !> Set `ibuffer_filter_groups' of the current Ibuffer buffer to
    !> GROUPS.  Port of ibuf-ext.el ibuffer-filter-groups (set).
    subroutine ibuffer_set_filter_groups(groups)
        type(ibuffer_filter_groups_type), intent(in) :: groups
        integer :: k

        k = ibuffer_local_index(.true.)
        locals(k)%filter_groups = groups
        if (.not. allocated(locals(k)%filter_groups%groups)) allocate (locals(k)%filter_groups%groups(0))
    end subroutine ibuffer_set_filter_groups

    ! The current buffer's formats (the buffer-local `ibuffer_formats' if
    ! set).  Not a port.
    function formats_of() result(f)
        type(ibuffer_formats_type) :: f
        integer :: k, j, found, i

        k = ibuffer_local_index(.false.)

        if (k > 0) then
            ! ibuffer-active-formats-name: the formats of the last filter in
            ! `ibuffer_filter_format_table'.
            found = 0

            if (allocated(ibuffer_filter_format_table%entries)) then
                do j = 1, size(locals(k)%qualifiers)
                    i = ibuffer_filter_format_table%find(filter_symbol(locals(k)%qualifiers(j)))
                    if (i > 0) found = i
                end do
            end if

            if (found > 0) then
                f = ibuffer_filter_format_table%entries(found)%formats
                return
            end if

            if (locals(k)%has_formats) then
                f = locals(k)%formats
                return
            end if
        end if

        f = ibuffer_formats
    end function formats_of

    !> (car FILTER): the name of a simple filter, else "not", "and", "or"
    !> or "saved".  Not a port.
    function filter_symbol(filter) result(name)
        type(ibuffer_filter_type), intent(in) :: filter
        character(len=:), allocatable :: name

        name = ""
        if (.not. allocated(filter%nodes)) return
        if (size(filter%nodes) == 0) return

        select case (filter%nodes(1)%kind)
        case (IBUFFER_FILTER_NOT)
            name = "not"
        case (IBUFFER_FILTER_AND)
            name = "and"
        case (IBUFFER_FILTER_OR)
            name = "or"
        case (IBUFFER_FILTER_SAVED)
            name = "saved"
        case default
            name = trim(filter%nodes(1)%name)
        end select
    end function filter_symbol

    ! ------------------------------------------------------------------
    ! Text properties of the lines
    ! ------------------------------------------------------------------

    !> The text property NAME at POS: false outside the accessible text.
    !> Not a port.
    logical function has_prop(pos, name)
        integer(int64), intent(in) :: pos
        character(len=*), intent(in) :: name
        type(value_type) :: v
        type(status_type) :: st

        has_prop = .false.
        if (pos < point_min() .or. pos >= point_max()) return
        call get_text_property(pos, sym(name), v, st)
        has_prop = st%ok() .and. .not. nilp(v)
    end function has_prop

    !> The string text property NAME at POS ("" for none).  Not a port.
    function string_prop(pos, name) result(s)
        integer(int64), intent(in) :: pos
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: s
        type(value_type) :: v
        type(status_type) :: st

        s = ""
        if (pos < point_min() .or. pos >= point_max()) return
        call get_text_property(pos, sym(name), v, st)
        if (st%ok() .and. stringp(v)) s = xstring(v)
    end function string_prop

    !> Whether there is a string text property NAME at POS.  Not a port.
    logical function has_string_prop(pos, name)
        integer(int64), intent(in) :: pos
        character(len=*), intent(in) :: name
        type(value_type) :: v
        type(status_type) :: st

        has_string_prop = .false.
        if (pos < point_min() .or. pos >= point_max()) return
        call get_text_property(pos, sym(name), v, st)
        has_string_prop = st%ok() .and. stringp(v)
    end function has_string_prop

    !> The serial number of the buffer of the line at POS (0 for none).
    !> Not a port.
    integer(int64) function line_serial(pos) result(serial)
        integer(int64), intent(in) :: pos
        type(value_type) :: v
        type(status_type) :: st

        serial = 0
        if (pos < point_min() .or. pos >= point_max()) return
        call get_text_property(pos, sym("ibuffer_properties"), v, st)
        if (st%ok() .and. fixnump(v)) serial = xfixnum(v)
    end function line_serial

    !> The live buffer with serial number SERIAL (the null handle for
    !> none).  Not a port.
    function serial_buffer(serial) result(h)
        integer(int64), intent(in) :: serial
        type(handle_type) :: h
        integer(int64) :: k

        h = null_handle()
        if (serial == 0 .or. .not. allocated(buffers)) return

        do k = 1, size(buffers, kind=int64)
            if (buffers(k)%serial /= serial) cycle
            if (is_buffer_live(buffer_handle(k))) h = buffer_handle(k)
            return
        end do
    end function serial_buffer

    !> The mark of the buffer on the current line.  Port of
    !> ibuffer.el:ibuffer-current-mark.
    character function ibuffer_current_mark() result(mark)
        type(value_type) :: v
        type(status_type) :: st
        integer(int64) :: pos

        mark = " "
        pos = pos_bol()
        if (pos < point_min() .or. pos >= point_max()) return
        call get_text_property(pos, sym("ibuffer_mark"), v, st)
        if (st%ok() .and. fixnump(v)) mark = achar(xfixnum(v))
    end function ibuffer_current_mark

    !> Return the buffer on the current line (the null handle for none or
    !> a killed buffer).  If MUST_BE_LIVE, signal an error in STATUS if
    !> there is none or it has been killed.  Port of
    !> ibuffer.el:ibuffer-current-buffer.
    function ibuffer_current_buffer(status, must_be_live) result(buf)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: must_be_live
        type(handle_type) :: buf
        integer(int64) :: serial
        logical :: live

        status = status_type()
        serial = line_serial(pos_bol())
        buf = serial_buffer(serial)
        live = .false.
        if (present(must_be_live)) live = must_be_live
        if (.not. live) return

        if (serial == 0) then
            call set_error(status, FM_GENERIC_ERROR, "No buffer on this line")
        else if (buf%index == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Buffer #<killed buffer> has been killed; " &
                           //substitute_command_keys("use `\[ibuffer_update]' to update"))
        end if
    end function ibuffer_current_buffer

    ! (assert (derived-mode-p 'ibuffer-mode)).  Not a port.
    subroutine assert_ibuffer_mode(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (.not. is_derived_mode("ibuffer_mode")) call set_error(status, FM_GENERIC_ERROR, &
                                                                  "Assertion failed: (derived-mode-p 'ibuffer-mode)")
    end subroutine assert_ibuffer_mode

    ! ------------------------------------------------------------------
    ! Utility functions
    ! ------------------------------------------------------------------

    !> Insert NAMES into the current buffer in as many columns as possible.
    !> The maximum number of columns is determined by the current window
    !> width and the longest string in NAMES.  Port of
    !> ibuffer.el:ibuffer-columnize-and-insert-list.
    subroutine ibuffer_columnize_and_insert_list(names, n, status, pad_width)
        character(len=*), intent(in) :: names(:)
        integer, intent(in) :: n
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: pad_width
        integer :: pad, width, max, columns, k, j

        status = status_type()
        pad = 3
        if (present(pad_width)) pad = pad_width
        width = 80

        if (selected_frame%selected_window%index /= 0) then
            width = window_text_cols(windows(selected_frame%selected_window%index))
        end if

        max = 0

        do k = 1, n
            max = max0(max, len_trim(names(k)))
        end do

        max = max + pad
        columns = width/max
        if (columns == 0) columns = 1
        k = 1

        do while (k <= n)
            do j = 1, columns - 1
                if (k > n) exit
                call insert(trim(names(k))//repeat(" ", max - len_trim(names(k))), status)
                k = k + 1
            end do

            if (k <= n) then
                call insert(trim(names(k)), status)
                k = k + 1
            end if

            call insert(achar(10), status)
        end do
    end subroutine ibuffer_columnize_and_insert_list

    !> Like `find_file', but default to the directory of the buffer at
    !> point.  Port of ibuffer.el:ibuffer-find-file.
    recursive subroutine ibuffer_find_file(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buf, old
        type(status_type) :: st
        character(len=:), allocatable :: dir, file

        old = current_buffer()
        dir = buffers(cur)%directory
        buf = ibuffer_current_buffer(st)

        if (buf%index /= 0) then
            if (allocated(buffers(buf%index)%directory)) dir = buffers(buf%index)%directory
        end if

        call read_file_name("Find file: ", file, status, dir=dir)
        if (.not. status%ok()) return
        call find_file(file, status)
    end subroutine ibuffer_find_file

    ! Move in DIRECTION while point is on a line with one of PROPS.  Port of
    ! ibuffer.el:ibuffer-skip-properties.
    subroutine ibuffer_skip_properties(props, nprops, direction)
        character(len=*), intent(in) :: props(:)
        integer, intent(in) :: nprops
        integer(int64), intent(in) :: direction
        integer(int64) :: shortage
        logical :: hit
        integer :: k

        do while (.not. is_eob())
            hit = .false.

            do k = 1, nprops
                if (has_prop(point(), trim(props(k)))) hit = .true.
            end do

            if (.not. hit) exit
            shortage = forward_line(direction)
            call beginning_of_line()
        end do
    end subroutine ibuffer_skip_properties

    !> Move backwards ARG lines, wrapping around the list if necessary.
    !> Port of ibuffer.el:ibuffer-backward-line.
    recursive subroutine ibuffer_backward_line(arg, skip_group_names)
        integer(int64), intent(in), optional :: arg
        logical, intent(in), optional :: skip_group_names
        integer(int64) :: n, shortage
        logical :: skip
        character(len=32) :: props(2)

        n = 1
        if (present(arg)) n = arg
        skip = .false.
        if (present(skip_group_names)) skip = skip_group_names
        props(1) = "ibuffer_summary"
        props(2) = "ibuffer_filter_group_name"
        call beginning_of_line()

        do while (n > 0)
            shortage = forward_line(-1_int64)

            if (ibuffer_movement_cycle) then
                if (has_prop(point(), "ibuffer_title") .or. &
                    (skip .and. has_prop(point(), "ibuffer_filter_group_name"))) then
                    call goto_char(point_max())
                    call beginning_of_line()
                end if
            end if

            if (skip) then
                call ibuffer_skip_properties(props, 2, -1_int64)
            else
                call ibuffer_skip_properties(props, 1, -1_int64)
            end if

            ! Handle the special case of no buffers.
            if (has_prop(point(), "ibuffer_title")) then
                shortage = forward_line(1_int64)
                n = 1
            end if

            n = n - 1
        end do
    end subroutine ibuffer_backward_line

    recursive subroutine cmd_ibuffer_backward_line(status)
        type(status_type), intent(out) :: status

        status = status_type()

        if (nilp(current_prefix_arg)) then
            call ibuffer_backward_line()
        else
            call ibuffer_backward_line(prefix_numeric_value(current_prefix_arg))
        end if
    end subroutine cmd_ibuffer_backward_line

    !> Move forward ARG lines, wrapping around the list if necessary.
    !> Port of ibuffer.el:ibuffer-forward-line.
    recursive subroutine ibuffer_forward_line(arg, skip_group_names)
        integer(int64), intent(in), optional :: arg
        logical, intent(in), optional :: skip_group_names
        integer(int64) :: n, shortage
        logical :: skip
        character(len=32) :: props(2)

        n = 1
        if (present(arg)) n = arg
        skip = .false.
        if (present(skip_group_names)) skip = skip_group_names
        props(1) = "ibuffer_title"
        props(2) = "ibuffer_filter_group_name"
        call beginning_of_line()

        if (ibuffer_movement_cycle .and. (is_eob() .or. has_prop(point(), "ibuffer_summary"))) then
            call goto_char(point_min())
        end if

        if (has_prop(point(), "ibuffer_title") .or. (skip .and. has_prop(point(), "ibuffer_filter_group_name"))) then
            if (n > 0) n = n - 1

            if (skip) then
                call ibuffer_skip_properties(props, 2, 1_int64)
            else
                call ibuffer_skip_properties(props, 1, 1_int64)
            end if
        end if

        if (n < 0) then
            call ibuffer_backward_line(-n)
        else
            do while (n > 0)
                shortage = forward_line(1_int64)

                if (ibuffer_movement_cycle .and. (is_eob() .or. has_prop(point(), "ibuffer_summary"))) then
                    call goto_char(point_min())
                end if

                n = n - 1

                if (skip) then
                    call ibuffer_skip_properties(props, 2, 1_int64)
                else
                    call ibuffer_skip_properties(props, 1, 1_int64)
                end if
            end do
        end if
    end subroutine ibuffer_forward_line

    recursive subroutine cmd_ibuffer_forward_line(status)
        type(status_type), intent(out) :: status

        status = status_type()

        if (nilp(current_prefix_arg)) then
            call ibuffer_forward_line()
        else
            call ibuffer_forward_line(prefix_numeric_value(current_prefix_arg))
        end if
    end subroutine cmd_ibuffer_forward_line

    !> Visit the buffer on this line.
    !> If optional argument SINGLE is true, then also ensure there is only
    !> one window.  Port of ibuffer.el:ibuffer-visit-buffer.
    recursive subroutine ibuffer_visit_buffer(status, single)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: single
        type(handle_type) :: buf

        buf = ibuffer_current_buffer(status, .true.)
        if (.not. status%ok()) return
        call switch_to_buffer(buffers(buf%index)%name, status)
        if (.not. status%ok()) return

        if (present(single)) then
            if (single) call delete_other_windows(selected_window(), status)
        end if
    end subroutine ibuffer_visit_buffer

    recursive subroutine cmd_ibuffer_visit_buffer(status)
        type(status_type), intent(out) :: status

        call ibuffer_visit_buffer(status,.not. nilp(current_prefix_arg))
    end subroutine cmd_ibuffer_visit_buffer

    !> Visit the buffer on this line in another window.  Port of
    !> ibuffer.el:ibuffer-visit-buffer-other-window.
    recursive subroutine ibuffer_visit_buffer_other_window(status, noselect)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: noselect
        type(handle_type) :: buf, window
        logical :: nosel

        nosel = .false.
        if (present(noselect)) nosel = noselect
        buf = ibuffer_current_buffer(status, .true.)
        if (.not. status%ok()) return
        call bury_buffer(status, current_buffer())
        if (.not. status%ok()) return

        if (nosel) then
            call display_buffer(buf, window, status)
        else
            call switch_to_buffer_other_window(buf, status)
        end if
    end subroutine ibuffer_visit_buffer_other_window

    recursive subroutine cmd_ibuffer_visit_buffer_other_window(status)
        type(status_type), intent(out) :: status

        call ibuffer_visit_buffer_other_window(status)
    end subroutine cmd_ibuffer_visit_buffer_other_window

    !> Visit the buffer on this line in another window, but don't select
    !> it.  Port of ibuffer.el:ibuffer-visit-buffer-other-window-noselect.
    recursive subroutine ibuffer_visit_buffer_other_window_noselect(status)
        type(status_type), intent(out) :: status

        call ibuffer_visit_buffer_other_window(status, .true.)
    end subroutine ibuffer_visit_buffer_other_window_noselect

    !> Visit the buffer on this line, and delete other windows.  Port of
    !> ibuffer.el:ibuffer-visit-buffer-1-window.
    recursive subroutine ibuffer_visit_buffer_1_window(status)
        type(status_type), intent(out) :: status

        call ibuffer_visit_buffer(status, .true.)
    end subroutine ibuffer_visit_buffer_1_window

    !> Bury the buffer on this line.  Port of
    !> ibuffer.el:ibuffer-bury-buffer.
    recursive subroutine ibuffer_bury_buffer(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buf
        integer(int64) :: line, shortage

        buf = ibuffer_current_buffer(status, .true.)
        if (.not. status%ok()) return
        line = 1 + count_lines(1_int64, point())
        call bury_buffer(status, buf)
        if (.not. status%ok()) return
        call ibuffer_update(status, .false., .true.)
        if (.not. status%ok()) return
        call goto_char(point_min())
        shortage = forward_line(line - 1)
    end subroutine ibuffer_bury_buffer

    !> View marked buffers, or the buffer on the current line.
    !> Display each buffer as a new window in the current frame, splitting
    !> vertically.  Port of ibuffer.el:ibuffer-do-view.
    recursive subroutine ibuffer_do_view(status)
        type(status_type), intent(out) :: status

        call ibuffer_do_view_1(.false., status)
    end subroutine ibuffer_do_view

    !> As `ibuffer_do_view', but split windows horizontally.  Port of
    !> ibuffer.el:ibuffer-do-view-horizontally.
    recursive subroutine ibuffer_do_view_horizontally(status)
        type(status_type), intent(out) :: status

        call ibuffer_do_view_1(.true., status)
    end subroutine ibuffer_do_view_horizontally

    ! ibuffer-do-view-1 (TYPE vertically or horizontally).  Port of
    ! ibuffer.el:ibuffer-do-view-1.
    recursive subroutine ibuffer_do_view_1(horizontally, status)
        logical, intent(in) :: horizontally
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: marked(:)
        type(handle_type) :: new
        integer :: height, k

        call ibuffer_get_marked_buffers(marked, status)
        if (.not. status%ok()) return

        if (size(marked) == 0) then
            deallocate (marked)
            allocate (marked(1))
            marked(1) = ibuffer_current_buffer(status, .true.)
            if (.not. status%ok()) return
        end if

        call restore_buffer_modified(.false.)
        call delete_other_windows(selected_window(), status)
        if (.not. status%ok()) return
        call switch_to_buffer(buffers(marked(1)%index)%name, status)
        if (.not. status%ok()) return

        if (horizontally) then
            height = (frame_cols(selected_frame) - 1)/size(marked)
        else
            height = (frame_lines(selected_frame) - 1)/size(marked)
        end if

        do k = 2, size(marked)
            call split_window(selected_window(), height, .true., horizontally, new, status)
            if (.not. status%ok()) return
            call other_window(1_int64, status)
            if (.not. status%ok()) return
            call switch_to_buffer(buffers(marked(k)%index)%name, status)
            if (.not. status%ok()) return
        end do
    end subroutine ibuffer_do_view_1

    !> ibuffer-map-marked-lines: FUNC on the marked lines, then redisplay.
    !> Port of ibuffer.el:ibuffer-map-marked-lines.
    recursive integer function map_marked_lines(func, status) result(count)
        procedure(map_proc) :: func
        type(status_type), intent(out) :: status
        type(status_type) :: st

        count = map_on_mark(ibuffer_marked_char, func, status)
        call ibuffer_redisplay(st, .true.)
    end function map_marked_lines

    !> Shrink the selected window to fit the Ibuffer buffer; with OWIN,
    !> to at most its share of the frame.  Port of
    !> ibuffer.el:ibuffer-shrink-to-fit.
    subroutine ibuffer_shrink_to_fit(owin)
        logical, intent(in), optional :: owin
        integer :: nwin
        logical :: share
        type(handle_type), allocatable :: leaves(:)

        if (selected_frame%selected_window%index == 0) return

        associate (w => windows(selected_frame%selected_window%index))
            if (.not. is_buffer_live(w%buffer)) return
            if (.not. allocated(buffers(w%buffer%index)%major_mode)) return
            if (buffers(w%buffer%index)%major_mode /= "ibuffer_mode") return
        end associate

        share = .false.
        if (present(owin)) share = owin

        if (share) then
            call window_leaves(selected_frame%root_window, leaves)
            nwin = size(leaves)
            call fit_window_to_buffer(selected_window(), frame_lines(selected_frame)/max(1, nwin))
        else
            call fit_window_to_buffer(selected_window())
        end if
    end subroutine ibuffer_shrink_to_fit

    !> Display a buffer asking whether to perform OPERATION on NAMES.
    !> Port of ibuffer.el:ibuffer-confirm-operation-on.
    recursive subroutine ibuffer_confirm_operation_on(operation, names, n, answer, status)
        character(len=*), intent(in) :: operation
        character(len=*), intent(in) :: names(:)
        integer, intent(in) :: n
        logical, intent(out) :: answer
        type(status_type), intent(out) :: status
        type(handle_type) :: buf, old, new
        type(window_configuration_type) :: config
        type(status_type) :: st
        character(len=24) :: count

        answer = .true.
        status = status_type()
        if (ibuffer_expert) return

        if (n == 1) then
            call read_y_or_n("Really "//operation//" buffer "//trim(names(1))//"? ", answer, status)
            return
        end if

        old = current_buffer()
        call get_buffer_create("*Ibuffer confirmation*", buf, status)
        if (.not. status%ok()) return
        call set_buffer(buf, status)
        buffers(cur)%read_only = .false.
        call erase_buffer(st)
        call ibuffer_columnize_and_insert_list(names, n, st)
        call goto_char(point_min())
        buffers(cur)%read_only = .true.
        call set_buffer(old, st)
        ! Now attempt to display the buffer...
        call current_window_configuration(config)
        call split_window(selected_window(), 0, .false., .false., new, status)

        if (status%ok()) then
            call select_window(new, status)
            if (status%ok()) call switch_to_buffer("*Ibuffer confirmation*", status)

            if (status%ok()) then
                call fit_window_to_buffer(selected_window())
                write (count, "(i0)") n
                call read_y_or_n("Really "//operation//" "//trim(count)//" buffers? ", answer, status)
            end if
        end if

        call kill_buffer(buf, st)
        call set_window_configuration(config)
        call set_buffer(old, st)
    end subroutine ibuffer_confirm_operation_on

    ! ibuffer-map-lines-nomodify.  Port of
    ! ibuffer.el:ibuffer-map-lines-nomodify.
    recursive integer function map_lines_nomodify(func, status) result(count)
        procedure(map_proc) :: func
        type(status_type), intent(out) :: status

        count = ibuffer_map_lines(func, status, nomodify=.true.)
    end function map_lines_nomodify

    !> The names of the buffers marked with MARK, in NAMES(1:N) (in
    !> reverse order, as upstream).  Port of
    !> ibuffer.el:ibuffer-buffer-names-with-mark.
    recursive subroutine ibuffer_buffer_names_with_mark(mark, names, n, status)
        character, intent(in) :: mark
        character(len=256), allocatable, intent(out) :: names(:)
        integer, intent(out) :: n
        type(status_type), intent(out) :: status
        integer :: count, k

        map_mark = mark
        map_nnames = 0
        if (allocated(map_names)) deallocate (map_names)
        allocate (map_names(16))
        count = map_lines_nomodify(collect_name, status)
        n = map_nnames
        allocate (names(max(1, n)))

        ! (Pushed onto a list: the last line first.)
        do k = 1, n
            names(k) = map_names(n - k + 1)
        end do

        if (count < 0) n = 0
    end subroutine ibuffer_buffer_names_with_mark

    ! Not a port (a lambda of ibuffer.el).
    recursive integer function collect_name(buf, mark) result(r)
        type(handle_type), intent(in) :: buf
        character, intent(in) :: mark
        character(len=256), allocatable :: grown(:)

        r = MAP_NIL
        if (mark /= map_mark) return

        if (map_nnames == size(map_names)) then
            allocate (grown(2*size(map_names)))
            grown(1:map_nnames) = map_names(1:map_nnames)
            call move_alloc(grown, map_names)
        end if

        map_nnames = map_nnames + 1
        map_names(map_nnames) = buffers(buf%index)%name
    end function collect_name

    !> The names of the marked buffers.  Port of
    !> ibuffer.el:ibuffer-marked-buffer-names.
    recursive subroutine ibuffer_marked_buffer_names(names, n, status)
        character(len=256), allocatable, intent(out) :: names(:)
        integer, intent(out) :: n
        type(status_type), intent(out) :: status

        call ibuffer_buffer_names_with_mark(ibuffer_marked_char, names, n, status)
    end subroutine ibuffer_marked_buffer_names

    !> The names of the buffers marked for deletion.  Port of
    !> ibuffer.el:ibuffer-deletion-marked-buffer-names.
    recursive subroutine ibuffer_deletion_marked_buffer_names(names, n, status)
        character(len=256), allocatable, intent(out) :: names(:)
        integer, intent(out) :: n
        type(status_type), intent(out) :: status

        call ibuffer_buffer_names_with_mark(ibuffer_deletion_char, names, n, status)
    end subroutine ibuffer_deletion_marked_buffer_names

    !> The number of marked lines (with ALL, of lines with any mark).
    !> Port of ibuffer.el:ibuffer-count-marked-lines.
    recursive integer function ibuffer_count_marked_lines(status, all) result(count)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: all
        logical :: any_mark

        any_mark = .false.
        if (present(all)) any_mark = all

        if (any_mark) then
            count = map_lines_nomodify(any_marked, status)
        else
            map_mark = ibuffer_marked_char
            count = map_lines_nomodify(mark_is, status)
        end if
    end function ibuffer_count_marked_lines

    ! Not a port (a lambda of ibuffer.el).
    recursive integer function any_marked(buf, mark) result(r)
        type(handle_type), intent(in) :: buf
        character, intent(in) :: mark

        if (buf%index < 0) continue
        r = MAP_NIL
        if (mark /= " ") r = MAP_T
    end function any_marked

    ! Not a port (a lambda of ibuffer.el).
    recursive integer function mark_is(buf, mark) result(r)
        type(handle_type), intent(in) :: buf
        character, intent(in) :: mark

        if (buf%index < 0) continue
        r = MAP_NIL
        if (mark == map_mark) r = MAP_T
    end function mark_is

    ! ibuffer-map-deletion-lines.  Port of
    ! ibuffer.el:ibuffer-map-deletion-lines.
    recursive integer function map_deletion_lines(func, status) result(count)
        procedure(map_proc) :: func
        type(status_type), intent(out) :: status

        count = map_on_mark(ibuffer_deletion_char, func, status)
    end function map_deletion_lines

    !> The file name of the current buffer, else its
    !> `list_buffers_directory', else its Dired directory ("" for none).
    !> Port of ibuffer.el:ibuffer-buffer-file-name.
    function ibuffer_buffer_file_name() result(name)
        character(len=:), allocatable :: name
        character(len=:), allocatable :: dirname
        type(status_type) :: st

        name = ""

        if (allocated(buffers(cur)%filename)) then
            if (len(buffers(cur)%filename) > 0) then
                name = buffers(cur)%filename
                return
            end if
        end if

        if (allocated(list_buffers_directory%value)) then
            if (len(list_buffers_directory%value) > 0) then
                name = list_buffers_directory%value
                return
            end if
        end if

        dirname = dired_directory_of()
        if (len(dirname) > 0) call expand_file_name(dirname, name, st)
    end function ibuffer_buffer_file_name

    !> Abbreviate FILENAME using `ibuffer_directory_abbrev_table'.  Port of
    !> ibuffer.el:ibuffer--abbreviate-file-name.
    function ibuffer_abbreviate_file_name(filename) result(abbrev)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: abbrev
        integer :: k
        integer(int64) :: m
        type(status_type) :: st

        abbrev = filename

        if (allocated(ibuffer_directory_abbrev_table)) then
            do k = 1, size(ibuffer_directory_abbrev_table)
                m = string_match(trim(ibuffer_directory_abbrev_table(k)%from), abbrev, st)

                if (st%ok() .and. m == 1) then
                    call apply_abbrev(k)
                end if
            end do
        end if

        abbrev = abbreviate_file_name(abbrev)

    contains

        subroutine apply_abbrev(i)
            use search_fm, only: match_end
            integer, intent(in) :: i
            integer(int64) :: e
            type(status_type) :: st2

            e = match_end(0, st2)
            abbrev = trim(ibuffer_directory_abbrev_table(i)%to)//abbrev(e:)
        end subroutine apply_abbrev
    end function ibuffer_abbreviate_file_name

    ! ------------------------------------------------------------------
    ! Operations
    ! ------------------------------------------------------------------

    !> Run the operation OP on the marked buffers (or the buffer on this
    !> line), as a command made by define-ibuffer-op does.  Port of
    !> ibuf-macs.el:define-ibuffer-op (the function it defines).
    recursive subroutine ibuffer_operate(op, status)
        type(ibuffer_op_type), intent(in) :: op
        type(status_type), intent(out) :: status
        type(ibuffer_op_type) :: saved_op
        character(len=256), allocatable :: names(:)
        type(handle_type) :: buf
        integer :: n, count, k
        logical :: answer
        character(len=24) :: c
        type(status_type) :: st

        call assert_ibuffer_mode(status)
        if (.not. status%ok()) return
        k = ibuffer_local_index(.true.)
        locals(k)%did_modification = .false.

        if (op%deletion) then
            call ibuffer_deletion_marked_buffer_names(names, n, status)
        else
            call ibuffer_marked_buffer_names(names, n, status)
        end if

        if (.not. status%ok()) return

        if (n == 0) then
            if (line_serial(pos_bol()) == 0) then
                call set_error(status, FM_GENERIC_ERROR, "No buffer on this line")
                return
            end if

            buf = ibuffer_current_buffer(st)
            n = 1
            names(1) = ""
            if (buf%index /= 0) names(1) = buffers(buf%index)%name

            if (op%deletion) then
                call ibuffer_set_mark(ibuffer_deletion_char, status)
            else
                call ibuffer_set_mark(ibuffer_marked_char, status)
            end if

            if (.not. status%ok()) return
        end if

        if (op%dangerous) then
            call ibuffer_confirm_operation_on(trim(op%active_opstring), names, n, answer, status)
            if (.not. status%ok() .or. .not. answer) return
        end if

        ! (Operations may nest: one runs another.)
        saved_op = current_op
        current_op = op

        if (associated(op%before)) then
            call op%before(status)

            if (.not. status%ok()) then
                current_op = saved_op
                return
            end if
        end if

        if (op%deletion) then
            count = map_deletion_lines(op_line, status)
        else
            count = map_marked_lines(op_line, status)
        end if

        current_op = saved_op
        if (.not. status%ok()) return
        k = ibuffer_local_index(.true.)
        if (op%modifier_p == IBUFFER_MODIFIER_T) locals(k)%did_modification = .true.

        if (associated(op%after)) then
            call op%after(status)
            if (.not. status%ok()) return
        end if

        call ibuffer_redisplay(status, .true.)
        write (c, "(i0)") count

        if (count == 1) then
            call message("Operation finished; "//trim(op%opstring)//" "//trim(c)//" buffer")
        else
            call message("Operation finished; "//trim(op%opstring)//" "//trim(c)//" buffers")
        end if
    end subroutine ibuffer_operate

    ! The mapping function of an operation: its BODY for BUF.  Not a port
    ! (the lambda define-ibuffer-op maps).
    recursive integer function op_line(buf, mark) result(r)
        type(handle_type), intent(in) :: buf
        character, intent(in) :: mark
        type(handle_type) :: old
        type(status_type) :: st, ignore
        logical :: before, after
        integer(int64) :: saved
        integer :: k

        r = MAP_NIL
        before = .false.
        if (current_op%modifier_p == IBUFFER_MODIFIER_MAYBE) call is_buffer_modified(buf, before, ignore)

        if (current_op%complex) then
            call current_op%body(buf, mark, r, st)
        else
            old = current_buffer()
            call set_buffer(buf, st)
            saved = point()
            call current_op%body(buf, mark, r, st)

            if (is_buffer_live(buf)) then
                call set_buffer(buf, ignore)
                call goto_char(min(saved, point_max()))
            end if

            call set_buffer(old, ignore)
            r = MAP_T
        end if

        if (.not. st%ok()) call message(format_message_of(st))

        if (current_op%modifier_p == IBUFFER_MODIFIER_MAYBE .and. is_buffer_live(buf)) then
            call is_buffer_modified(buf, after, ignore)

            if (before .neqv. after) then
                k = ibuffer_local_index(.true.)
                locals(k)%did_modification = .true.
            end if
        end if
    end function op_line

    ! Not a port.
    function format_message_of(st) result(text)
        use error_fm, only: format_error
        type(status_type), intent(in) :: st
        character(len=:), allocatable :: text

        text = format_error(st)
    end function format_message_of

    !> Save marked buffers as with `save_buffer'.  Port of
    !> ibuffer.el:ibuffer-do-save.
    recursive subroutine ibuffer_do_save(status)
        type(status_type), intent(out) :: status
        type(ibuffer_op_type) :: op

        op%complex = .true.
        op%opstring = "saved"
        op%modifier_p = IBUFFER_MODIFIER_MAYBE
        op%body => save_body
        call ibuffer_operate(op, status)
    end subroutine ibuffer_do_save

    ! Not a port (the body of ibuffer.el's define-ibuffer-op do-save).
    recursive subroutine save_body(buf, mark, result, status)
        type(handle_type), intent(in) :: buf
        character, intent(in) :: mark
        integer, intent(out) :: result
        type(status_type), intent(out) :: status
        type(handle_type) :: old
        type(window_configuration_type) :: config
        type(status_type) :: st
        logical :: modified

        if (mark == achar(0)) continue
        result = MAP_T
        call is_buffer_modified(buf, modified, status)
        if (.not. status%ok() .or. .not. modified) return
        old = current_buffer()

        if (.not. allocated(buffers(buf%index)%filename)) then
            ! Handle the case where we're prompted for a file name.
            call current_window_configuration(config)
            call switch_to_buffer(buffers(buf%index)%name, status)
            if (status%ok()) call save_buffer(1_int64, .false., status)
            call set_window_configuration(config)
        else
            call set_buffer(buf, status)
            if (status%ok()) call save_buffer(1_int64, .false., status)
        end if

        call set_buffer(old, st)
    end subroutine save_body

    !> Toggle modification flag of marked buffers.  Port of
    !> ibuffer.el:ibuffer-do-toggle-modified.
    recursive subroutine ibuffer_do_toggle_modified(status)
        type(status_type), intent(out) :: status
        type(ibuffer_op_type) :: op

        op%opstring = "(un)marked as modified"
        op%modifier_p = IBUFFER_MODIFIER_T
        op%body => toggle_modified_body
        call ibuffer_operate(op, status)
    end subroutine ibuffer_do_toggle_modified

    ! Not a port (the body of ibuffer.el's define-ibuffer-op
    ! do-toggle-modified).
    recursive subroutine toggle_modified_body(buf, mark, result, status)
        type(handle_type), intent(in) :: buf
        character, intent(in) :: mark
        integer, intent(out) :: result
        type(status_type), intent(out) :: status
        logical :: modified

        if (mark == achar(0)) continue
        result = MAP_T
        call is_buffer_modified(buf, modified, status)
        call restore_buffer_modified(.not. modified)
    end subroutine toggle_modified_body

    !> Toggle read only status in marked buffers.
    !> If optional ARG is a non-negative integer, make buffers read only.
    !> If ARG is a negative integer or 0, make buffers writable.
    !> Otherwise, toggle read only status.  Port of
    !> ibuffer.el:ibuffer-do-toggle-read-only.
    recursive subroutine ibuffer_do_toggle_read_only(status, arg)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg
        type(ibuffer_op_type) :: op

        read_only_arg = huge(0_int64)
        if (present(arg)) read_only_arg = arg
        op%opstring = "toggled read only status in"
        op%modifier_p = IBUFFER_MODIFIER_T
        op%body => toggle_read_only_body
        call ibuffer_operate(op, status)
    end subroutine ibuffer_do_toggle_read_only

    recursive subroutine cmd_ibuffer_do_toggle_read_only(status)
        type(status_type), intent(out) :: status

        ! (integerp ARG): a numeric prefix argument, not C-u.
        if (fixnump(current_prefix_arg)) then
            call ibuffer_do_toggle_read_only(status, xfixnum(current_prefix_arg))
        else if (.not. nilp(current_prefix_arg) .and. .not. consp(current_prefix_arg)) then
            call ibuffer_do_toggle_read_only(status, prefix_numeric_value(current_prefix_arg))
        else
            call ibuffer_do_toggle_read_only(status)
        end if
    end subroutine cmd_ibuffer_do_toggle_read_only

    ! Not a port (the body of ibuffer.el's define-ibuffer-op
    ! do-toggle-read-only).
    recursive subroutine toggle_read_only_body(buf, mark, result, status)
        type(handle_type), intent(in) :: buf
        character, intent(in) :: mark
        integer, intent(out) :: result
        type(status_type), intent(out) :: status

        if (mark == achar(0) .or. buf%index < 0) continue
        result = MAP_T

        if (read_only_arg == huge(0_int64)) then
            call minor_modes%read_only_mode%toggle(status)
        else if (read_only_arg > 0) then
            call minor_modes%read_only_mode%enable(status)
        else
            call minor_modes%read_only_mode%disable(status)
        end if
    end subroutine toggle_read_only_body

    !> Toggle locked status in marked buffers.  (emacs-lock.el is not
    !> ported: this does nothing.)  Port of
    !> ibuffer.el:ibuffer-do-toggle-lock.
    recursive subroutine ibuffer_do_toggle_lock(status)
        type(status_type), intent(out) :: status

        status = status_type()
    end subroutine ibuffer_do_toggle_lock

    !> Kill marked buffers as with `kill_this_buffer'.  Port of
    !> ibuffer.el:ibuffer-do-delete.
    recursive subroutine ibuffer_do_delete(status)
        type(status_type), intent(out) :: status
        type(ibuffer_op_type) :: op

        op%opstring = "killed"
        op%active_opstring = "kill"
        op%dangerous = .true.
        op%complex = .true.
        op%modifier_p = IBUFFER_MODIFIER_T
        op%body => kill_body
        call ibuffer_operate(op, status)
    end subroutine ibuffer_do_delete

    !> Kill buffers marked for deletion as with `kill_this_buffer'.
    !> Port of ibuffer.el:ibuffer-do-kill-on-deletion-marks.
    recursive subroutine ibuffer_do_kill_on_deletion_marks(status)
        type(status_type), intent(out) :: status
        type(ibuffer_op_type) :: op

        op%opstring = "killed"
        op%active_opstring = "kill"
        op%dangerous = .true.
        op%complex = .true.
        op%deletion = .true.
        op%modifier_p = IBUFFER_MODIFIER_T
        op%body => kill_body
        call ibuffer_operate(op, status)
    end subroutine ibuffer_do_kill_on_deletion_marks

    ! Not a port (the body of ibuffer.el's define-ibuffer-op
    ! do-kill-on-deletion-marks).
    recursive subroutine kill_body(buf, mark, result, status)
        type(handle_type), intent(in) :: buf
        character, intent(in) :: mark
        integer, intent(out) :: result
        type(status_type), intent(out) :: status
        type(handle_type) :: old
        type(status_type) :: st

        if (mark == achar(0)) continue
        old = current_buffer()
        call kill_buffer(buf, status)
        call set_buffer(old, st)

        if (status%ok() .and. .not. is_buffer_live(buf)) then
            result = MAP_KILL
        else
            result = MAP_NIL
        end if
    end subroutine kill_body

    ! ------------------------------------------------------------------
    ! Marks
    ! ------------------------------------------------------------------

    !> Unmark all buffers with mark MARK (RET means all).  Port of
    !> ibuffer.el:ibuffer-unmark-all.
    recursive subroutine ibuffer_unmark_all(mark, status)
        character, intent(in) :: mark
        type(status_type), intent(out) :: status
        integer :: count

        if (ibuffer_count_marked_lines(status, .true.) == 0) then
            call message(substitute_command_keys("No buffers marked; use \<ibuffer_mode_map>" &
                                                 //"\[ibuffer_mark_forward] to mark a buffer"))
        else if (mark == achar(13)) then
            count = ibuffer_map_lines(unmark_any, status)
        else
            count = map_on_mark(mark, unmark_any, status)
        end if

        if (.not. status%ok()) return
        call ibuffer_redisplay(status, .true.)
    end subroutine ibuffer_unmark_all

    recursive subroutine cmd_ibuffer_unmark_all(status)
        type(status_type), intent(out) :: status
        integer(int64) :: c

        call read_char("Remove marks (RET means all):", c, status)
        if (.not. status%ok()) return

        if (c > 127) then
            call ibuffer_unmark_all(achar(0), status)
        else
            call ibuffer_unmark_all(achar(c), status)
        end if
    end subroutine cmd_ibuffer_unmark_all

    ! Not a port (a lambda of ibuffer.el).
    recursive integer function unmark_any(buf, mark) result(r)
        type(handle_type), intent(in) :: buf
        character, intent(in) :: mark

        if (buf%index < 0) continue
        if (mark /= " ") call ibuffer_set_mark_1(" ")
        r = MAP_T
    end function unmark_any

    !> Remove all marks from all marked buffers in Ibuffer.  Port of
    !> ibuffer.el:ibuffer-unmark-all-marks.
    recursive subroutine ibuffer_unmark_all_marks(status)
        type(status_type), intent(out) :: status

        call ibuffer_unmark_all(achar(13), status)
    end subroutine ibuffer_unmark_all_marks

    !> Toggle which buffers are marked.
    !> In other words, unmarked buffers become marked, and marked buffers
    !> become unmarked.
    !> If point is on a group name, then this function operates on that
    !> group.  Port of ibuffer.el:ibuffer-toggle-marks.
    recursive subroutine ibuffer_toggle_marks(status, group)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: group
        character(len=:), allocatable :: g
        character(len=24) :: c
        integer :: count
        logical :: has_group

        has_group = present(group)
        if (has_group) g = group

        if (has_string_prop(point(), "ibuffer_filter_group_name")) then
            g = string_prop(point(), "ibuffer_filter_group_name")
            has_group = .true.
        end if

        if (has_group) then
            count = ibuffer_map_lines(toggle_mark, status, group=g)
        else
            count = ibuffer_map_lines(toggle_mark, status)
        end if

        if (.not. status%ok()) return
        write (c, "(i0)") count
        call message(trim(c)//" buffers marked")
        call ibuffer_redisplay(status, .true.)
    end subroutine ibuffer_toggle_marks

    recursive subroutine cmd_ibuffer_toggle_marks(status)
        type(status_type), intent(out) :: status

        call ibuffer_toggle_marks(status)
    end subroutine cmd_ibuffer_toggle_marks

    ! Not a port (a lambda of ibuffer.el).
    recursive integer function toggle_mark(buf, mark) result(r)
        type(handle_type), intent(in) :: buf
        character, intent(in) :: mark

        if (buf%index < 0) continue
        r = MAP_NIL

        if (mark == ibuffer_marked_char) then
            call ibuffer_set_mark_1(" ")
        else if (mark == " ") then
            call ibuffer_set_mark_1(ibuffer_marked_char)
            r = MAP_T
        end if
    end function toggle_mark

    !> Change all OLD marks to NEW marks.
    !> OLD and NEW are both characters used to mark buffers.  Port of
    !> ibuffer.el:ibuffer-change-marks.
    recursive subroutine ibuffer_change_marks(old, new, status)
        character, intent(in) :: old, new
        type(status_type), intent(out) :: status
        integer :: count
        character(len=24) :: c

        status = status_type()

        if (old == achar(13) .or. new == achar(13)) then
            call ding()
            return
        end if

        map_mark = old
        map_new = new
        count = ibuffer_map_lines(change_mark, status)
        if (.not. status%ok()) return
        write (c, "(i0)") count
        call message(trim(c)//" marks changed")
    end subroutine ibuffer_change_marks

    recursive subroutine cmd_ibuffer_change_marks(status)
        type(status_type), intent(out) :: status
        integer(int64) :: old, new

        call read_char("Change (old mark): ", old, status)
        if (.not. status%ok()) return
        call read_char("Change "//achar(min(old, 127_int64))//" marks to (new mark): ", new, status)
        if (.not. status%ok()) return
        call ibuffer_change_marks(achar(min(old, 127_int64)), achar(min(new, 127_int64)), status)
    end subroutine cmd_ibuffer_change_marks

    ! Not a port (a lambda of ibuffer.el).
    recursive integer function change_mark(buf, mark) result(r)
        type(handle_type), intent(in) :: buf
        character, intent(in) :: mark
        type(status_type) :: st

        if (buf%index < 0) continue
        r = MAP_NIL

        if (mark == map_mark) then
            call ibuffer_set_mark(map_new, st)
            r = MAP_T
        end if
    end function change_mark

    ! ibuffer-get-region-and-prefix: START, END (when the region is
    ! active) and ARG.  Port of ibuffer.el:ibuffer-get-region-and-prefix.
    subroutine get_region_and_prefix(has_region, start, end, arg)
        use editfns_fm, only: region_beginning, region_end
        logical, intent(out) :: has_region
        integer(int64), intent(out) :: start, end, arg
        type(status_type) :: st

        arg = prefix_numeric_value(current_prefix_arg)
        has_region = is_use_region()
        start = 0
        end = 0

        if (has_region) then
            call region_beginning(start, st)
            call region_end(end, st)
        end if
    end subroutine get_region_and_prefix

    !> Mark the buffers in the region, or ARG buffers.
    !> If point is on a group name, this function operates on that group.
    !> Port of ibuffer.el:ibuffer-mark-forward.
    recursive subroutine ibuffer_mark_forward(status, arg)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg

        call mark_command(ibuffer_marked_char, 1_int64, status, arg)
    end subroutine ibuffer_mark_forward

    recursive subroutine cmd_ibuffer_mark_forward(status)
        type(status_type), intent(out) :: status

        call mark_command_interactive(ibuffer_marked_char, 1_int64, status)
    end subroutine cmd_ibuffer_mark_forward

    !> Unmark the buffers in the region, or ARG buffers.
    !> If point is on a group name, this function operates on that group.
    !> Port of ibuffer.el:ibuffer-unmark-forward.
    recursive subroutine ibuffer_unmark_forward(status, arg)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg

        call mark_command(" ", 1_int64, status, arg)
    end subroutine ibuffer_unmark_forward

    recursive subroutine cmd_ibuffer_unmark_forward(status)
        type(status_type), intent(out) :: status

        call mark_command_interactive(" ", 1_int64, status)
    end subroutine cmd_ibuffer_unmark_forward

    !> Unmark the buffers in the region, or previous ARG buffers.
    !> If point is on a group name, this function operates on that group.
    !> Port of ibuffer.el:ibuffer-unmark-backward.
    recursive subroutine ibuffer_unmark_backward(status, arg)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg

        call mark_command(" ", -1_int64, status, arg)
    end subroutine ibuffer_unmark_backward

    recursive subroutine cmd_ibuffer_unmark_backward(status)
        type(status_type), intent(out) :: status

        call mark_command_interactive(" ", -1_int64, status)
    end subroutine cmd_ibuffer_unmark_backward

    !> Mark for deletion the buffers in the region, or ARG buffers.
    !> If point is on a group name, this function operates on that group.
    !> Port of ibuffer.el:ibuffer-mark-for-delete.
    recursive subroutine ibuffer_mark_for_delete(status, arg)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg

        call mark_command(ibuffer_deletion_char, 1_int64, status, arg)
    end subroutine ibuffer_mark_for_delete

    recursive subroutine cmd_ibuffer_mark_for_delete(status)
        type(status_type), intent(out) :: status

        call mark_command_interactive(ibuffer_deletion_char, 1_int64, status)
    end subroutine cmd_ibuffer_mark_for_delete

    !> Mark for deletion the ARG previous buffers.
    !> If point is on a group name, this function operates on that group.
    !> Port of ibuffer.el:ibuffer-mark-for-delete-backwards.
    recursive subroutine ibuffer_mark_for_delete_backwards(status, arg)
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg
        integer(int64) :: n

        n = 1
        if (present(arg)) n = arg
        call ibuffer_mark_interactive(n, ibuffer_deletion_char, status, -1_int64)
    end subroutine ibuffer_mark_for_delete_backwards

    recursive subroutine cmd_ibuffer_mark_for_delete_backwards(status)
        type(status_type), intent(out) :: status

        call ibuffer_mark_for_delete_backwards(status, prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_ibuffer_mark_for_delete_backwards

    ! The mark commands called from Fortran: ARG buffers (1 by default)
    ! in DIRECTION.  Not a port.
    recursive subroutine mark_command(mark, direction, status, arg)
        character, intent(in) :: mark
        integer(int64), intent(in) :: direction
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: arg
        integer(int64) :: n

        n = 1
        if (present(arg)) n = arg
        call ibuffer_mark_interactive(direction*n, mark, status)
    end subroutine mark_command

    ! The mark commands called interactively: the region, or ARG buffers
    ! in DIRECTION (ibuffer-mark-region-or-n-with-char).  Port of
    ! ibuffer.el:ibuffer-mark-region-or-n-with-char.
    recursive subroutine mark_command_interactive(mark, direction, status)
        character, intent(in) :: mark
        integer(int64), intent(in) :: direction
        type(status_type), intent(out) :: status
        integer(int64) :: start, end, arg, here
        logical :: has_region

        call get_region_and_prefix(has_region, start, end, arg)

        if (has_region) then
            here = point()
            call goto_char(start)
            call ibuffer_mark_interactive(count_lines(start, end), mark, status)
            call goto_char(here)
        else
            call ibuffer_mark_interactive(direction*arg, mark, status)
        end if
    end subroutine mark_command_interactive

    ! ibuffer-mark-interactive.  Port of
    ! ibuffer.el:ibuffer-mark-interactive.
    recursive subroutine ibuffer_mark_interactive(arg, mark, status, movement)
        integer(int64), intent(in) :: arg
        character, intent(in) :: mark
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: movement
        integer(int64) :: n
        character(len=:), allocatable :: group

        call assert_ibuffer_mode(status)
        if (.not. status%ok()) return
        n = arg

        ! deprecated movement argument
        if (present(movement)) then
            if (movement < 0) n = -n
        end if

        call ibuffer_forward_line(0_int64)

        if (has_string_prop(point(), "ibuffer_filter_group_name")) then
            group = string_prop(point(), "ibuffer_filter_group_name")
            call mark_on_buffer_identity(mark, group, status)
            return
        end if

        call ibuffer_forward_line(0_int64, .true.)

        do while (n > 0)
            call ibuffer_set_mark(mark, status)
            if (.not. status%ok()) return
            call ibuffer_forward_line(1_int64, .true.)
            n = n - 1
        end do

        do while (n < 0)
            call ibuffer_forward_line(-1_int64, .true.)
            call ibuffer_set_mark(mark, status)
            if (.not. status%ok()) return
            n = n + 1
        end do
    end subroutine ibuffer_mark_interactive

    ! (ibuffer-mark-on-buffer #'identity MARK GROUP).  Port of
    ! ibuf-ext.el:ibuffer-mark-on-buffer, in part (with identity).
    recursive subroutine mark_on_buffer_identity(mark, group, status)
        character, intent(in) :: mark
        character(len=*), intent(in) :: group
        type(status_type), intent(out) :: status
        integer :: count
        character(len=24) :: c

        map_mark = mark
        count = ibuffer_map_lines(mark_always, status, group=group)
        if (.not. status%ok()) return
        call ibuffer_redisplay(status, .true.)

        if (mark /= " ") then
            write (c, "(i0)") count
            call message("Marked "//trim(c)//" buffers")
        end if
    end subroutine mark_on_buffer_identity

    ! Not a port (a lambda of ibuf-ext.el).
    recursive integer function mark_always(buf, mark) result(r)
        type(handle_type), intent(in) :: buf
        character, intent(in) :: mark

        if (buf%index < 0 .or. mark == achar(0)) continue
        call ibuffer_set_mark_1(map_mark)
        r = MAP_T
    end function mark_always

    !> Set the mark of the buffer on the current line to MARK, and
    !> redisplay the line.  Port of ibuffer.el:ibuffer-set-mark.
    recursive subroutine ibuffer_set_mark(mark, status)
        character, intent(in) :: mark
        type(status_type), intent(out) :: status
        logical :: ro
        integer :: k

        call assert_ibuffer_mode(status)
        if (.not. status%ok()) return
        ro = buffers(cur)%read_only
        buffers(cur)%read_only = .false.
        call ibuffer_set_mark_1(mark)
        k = ibuffer_local_index(.true.)
        locals(k)%did_modification = .true.
        call ibuffer_redisplay_current(status)
        buffers(cur)%read_only = ro
        call beginning_of_line()
    end subroutine ibuffer_set_mark

    !> ibuffer-set-mark-1: set the mark of the current line.  Port of
    !> ibuffer.el:ibuffer-set-mark-1.
    recursive subroutine ibuffer_set_mark_1(mark)
        character, intent(in) :: mark
        integer(int64) :: beg, end, serial
        type(status_type) :: st
        logical :: ro

        beg = pos_bol()
        end = pos_eol()
        serial = line_serial(beg)
        ro = buffers(cur)%read_only
        buffers(cur)%read_only = .false.
        call put_text_property(beg, end, sym("ibuffer_properties"), make_fixnum(serial), st)
        call put_text_property(beg, end, sym("ibuffer_mark"), make_fixnum(int(iachar(mark), int64)), st)
        buffers(cur)%read_only = ro
    end subroutine ibuffer_set_mark_1

    ! ------------------------------------------------------------------
    ! Formats
    ! ------------------------------------------------------------------

    ! ibuffer-current-format: the index of the current format (0-based).
    ! Port of ibuffer.el:ibuffer-current-format.
    integer function current_format_index() result(n)
        integer :: k

        k = ibuffer_local_index(.true.)
        if (locals(k)%current_format < 0) locals(k)%current_format = 0
        n = locals(k)%current_format
    end function current_format_index

    ! The current format's entries.  Not a port.
    subroutine current_format_entries(entries, status)
        type(ibuffer_column_spec_type), allocatable, intent(out) :: entries(:)
        type(status_type), intent(out) :: status
        type(ibuffer_formats_type) :: f
        integer :: n

        status = status_type()
        f = formats_of()

        if (f%count() == 0) then
            call set_error(status, FM_GENERIC_ERROR, "No formats!")
            allocate (entries(0))
            return
        end if

        n = current_format_index()
        if (n >= f%count()) n = 0
        allocate (entries(size(f%formats(n + 1)%entries)))
        entries = f%formats(n + 1)%entries
    end subroutine current_format_entries

    ! The index in `ibuffer_columns' of the column NAME (0 for none).
    ! Not a port.
    integer function column_index(name) result(k)
        character(len=*), intent(in) :: name

        do k = 1, size(ibuffer_columns)
            if (ibuffer_columns(k)%name == name) return
        end do

        k = 0
    end function column_index

    ! ibuffer-expand-format-entry: check SPEC's column, and give MIN and
    ! MAX their values.  Port of ibuffer.el:ibuffer-expand-format-entry.
    subroutine expand_format_entry(spec, column, min, max, status)
        type(ibuffer_column_spec_type), intent(in) :: spec
        integer, intent(out) :: column, min, max
        type(status_type), intent(out) :: status

        status = status_type()
        column = 0
        min = spec%min_size
        max = spec%max_size
        if (len_trim(spec%name) == 0) return
        column = column_index(trim(spec%name))

        if (column == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Unknown column "//trim(spec%name)//" in ibuffer-formats")
            return
        end if

        if (associated(spec%min_function)) min = spec%min_function()
        if (associated(spec%max_function)) max = spec%max_function()
    end subroutine expand_format_entry

    !> Recompile `ibuffer_formats': check that their columns exist.
    !> (Fortmacs interprets the formats.)  Port of
    !> ibuffer.el:ibuffer-recompile-formats.
    recursive subroutine ibuffer_recompile_formats(status)
        type(status_type), intent(out) :: status
        type(ibuffer_formats_type) :: f
        integer :: k, j, column, min, max

        status = status_type()
        f = formats_of()

        do k = 1, f%count()
            do j = 1, size(f%formats(k)%entries)
                call expand_format_entry(f%formats(k)%entries(j), column, min, max, status)
                if (.not. status%ok()) return
            end do
        end do
    end subroutine ibuffer_recompile_formats

    ! ibuffer-clear-summary-columns.  Port of
    ! ibuffer.el:ibuffer-clear-summary-columns.
    subroutine clear_summary_columns()
        integer :: k

        if (allocated(summaries)) deallocate (summaries)
        allocate (summaries(size(ibuffer_columns)))

        do k = 1, size(summaries)
            allocate (summaries(k)%cells(8))
        end do
    end subroutine clear_summary_columns

    ! Record CELL in the summary of column K.  Not a port.
    subroutine add_summary(k, cell)
        integer, intent(in) :: k
        type(ibuffer_cell_type), intent(in) :: cell
        type(ibuffer_cell_type), allocatable :: grown(:)
        integer :: j

        if (.not. allocated(summaries)) call clear_summary_columns()
        if (k > size(summaries)) return

        associate (s => summaries(k))
            if (s%n == size(s%cells)) then
                allocate (grown(2*size(s%cells)))

                do j = 1, s%n
                    grown(j) = s%cells(j)
                end do

                call move_alloc(grown, s%cells)
            end if

            s%n = s%n + 1
            s%cells(s%n) = cell
        end associate
    end subroutine add_summary

    ! Insert the column of SPEC for BUFFER marked MARK at point (one entry
    ! of the function `ibuffer-compile-format' makes).  Port of
    ! ibuffer.el:ibuffer-compile-format, in part (the insertion of a column
    ! by the function it makes).
    recursive subroutine insert_column(spec, buffer, mark, status)
        type(ibuffer_column_spec_type), intent(in) :: spec
        type(handle_type), intent(in) :: buffer
        character, intent(in) :: mark
        type(status_type), intent(out) :: status
        type(ibuffer_cell_type) :: cell
        type(handle_type) :: menu
        character(len=:), allocatable :: str, ellipsis_text
        integer :: column, min, max, strlen, tmp1, tmp2, offset, ell_at, ell_len, str_len
        integer(int64) :: beg
        logical :: from_end_p, min_used, max_used
        type(status_type) :: st

        if (len_trim(spec%name) == 0) then
            call insert(spec%text(1:spec%text_len), status)
            return
        end if

        call expand_format_entry(spec, column, min, max, status)
        if (.not. status%ok()) return
        from_end_p = min < 0
        if (from_end_p) min = -min
        ! The column's body, with BUFFER current.
        menu = current_buffer()
        call set_buffer(buffer, st)
        cell = ibuffer_columns(column)%make(buffer, mark)
        call set_buffer(menu, st)
        if (.not. allocated(cell%text)) cell%text = ""
        if (.not. allocated(cell%face)) cell%face = ""
        str = cell%text
        ! The original text is STR(OFFSET+1:OFFSET+STR_LEN) of the result;
        ! an ellipsis ELL_LEN long is at ELL_AT (0 for none).
        offset = 0
        str_len = len(str)
        ell_at = 0
        ell_len = 0
        min_used = min >= 0
        max_used = max > 0
        strlen = int(string_width(str))

        if (min_used .and. strlen < min) then
            tmp1 = min - strlen
            tmp2 = tmp1/2

            select case (spec%align)
            case (IBUFFER_ALIGN_RIGHT)
                offset = tmp1
                str = repeat(" ", tmp2)//repeat(" ", tmp1 - tmp2)//str
            case (IBUFFER_ALIGN_CENTER)
                offset = tmp2
                str = repeat(" ", tmp2)//str//repeat(" ", tmp1 - tmp2)
            case default
                str = str//repeat(" ", tmp2)//repeat(" ", tmp1 - tmp2)
            end select
        else if (max_used .and. strlen > max) then
            if (from_end_p) then
                str = truncate_string_to_width(str, strlen, strlen - max, " ")
            else
                str = truncate_string_to_width(str, max, padding=" ")
            end if

            str_len = len(str)
            strlen = int(string_width(str))

            if (spec%elide .and. strlen > 5) then
                ellipsis_text = ibuffer_eliding_string

                if (from_end_p) then
                    str = ellipsis_text//substring_after_width(str, int(string_width(ibuffer_eliding_string)))
                    ell_at = 1
                    offset = len(ellipsis_text)
                    str_len = len(str) - offset
                else
                    str = truncate_string_to_width(str, strlen - int(string_width(ellipsis_text)), padding=".") &
                          //ellipsis_text
                    ell_at = len(str) - len(ellipsis_text) + 1
                    str_len = ell_at - 1
                end if

                ell_len = len(ellipsis_text)
            end if
        end if

        beg = point()
        call insert(str, status)
        if (.not. status%ok()) return

        ! The column's properties on its text (concat keeps them; the
        ! padding has none).
        if (str_len > 0) then
            if (len(cell%face) > 0) call put_text_property(beg + offset, beg + offset + str_len, &
                                                           sym("font_lock_face"), sym(cell%face), st)
            if (allocated(cell%mouse_face)) call put_text_property(beg + offset, beg + offset + str_len, &
                                                                   sym("mouse_face"), sym(cell%mouse_face), st)
            if (cell%name_column) call put_text_property(beg + offset, beg + offset + str_len, &
                                                         sym("ibuffer_name_column"), t_value(), st)
        end if

        if (ell_at > 0) call put_text_property(beg + ell_at - 1, beg + ell_at - 1 + ell_len, sym("font_lock_face"), &
                                               sym("bold"), st)

        if (associated(ibuffer_columns(column)%summarizer)) then
            cell%text = str
            call add_summary(column, cell)
        end if
    end subroutine insert_column

    ! (substring STR (string-width ...)) for ASCII-width text: STR after
    ! its first N characters.  Not a port.
    function substring_after_width(str, n) result(s)
        character(len=*), intent(in) :: str
        integer, intent(in) :: n
        character(len=:), allocatable :: s

        if (n >= len(str)) then
            s = ""
        else
            s = str(n + 1:)
        end if
    end function substring_after_width

    ! ibuffer-format-column: STR padded with WIDTH spaces by ALIGNMENT.
    ! Port of ibuffer.el:ibuffer-format-column.
    function ibuffer_format_column(str, width, alignment) result(s)
        character(len=*), intent(in) :: str
        integer, intent(in) :: width, alignment
        character(len=:), allocatable :: s
        character(len=:), allocatable :: left, right

        left = repeat(" ", width/2)
        right = repeat(" ", width - width/2)

        select case (alignment)
        case (IBUFFER_ALIGN_RIGHT)
            s = left//right//str
        case (IBUFFER_ALIGN_CENTER)
            s = left//str//right
        case default
            s = str//left//right
        end select
    end function ibuffer_format_column

    !> The face of the name of BUF, marked with MARK (upstream a symbol;
    !> "" for nil).  Port of ibuffer.el:ibuffer-buffer-name-face.
    recursive function ibuffer_buffer_name_face(buf, mark) result(face)
        type(handle_type), intent(in) :: buf
        character, intent(in) :: mark
        character(len=:), allocatable :: face
        type(handle_type) :: old
        type(status_type) :: st
        integer :: level, k
        logical :: hit

        face = ""

        if (mark == ibuffer_marked_char) then
            face = ibuffer_marked_face
        else if (mark == ibuffer_deletion_char) then
            face = ibuffer_deletion_face
        else if (allocated(ibuffer_fontification_table)) then
            level = -1
            old = current_buffer()

            do k = 1, size(ibuffer_fontification_table)
                if (ibuffer_fontification_table(k)%priority <= level) cycle
                if (.not. associated(ibuffer_fontification_table(k)%form)) cycle
                call set_buffer(buf, st)
                hit = ibuffer_fontification_table(k)%form()
                call set_buffer(old, st)

                if (hit) then
                    level = ibuffer_fontification_table(k)%priority
                    face = trim(ibuffer_fontification_table(k)%face)
                end if
            end do
        end if
    end function ibuffer_buffer_name_face

    ! Insert a line describing BUFFER and MARK using the current format
    ! ENTRIES.  Port of ibuffer.el:ibuffer-insert-buffer-line.
    recursive subroutine ibuffer_insert_buffer_line(buffer, mark, entries, status)
        type(handle_type), intent(in) :: buffer
        character, intent(in) :: mark
        type(ibuffer_column_spec_type), intent(in) :: entries(:)
        type(status_type), intent(out) :: status
        integer(int64) :: beg
        type(status_type) :: st
        integer :: k

        call assert_ibuffer_mode(status)
        if (.not. status%ok()) return
        beg = point()

        do k = 1, size(entries)
            call insert_column(entries(k), buffer, mark, status)
            if (.not. status%ok()) return
        end do

        if (point() > beg) then
            call put_text_property(beg, point(), sym("ibuffer_properties"), make_fixnum(buffers(buffer%index)%serial), st)
            call put_text_property(beg, point(), sym("ibuffer_mark"), make_fixnum(int(iachar(mark), int64)), st)
        end if

        call insert(achar(10), status)
    end subroutine ibuffer_insert_buffer_line

    ! Redisplay the line of the buffer at point.  Port of
    ! ibuffer.el:ibuffer-redisplay-current.
    recursive subroutine ibuffer_redisplay_current(status)
        type(status_type), intent(out) :: status
        type(ibuffer_column_spec_type), allocatable :: entries(:)
        type(handle_type) :: buf
        type(status_type) :: st
        character :: mark
        integer(int64) :: saved, shortage, m
        logical :: ro

        call assert_ibuffer_mode(status)
        if (.not. status%ok()) return
        if (is_eob()) shortage = forward_line(-1_int64)
        call beginning_of_line()
        call current_format_entries(entries, status)
        if (.not. status%ok()) return
        call clear_summary_columns()
        buf = ibuffer_current_buffer(st)
        if (buf%index == 0) return
        mark = ibuffer_current_mark()
        saved = point()
        ro = buffers(cur)%read_only
        buffers(cur)%read_only = .false.
        call delete_region(point(), min(point_max(), pos_eol() + 1), status)
        if (status%ok()) call ibuffer_insert_buffer_line(buf, mark, entries, status)
        buffers(cur)%read_only = ro
        m = saved
        call goto_char(m)
        if (shrink_of() /= IBUFFER_SHRINK_NIL) call ibuffer_shrink_to_fit()
    end subroutine ibuffer_redisplay_current

    ! Not a port.
    integer function shrink_of() result(shrink)
        integer :: k

        k = ibuffer_local_index(.true.)
        shrink = locals(k)%shrink
    end function shrink_of

    ! ibuffer-map-on-mark: FUNC on the lines marked with MARK.  Port of
    ! ibuffer.el:ibuffer-map-on-mark.
    recursive integer function map_on_mark(mark, func, status) result(count)
        character, intent(in) :: mark
        procedure(map_proc) :: func
        type(status_type), intent(out) :: status
        procedure(map_proc), pointer :: saved_fn
        character :: saved_mark

        saved_fn => on_mark_fn
        saved_mark = on_mark_mark
        on_mark_fn => func
        on_mark_mark = mark
        count = ibuffer_map_lines(on_mark, status)
        on_mark_fn => saved_fn
        on_mark_mark = saved_mark
    end function map_on_mark

    ! Not a port (the lambda of ibuffer-map-on-mark).
    recursive integer function on_mark(buf, mk) result(r)
        type(handle_type), intent(in) :: buf
        character, intent(in) :: mk

        r = MAP_NIL
        if (mk == on_mark_mark) r = on_mark_fn(buf, on_mark_mark)
    end function on_mark

    !> Call FUNCTION for each buffer.
    !> Set the ibuffer modification flag unless NOMODIFY is true.
    !>
    !> If optional argument GROUP is present, then only call FUNCTION on
    !> buffers in filtering group GROUP.
    !>
    !> FUNCTION is called with two arguments:
    !> the buffer object itself and the current mark symbol.  Return the
    !> number of lines it affected.  Port of
    !> ibuffer.el:ibuffer-map-lines.
    recursive integer function ibuffer_map_lines(func, status, nomodify, group) result(count)
        procedure(map_proc) :: func
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: nomodify
        character(len=*), intent(in), optional :: group
        integer(int64) :: orig_target_line, target_line_offset, start, shortage, saved
        integer :: total, result
        type(handle_type) :: buf, m
        type(status_type) :: st
        logical :: nomod, in_group
        integer :: k

        call assert_ibuffer_mode(status)
        count = 0
        if (.not. status%ok()) return
        nomod = .false.
        if (present(nomodify)) nomod = nomodify
        call ibuffer_forward_line(0_int64)
        saved = point()
        call goto_char(point_min())
        call ibuffer_forward_line(0_int64)
        start = point()
        call goto_char(saved)
        orig_target_line = 1 + count_lines(start, point())
        target_line_offset = orig_target_line
        total = 0
        buffers(cur)%read_only = .false.
        call goto_char(point_min())
        call ibuffer_forward_line(0_int64, .true.)

        do
            if (is_eob() .or. has_prop(point(), "ibuffer_summary")) exit
            call ibuffer_forward_line(0_int64, .true.)
            if (is_eob() .or. has_prop(point(), "ibuffer_summary")) exit
            buf = ibuffer_current_buffer(st)

            if (is_buffer_live(buf)) then
                in_group = .true.
                if (present(group)) in_group = string_prop(point(), "ibuffer_filter_group") == group &
                                               .and. has_string_prop(point(), "ibuffer_filter_group")

                if (in_group) then
                    ! (save-excursion)
                    call point_marker(m)
                    result = func(buf, ibuffer_current_mark())
                    call marker_position(m, saved, st)
                    call free_marker(m, st)
                    call goto_char(saved)
                else
                    result = MAP_NIL
                end if
            else
                ! Kill the line if the buffer is dead
                result = MAP_KILL
            end if

            ! A given mapping function should return:
            ! nil if it chose not to affect the buffer
            ! `kill' means the remove line from the buffer list
            ! t otherwise
            total = total + 1

            select case (result)
            case (MAP_NIL)
                shortage = forward_line(1_int64)
            case (MAP_KILL)
                call delete_region(pos_bol(), min(point_max(), pos_eol() + 1), st)
                count = count + 1
                if (total < orig_target_line) target_line_offset = target_line_offset - 1
            case default
                count = count + 1
                shortage = forward_line(1_int64)
            end select
        end do

        ! With `ibuffer_auto_mode' enabled, `ibuffer_expert' false and more
        ! than one marked buffer lines, the preceding loop counts the
        ! automatically popped up (and hence not user-marked) buffer
        ! "*Ibuffer confirmation*".
        k = ibuffer_local_index(.true.)
        if (minor_modes%ibuffer_auto_mode%enabled() .and. count > 1 .and. .not. ibuffer_expert) count = count - 1
        buffers(cur)%read_only = .true.
        if (.not. nomod) call restore_buffer_modified(.false.)
        call goto_char(point_min())
        call ibuffer_forward_line(0_int64)
        call ibuffer_forward_line(target_line_offset - 1)
    end function ibuffer_map_lines

    !> Return in BUFS the buffers currently marked.  Port of
    !> ibuffer.el:ibuffer-get-marked-buffers.
    recursive subroutine ibuffer_get_marked_buffers(bufs, status)
        type(handle_type), allocatable, intent(out) :: bufs(:)
        type(status_type), intent(out) :: status
        type(ibuffer_state_type), allocatable :: states(:)
        integer :: k, n

        call ibuffer_current_state_list(states, status)
        allocate (bufs(size(states)))
        n = 0

        do k = 1, size(states)
            if (states(k)%mark /= ibuffer_marked_char) cycle
            n = n + 1
            bufs(n) = states(k)%buffer
        end do

        bufs = bufs(1:n)
    end subroutine ibuffer_get_marked_buffers

    !> Return in STATES the buffers of the Ibuffer buffer with their marks
    !> (BUF . MARK); with POS, also the point at the beginning of each
    !> line (BUF MARK POINT).  Port of ibuffer.el:ibuffer-current-state-list.
    recursive subroutine ibuffer_current_state_list(states, status, pos)
        type(ibuffer_state_type), allocatable, intent(out) :: states(:)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: pos
        type(ibuffer_state_type), allocatable :: saved(:)
        integer :: count, saved_n
        logical :: saved_pos

        ! (Nested calls keep the outer list.)
        call move_alloc(map_states, saved)
        saved_n = map_nstates
        saved_pos = map_with_pos
        map_with_pos = .false.
        if (present(pos)) map_with_pos = pos
        allocate (map_states(16))
        map_nstates = 0
        count = map_lines_nomodify(collect_state, status)
        allocate (states(map_nstates))
        if (map_nstates > 0) states = map_states(1:map_nstates)
        deallocate (map_states)
        call move_alloc(saved, map_states)
        map_nstates = saved_n
        map_with_pos = saved_pos
        if (count < 0) continue
    end subroutine ibuffer_current_state_list

    ! Not a port (the lambda of ibuffer-current-state-list).
    recursive integer function collect_state(buf, mark) result(r)
        type(handle_type), intent(in) :: buf
        character, intent(in) :: mark
        type(ibuffer_state_type), allocatable :: grown(:)

        r = MAP_NIL
        if (.not. is_buffer_live(buf)) return

        if (map_nstates == size(map_states)) then
            allocate (grown(2*size(map_states)))
            grown(1:map_nstates) = map_states(1:map_nstates)
            call move_alloc(grown, map_states)
        end if

        map_nstates = map_nstates + 1
        map_states(map_nstates)%buffer = buf
        map_states(map_nstates)%mark = mark
        if (map_with_pos) map_states(map_nstates)%point = point()
    end function collect_state

    ! ibuffer-current-buffers-with-marks: (BUF . MARK) for each of
    ! CURBUFS, with the marks the buffers have in the list.  Port of
    ! ibuffer.el:ibuffer-current-buffers-with-marks.
    recursive subroutine current_buffers_with_marks(curbufs, states, status)
        type(handle_type), intent(in) :: curbufs(:)
        type(ibuffer_state_type), allocatable, intent(out) :: states(:)
        type(status_type), intent(out) :: status
        type(ibuffer_state_type), allocatable :: bufs(:)
        integer :: k, j

        call ibuffer_current_state_list(bufs, status)
        allocate (states(size(curbufs)))

        do k = 1, size(curbufs)
            states(k)%buffer = curbufs(k)
            states(k)%mark = " "

            do j = 1, size(bufs)
                if (bufs(j)%buffer%index == curbufs(k)%index .and. &
                    bufs(j)%buffer%generation == curbufs(k)%generation) then
                    states(k)%mark = bufs(j)%mark
                    exit
                end if
            end do
        end do
    end subroutine current_buffers_with_marks

    !> ibuffer-buf-matches-predicates.  Port of
    !> ibuffer.el:ibuffer-buf-matches-predicates.
    recursive logical function buf_matches_predicates(buf, predicates) result(hit)
        type(handle_type), intent(in) :: buf
        type(ibuffer_show_predicate_type), intent(in) :: predicates(:)
        integer :: k
        integer(int64) :: m
        type(status_type) :: st

        hit = .false.

        do k = 1, size(predicates)
            if (len_trim(predicates(k)%regexp) > 0) then
                m = string_match(trim(predicates(k)%regexp), buffers(buf%index)%name, st)
                if (st%ok() .and. m > 0) hit = .true.
            else if (associated(predicates(k)%predicate)) then
                if (predicates(k)%predicate(buf)) hit = .true.
            end if
        end do
    end function buf_matches_predicates

    !> (ibuffer-buf-matches-predicates BUF REGEXPS) for a list of regexps.
    !> Port of ibuffer.el:ibuffer-buf-matches-predicates, for regexps.
    logical function buf_matches_regexps(buf, regexps) result(hit)
        type(handle_type), intent(in) :: buf
        character(len=*), intent(in) :: regexps(:)
        integer :: k
        integer(int64) :: m
        type(status_type) :: st

        hit = .false.

        do k = 1, size(regexps)
            m = string_match(trim(regexps(k)), buffers(buf%index)%name, st)
            if (st%ok() .and. m > 0) hit = .true.
        end do
    end function buf_matches_regexps

    ! ibuffer-filter-buffers.  Port of ibuffer.el:ibuffer-filter-buffers.
    recursive subroutine filter_buffers(ibuffer_buf, last, bmarklist, all, result, status)
        type(handle_type), intent(in) :: ibuffer_buf, last
        type(ibuffer_state_type), intent(in) :: bmarklist(:)
        logical, intent(in) :: all
        type(ibuffer_state_type), allocatable, intent(out) :: result(:)
        type(status_type), intent(out) :: status
        integer :: k, n
        logical :: keep

        status = status_type()
        allocate (result(size(bmarklist)))
        n = 0

        do k = 1, size(bmarklist)
            ! This takes precedence over anything else
            keep = .false.

            if (ibuffer_always_show_last_buffer /= IBUFFER_SHOW_LAST_NEVER .and. last%index /= 0) then
                keep = last%index == bmarklist(k)%buffer%index
            end if

            if (.not. keep) then
                keep = is_ibuffer_ext_visible(bmarklist(k)%buffer, all, ibuffer_buf, status)
                if (.not. status%ok()) return
            end if

            if (keep) then
                n = n + 1
                result(n) = bmarklist(k)
            end if
        end do

        result = result(1:n)
    end subroutine filter_buffers

    !> The default member of `ibuffer_maybe_show_predicates'.
    !> True if BUF is not visiting a file and its name begins with a
    !> space.  Port of ibuffer.el:ibuffer-hidden-buffer-p.
    recursive logical function is_ibuffer_hidden_buffer(buf) result(hidden)
        type(handle_type), intent(in) :: buf

        hidden = .false.
        if (.not. is_buffer_live(buf)) return

        associate (b => buffers(buf%index))
            if (len(b%name) == 0) return
            if (b%name(1:1) /= " ") return
            hidden = .true.
            if (allocated(b%filename)) hidden = len(b%filename) == 0
        end associate
    end function is_ibuffer_hidden_buffer

    ! ibuffer-hidden-buffer-p of the current buffer (a fontification
    ! form).  Not a port.
    recursive logical function hidden_current()
        hidden_current = is_ibuffer_hidden_buffer(current_buffer())
    end function hidden_current

    ! ------------------------------------------------------------------
    ! Columns
    ! ------------------------------------------------------------------

    ! A column cell of TEXT.  Not a port.
    function text_cell(text) result(cell)
        character(len=*), intent(in) :: text
        type(ibuffer_cell_type) :: cell

        cell%text = text
        cell%face = ""
    end function text_cell

    ! define-ibuffer-column mark.  Port of ibuffer.el's
    ! (define-ibuffer-column mark).
    recursive function column_mark(buffer, mark) result(cell)
        type(handle_type), intent(in) :: buffer
        character, intent(in) :: mark
        type(ibuffer_cell_type) :: cell

        if (buffer%index < 0) continue
        cell = text_cell(mark)
    end function column_mark

    ! define-ibuffer-column read-only.  Port of ibuffer.el's
    ! (define-ibuffer-column read-only).
    recursive function column_read_only(buffer, mark) result(cell)
        type(handle_type), intent(in) :: buffer
        character, intent(in) :: mark
        type(ibuffer_cell_type) :: cell

        if (mark == achar(0)) continue

        if (buffers(buffer%index)%read_only) then
            cell = text_cell(ibuffer_read_only_char)
        else
            cell = text_cell(" ")
        end if
    end function column_read_only

    ! define-ibuffer-column locked (emacs-lock-mode is not ported).  Port of
    ! ibuffer.el's (define-ibuffer-column locked).
    recursive function column_locked(buffer, mark) result(cell)
        type(handle_type), intent(in) :: buffer
        character, intent(in) :: mark
        type(ibuffer_cell_type) :: cell

        if (buffer%index < 0 .or. mark == achar(0)) continue
        cell = text_cell(" ")
        cell%face = "ibuffer_locked_buffer"
    end function column_locked

    ! define-ibuffer-column modified.  Port of ibuffer.el's
    ! (define-ibuffer-column modified).
    recursive function column_modified(buffer, mark) result(cell)
        type(handle_type), intent(in) :: buffer
        character, intent(in) :: mark
        type(ibuffer_cell_type) :: cell
        type(status_type) :: st
        logical :: modified

        if (mark == achar(0)) continue
        call is_buffer_modified(buffer, modified, st)

        if (modified) then
            cell = text_cell(ibuffer_modified_char)
        else
            cell = text_cell(" ")
        end if
    end function column_modified

    ! define-ibuffer-column name.  Port of ibuffer.el's
    ! (define-ibuffer-column name).
    recursive function column_name(buffer, mark) result(cell)
        type(handle_type), intent(in) :: buffer
        character, intent(in) :: mark
        type(ibuffer_cell_type) :: cell
        character(len=:), allocatable :: name
        integer :: k

        name = ""

        do k = 1, len(buffers(buffer%index)%name)
            if (buffers(buffer%index)%name(k:k) == achar(10)) then
                name = name//"^J"
            else
                name = name//buffers(buffer%index)%name(k:k)
            end if
        end do

        cell%text = name
        cell%face = ibuffer_buffer_name_face(buffer, mark)
        cell%mouse_face = "highlight"
        cell%name_column = .true.
    end function column_name

    ! Not a port (the summarizer of the name column).
    subroutine summarize_name(cells, text)
        type(ibuffer_cell_type), intent(in) :: cells(:)
        character(len=:), allocatable, intent(out) :: text
        character(len=24) :: c

        if (size(cells) == 0) then
            text = "No buffers"
        else if (size(cells) == 1) then
            text = "1 buffer"
        else
            write (c, "(i0)") size(cells)
            text = trim(c)//" buffers"
        end if
    end subroutine summarize_name

    ! define-ibuffer-column size.  Port of ibuffer.el's
    ! (define-ibuffer-column size).
    recursive function column_size(buffer, mark) result(cell)
        type(handle_type), intent(in) :: buffer
        character, intent(in) :: mark
        type(ibuffer_cell_type) :: cell
        integer(int64) :: size
        character(len=24) :: c

        if (buffer%index < 0 .or. mark == achar(0)) continue
        size = buffer_size()

        if (ibuffer_human_readable_size) then
            cell = text_cell(file_size_human_readable(real(size, real64)))
        else
            write (c, "(i0)") size
            cell = text_cell(trim(c))
        end if

        cell%size = size
    end function column_size

    ! Not a port (the summarizer of the size column).
    subroutine summarize_size(cells, text)
        type(ibuffer_cell_type), intent(in) :: cells(:)
        character(len=:), allocatable, intent(out) :: text
        integer(int64) :: total
        integer :: k
        character(len=24) :: c

        total = 0

        do k = 1, size(cells)
            if (cells(k)%size >= 0) total = total + cells(k)%size
        end do

        if (ibuffer_human_readable_size) then
            text = file_size_human_readable(real(total, real64))
        else
            write (c, "(i0)") total
            text = trim(c)
        end if
    end subroutine summarize_size

    ! define-ibuffer-column recency.  Port of ibuffer.el's
    ! (define-ibuffer-column recency).
    recursive function column_recency(buffer, mark) result(cell)
        type(handle_type), intent(in) :: buffer
        character, intent(in) :: mark
        type(ibuffer_cell_type) :: cell
        integer(int64) :: sec
        integer(int32) :: nsec
        real(real64) :: since

        if (mark == achar(0)) continue

        associate (b => buffers(buffer%index))
            if (b%display_time_set) then
                call emacs_clock_realtime(sec, nsec)
                since = real(sec - b%display_time_sec, real64) + real(nsec - b%display_time_nsec, real64)*1.0e-9_real64
                cell = text_cell(seconds_to_string(since, SECONDS_TO_STRING_READABLE, .true.)//" ago")
            else
                cell = text_cell("never")
            end if
        end associate
    end function column_recency

    ! Not a port.
    subroutine summarize_ignore(cells, text)
        type(ibuffer_cell_type), intent(in) :: cells(:)
        character(len=:), allocatable, intent(out) :: text

        if (size(cells) < 0) continue
        text = ""
    end subroutine summarize_ignore

    ! define-ibuffer-column mode.  Port of ibuffer.el's
    ! (define-ibuffer-column mode).
    recursive function column_mode(buffer, mark) result(cell)
        type(handle_type), intent(in) :: buffer
        character, intent(in) :: mark
        type(ibuffer_cell_type) :: cell

        if (mark == achar(0)) continue
        cell = text_cell(mode_name_of(buffer))
        cell%mouse_face = "highlight"
    end function column_mode

    !> (format-mode-line mode-name nil nil BUFFER).  Not a port.
    function mode_name_of(buffer) result(name)
        type(handle_type), intent(in) :: buffer
        character(len=:), allocatable :: name

        name = "Fundamental"
        if (allocated(buffers(buffer%index)%mode_name)) name = buffers(buffer%index)%mode_name
    end function mode_name_of

    ! "(PROC STATUS)" of buffer BUFFER's process ("" for none).  Not a port.
    function process_text(buffer) result(text)
        type(handle_type), intent(in) :: buffer
        character(len=:), allocatable :: text
        type(handle_type) :: proc

        text = ""
        proc = get_buffer_process(buffer)
        if (proc%index == 0) return
        text = "("//process_name(proc)//" "//process_status(proc)//")"
    end function process_text

    ! define-ibuffer-column process.  Port of ibuffer.el's
    ! (define-ibuffer-column process).
    recursive function column_process(buffer, mark) result(cell)
        type(handle_type), intent(in) :: buffer
        character, intent(in) :: mark
        type(ibuffer_cell_type) :: cell

        if (mark == achar(0)) continue
        cell = text_cell(process_text(buffer))
    end function column_process

    ! Not a port (the summarizer of the process column).
    subroutine summarize_process(cells, text)
        type(ibuffer_cell_type), intent(in) :: cells(:)
        character(len=:), allocatable, intent(out) :: text

        text = count_text(cells, "No processes", "1 process", " processes")
    end subroutine summarize_process

    ! "No ...", "1 ..." or "N ..." of the non-empty CELLS.  Not a port.
    function count_text(cells, none, one, many) result(text)
        type(ibuffer_cell_type), intent(in) :: cells(:)
        character(len=*), intent(in) :: none, one, many
        character(len=:), allocatable :: text
        integer :: total, k
        character(len=24) :: c

        total = 0

        do k = 1, size(cells)
            if (len(cells(k)%text) > 0) total = total + 1
        end do

        if (total == 0) then
            text = none
        else if (total == 1) then
            text = one
        else
            write (c, "(i0)") total
            text = trim(c)//many
        end if
    end function count_text

    ! define-ibuffer-column filename.  Port of ibuffer.el's
    ! (define-ibuffer-column filename).
    recursive function column_filename(buffer, mark) result(cell)
        type(handle_type), intent(in) :: buffer
        character, intent(in) :: mark
        type(ibuffer_cell_type) :: cell

        if (buffer%index < 0 .or. mark == achar(0)) continue
        cell = text_cell(ibuffer_abbreviate_file_name(ibuffer_buffer_file_name()))
    end function column_filename

    ! Not a port (the summarizer of the filename column).
    subroutine summarize_filename(cells, text)
        type(ibuffer_cell_type), intent(in) :: cells(:)
        character(len=:), allocatable, intent(out) :: text

        text = count_text(cells, "No files", "1 file", " files")
    end subroutine summarize_filename

    ! define-ibuffer-column filename-and-process.  Port of ibuffer.el's
    ! (define-ibuffer-column filename-and-process).
    recursive function column_filename_and_process(buffer, mark) result(cell)
        type(handle_type), intent(in) :: buffer
        character, intent(in) :: mark
        type(ibuffer_cell_type) :: cell
        character(len=:), allocatable :: proc, filename

        proc = process_text(buffer)
        cell = column_filename(buffer, mark)
        filename = cell%text

        if (len(proc) > 0) then
            ! (The process part is in italics upstream.)
            if (len(filename) > 0) then
                cell%text = proc//" "//filename
            else
                cell%text = proc
            end if

            cell%process = .true.
        end if
    end function column_filename_and_process

    ! Not a port (the summarizer of the filename-and-process column).
    subroutine summarize_filename_and_process(cells, text)
        type(ibuffer_cell_type), intent(in) :: cells(:)
        character(len=:), allocatable, intent(out) :: text
        integer :: procs, files, k
        character(len=24) :: c

        procs = 0
        files = 0

        do k = 1, size(cells)
            if (len(cells(k)%text) == 0) cycle
            if (cells(k)%process) procs = procs + 1
            files = files + 1
        end do

        if (files == 0) then
            text = "No files"
        else if (files == 1) then
            text = "1 file"
        else
            write (c, "(i0)") files
            text = trim(c)//" files"
        end if

        text = text//", "

        if (procs == 0) then
            text = text//"no processes"
        else if (procs == 1) then
            text = text//"1 process"
        else
            write (c, "(i0)") procs
            text = text//trim(c)//" processes"
        end if
    end subroutine summarize_filename_and_process

    !> Add a column for use with `ibuffer_formats': NAME, its BODY (MAKE)
    !> called with the buffer current, its TITLE (the capitalized NAME by
    !> default) and an optional SUMMARIZER.  A column with the same name is
    !> replaced.  Port of ibuf-macs.el:define-ibuffer-column.
    subroutine define_ibuffer_column(name, make, title, summarizer)
        character(len=*), intent(in) :: name
        procedure(ibuffer_column_proc) :: make
        character(len=*), intent(in), optional :: title
        procedure(ibuffer_summarizer_proc), optional :: summarizer
        type(ibuffer_column_type), allocatable :: grown(:)
        integer :: k, n

        if (.not. allocated(ibuffer_columns)) allocate (ibuffer_columns(0))
        k = column_index(name)

        if (k == 0) then
            n = size(ibuffer_columns)
            allocate (grown(n + 1))
            grown(1:n) = ibuffer_columns
            call move_alloc(grown, ibuffer_columns)
            k = n + 1
        end if

        ibuffer_columns(k)%name = name

        if (present(title)) then
            ibuffer_columns(k)%title = title
        else
            ibuffer_columns(k)%title = capitalize_name(name)
        end if

        ibuffer_columns(k)%make => make
        ibuffer_columns(k)%summarizer => null()
        if (present(summarizer)) ibuffer_columns(k)%summarizer => summarizer
    end subroutine define_ibuffer_column

    ! (capitalize (symbol-name SYMBOL)) of a column name (words separated
    ! by underscores).  Not a port.
    function capitalize_name(name) result(s)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: s
        integer :: k
        logical :: start

        s = name
        start = .true.

        do k = 1, len(s)
            if (start .and. s(k:k) >= "a" .and. s(k:k) <= "z") s(k:k) = achar(iachar(s(k:k)) - 32)
            start = .not. (s(k:k) >= "a" .and. s(k:k) <= "z" .or. s(k:k) >= "A" .and. s(k:k) <= "Z" &
                           .or. s(k:k) >= "0" .and. s(k:k) <= "9")
        end do
    end function capitalize_name

    !> Add a method of sorting named NAME, described by DESCRIPTION, with
    !> the predicate LESS.  Port of ibuf-macs.el:define-ibuffer-sorter
    !> (the entry of `ibuffer-sorting-functions-alist').
    subroutine define_ibuffer_sorter(name, description, less)
        character(len=*), intent(in) :: name, description
        procedure(ibuffer_sorter_proc) :: less
        type(ibuffer_sorter_type), allocatable :: grown(:)
        integer :: k, n

        if (.not. allocated(ibuffer_sorting_functions)) allocate (ibuffer_sorting_functions(0))

        do k = 1, size(ibuffer_sorting_functions)
            if (ibuffer_sorting_functions(k)%name == name) exit
        end do

        if (k > size(ibuffer_sorting_functions)) then
            n = size(ibuffer_sorting_functions)
            allocate (grown(n + 1))
            grown(2:n + 1) = ibuffer_sorting_functions
            call move_alloc(grown, ibuffer_sorting_functions)
            ! (Pushed onto the table.)
            k = 1
        end if

        ibuffer_sorting_functions(k)%name = name
        ibuffer_sorting_functions(k)%description = description
        ibuffer_sorting_functions(k)%less => less
    end subroutine define_ibuffer_sorter

    !> Define a filter named NAME.
    !> DESCRIPTION is a short string describing the filter.
    !>
    !> TEST should return whether a particular buffer should be displayed
    !> or not, given the buffer and the qualifier of the filter
    !> specification.  (The command `ibuffer_filter_by_NAME' that applies
    !> it is a separate procedure, made with `ibuffer_filter_by'.)  Port of
    !> ibuf-macs.el:define-ibuffer-filter.
    subroutine define_ibuffer_filter(name, description, test)
        character(len=*), intent(in) :: name, description
        procedure(ibuffer_filter_test_proc) :: test
        type(ibuffer_filter_def_type), allocatable :: grown(:)
        integer :: n

        if (.not. allocated(ibuffer_filtering_table)) allocate (ibuffer_filtering_table(0))
        n = size(ibuffer_filtering_table)
        allocate (grown(n + 1))
        grown(2:n + 1) = ibuffer_filtering_table
        ! (Pushed onto the table.)
        grown(1)%name = name
        grown(1)%description = description
        grown(1)%test => test
        call move_alloc(grown, ibuffer_filtering_table)
    end subroutine define_ibuffer_filter

    !> Limit current view to buffers with name matching QUALIFIER.  Port of
    !> ibuf-ext.el:ibuffer-filter-by-name.
    recursive subroutine ibuffer_filter_by_name(qualifier, status)
        character(len=*), intent(in) :: qualifier
        type(status_type), intent(out) :: status

        call ibuffer_filter_by("name", status, qualifier=qualifier)
    end subroutine ibuffer_filter_by_name

    !> Limit current view to buffers running a process.  Port of
    !> ibuf-ext.el:ibuffer-filter-by-process.
    recursive subroutine ibuffer_filter_by_process(status)
        type(status_type), intent(out) :: status

        call ibuffer_filter_by("process", status)
    end subroutine ibuffer_filter_by_process

    !> Limit current view to buffers with name beginning and ending
    !> with *, along with an optional suffix of the form digits or
    !> <digits>.  Port of ibuf-ext.el:ibuffer-filter-by-starred-name.
    recursive subroutine ibuffer_filter_by_starred_name(status)
        type(status_type), intent(out) :: status

        call ibuffer_filter_by("starred_name", status)
    end subroutine ibuffer_filter_by_starred_name

    !> Limit current view to buffers with full file name matching
    !> QUALIFIER.
    !>
    !> For example, for a buffer associated with file '/a/b/c.d', this
    !> matches against '/a/b/c.d'.  Port of
    !> ibuf-ext.el:ibuffer-filter-by-filename.
    recursive subroutine ibuffer_filter_by_filename(qualifier, status)
        character(len=*), intent(in) :: qualifier
        type(status_type), intent(out) :: status

        call ibuffer_filter_by("filename", status, qualifier=qualifier)
    end subroutine ibuffer_filter_by_filename

    !> Limit current view to buffers with file basename matching
    !> QUALIFIER.
    !>
    !> For example, for a buffer associated with file `/a/b/c.d', this
    !> matches against `c.d'.  Port of ibuf-ext.el:ibuffer-filter-by-basename.
    recursive subroutine ibuffer_filter_by_basename(qualifier, status)
        character(len=*), intent(in) :: qualifier
        type(status_type), intent(out) :: status

        call ibuffer_filter_by("basename", status, qualifier=qualifier)
    end subroutine ibuffer_filter_by_basename

    !> Limit current view to buffers with filename extension matching
    !> QUALIFIER.
    !>
    !> The separator character (typically `.') is not part of the
    !> pattern.  For example, for a buffer associated with file
    !> `/a/b/c.d', this matches against `d'.  Port of
    !> ibuf-ext.el:ibuffer-filter-by-file-extension.
    recursive subroutine ibuffer_filter_by_file_extension(qualifier, status)
        character(len=*), intent(in) :: qualifier
        type(status_type), intent(out) :: status

        call ibuffer_filter_by("file_extension", status, qualifier=qualifier)
    end subroutine ibuffer_filter_by_file_extension

    !> Limit current view to buffers with directory matching QUALIFIER.
    !>
    !> For a buffer associated with file '/a/b/c.d', this matches
    !> against '/a/b'.  For a buffer not associated with a file, this
    !> matches against the value of `default_directory' in that buffer.
    !> Port of ibuf-ext.el:ibuffer-filter-by-directory.
    recursive subroutine ibuffer_filter_by_directory(qualifier, status)
        character(len=*), intent(in) :: qualifier
        type(status_type), intent(out) :: status

        call ibuffer_filter_by("directory", status, qualifier=qualifier)
    end subroutine ibuffer_filter_by_directory

    !> Limit current view to buffers with size greater than QUALIFIER.
    !> Port of ibuf-ext.el:ibuffer-filter-by-size-gt.
    recursive subroutine ibuffer_filter_by_size_gt(qualifier, status)
        integer(int64), intent(in) :: qualifier
        type(status_type), intent(out) :: status

        call ibuffer_filter_by("size_gt", status, number=qualifier)
    end subroutine ibuffer_filter_by_size_gt

    !> Limit current view to buffers with size less than QUALIFIER.
    !> Port of ibuf-ext.el:ibuffer-filter-by-size-lt.
    recursive subroutine ibuffer_filter_by_size_lt(qualifier, status)
        integer(int64), intent(in) :: qualifier
        type(status_type), intent(out) :: status

        call ibuffer_filter_by("size_lt", status, number=qualifier)
    end subroutine ibuffer_filter_by_size_lt

    !> Limit current view to buffers that are marked as modified.  Port of
    !> ibuf-ext.el:ibuffer-filter-by-modified.
    recursive subroutine ibuffer_filter_by_modified(status)
        type(status_type), intent(out) :: status

        call ibuffer_filter_by("modified", status)
    end subroutine ibuffer_filter_by_modified

    !> Limit current view to buffers that are visiting a file.  Port of
    !> ibuf-ext.el:ibuffer-filter-by-visiting-file.
    recursive subroutine ibuffer_filter_by_visiting_file(status)
        type(status_type), intent(out) :: status

        call ibuffer_filter_by("visiting_file", status)
    end subroutine ibuffer_filter_by_visiting_file

    !> Limit current view to buffers whose contents match QUALIFIER.
    !> Port of ibuf-ext.el:ibuffer-filter-by-content.
    recursive subroutine ibuffer_filter_by_content(qualifier, status)
        character(len=*), intent(in) :: qualifier
        type(status_type), intent(out) :: status

        call ibuffer_filter_by("content", status, qualifier=qualifier)
    end subroutine ibuffer_filter_by_content

    !> Sort the buffers by how recently they've been used.  Port of
    !> ibuffer.el define-ibuffer-sorter recency.
    recursive logical function sort_recency(a, b) result(less)
        type(handle_type), intent(in) :: a, b

        ! (time-less-p (display time of B or 0) (display time of A or 0))
        less = time_less(b, a)
    end function sort_recency

    ! Whether the display time of A is less than B's (an unset time is 0).
    ! Not a port.
    logical function time_less(a, b)
        type(handle_type), intent(in) :: a, b
        integer(int64) :: sa, sb
        integer(int32) :: na, nb

        sa = 0
        na = 0
        sb = 0
        nb = 0

        if (buffers(a%index)%display_time_set) then
            sa = buffers(a%index)%display_time_sec
            na = buffers(a%index)%display_time_nsec
        end if

        if (buffers(b%index)%display_time_set) then
            sb = buffers(b%index)%display_time_sec
            nb = buffers(b%index)%display_time_nsec
        end if

        time_less = sa < sb .or. (sa == sb .and. na < nb)
    end function time_less

    !> Sort the buffers by how recently they've been used.  Port of
    !> ibuffer.el:ibuffer-do-sort-by-recency.
    recursive subroutine ibuffer_do_sort_by_recency(status)
        type(status_type), intent(out) :: status

        call ibuffer_do_sort_by("recency", status)
    end subroutine ibuffer_do_sort_by_recency

    !> Select the sorting method NAME (reversing it if already selected)
    !> and redisplay: the command define-ibuffer-sorter defines.  Port of
    !> ibuf-macs.el:define-ibuffer-sorter, in part (the command it defines).
    recursive subroutine ibuffer_do_sort_by(name, status)
        character(len=*), intent(in) :: name
        type(status_type), intent(out) :: status
        integer :: k

        k = ibuffer_local_index(.true.)
        locals(k)%sorting_mode = name

        if (locals(k)%sorting_mode == locals(k)%last_sorting_mode) then
            locals(k)%sorting_reversep = .not. locals(k)%sorting_reversep
        end if

        call ibuffer_redisplay(status, .true.)
        k = ibuffer_local_index(.true.)
        locals(k)%last_sorting_mode = name
    end subroutine ibuffer_do_sort_by

    ! ------------------------------------------------------------------
    ! Display
    ! ------------------------------------------------------------------

    ! ibuffer-update-format.  Port of ibuffer.el:ibuffer-update-format.
    subroutine ibuffer_update_format(status)
        type(status_type), intent(out) :: status
        integer :: k
        type(ibuffer_formats_type) :: f

        status = status_type()
        k = ibuffer_local_index(.true.)
        if (locals(k)%current_format < 0) locals(k)%current_format = 0
        f = formats_of()
        if (f%count() == 0) call set_error(status, FM_GENERIC_ERROR, "Ibuffer error: no formats!")
    end subroutine ibuffer_update_format

    !> Switch the current display format.  Port of
    !> ibuffer.el:ibuffer-switch-format.
    recursive subroutine ibuffer_switch_format(status)
        type(status_type), intent(out) :: status
        type(ibuffer_formats_type) :: f
        integer :: k

        call assert_ibuffer_mode(status)
        if (.not. status%ok()) return
        f = formats_of()

        if (f%count() == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Ibuffer error: No formats!")
            return
        end if

        k = ibuffer_local_index(.true.)

        if (locals(k)%current_format >= f%count() - 1) then
            locals(k)%current_format = 0
        else
            locals(k)%current_format = locals(k)%current_format + 1
        end if

        call ibuffer_update_format(status)
        if (status%ok()) call ibuffer_redisplay(status, .true.)
    end subroutine ibuffer_switch_format

    ! ibuffer--format-title.  Port of ibuffer.el:ibuffer--format-title.
    function format_title(spec, status) result(title)
        type(ibuffer_column_spec_type), intent(in) :: spec
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: title
        integer :: column, min, max, l

        title = ""

        if (len_trim(spec%name) == 0) then
            status = status_type()
            title = spec%text(1:spec%text_len)
            return
        end if

        call expand_format_entry(spec, column, min, max, status)
        if (.not. status%ok()) return
        ! Ignore negative MIN, since the titles are left-aligned.
        if (min < 0) min = -min
        title = trim(ibuffer_columns(column)%title)
        if (len(title) == 0) title = " "
        l = len(title)
        if (l < min) title = ibuffer_format_column(title, min - l, spec%align)
    end function format_title

    ! ibuffer--format-summary.  Port of ibuffer.el:ibuffer--format-summary.
    function format_summary(spec, status) result(summary)
        type(ibuffer_column_spec_type), intent(in) :: spec
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: summary
        character(len=:), allocatable :: title
        integer :: column, min, max, l

        summary = ""

        if (len_trim(spec%name) == 0) then
            status = status_type()
            summary = repeat(" ", spec%text_len)
            return
        end if

        call expand_format_entry(spec, column, min, max, status)
        if (.not. status%ok()) return
        ! Ignore negative MIN, since the summaries are left-aligned.
        if (min < 0) min = -min

        if (associated(ibuffer_columns(column)%summarizer)) then
            if (.not. allocated(summaries)) call clear_summary_columns()
            ! (The summary list is pushed: the last line first.)
            call ibuffer_columns(column)%summarizer(reversed(summaries(column)%cells(1:summaries(column)%n)), summary)
        else
            title = trim(ibuffer_columns(column)%title)
            if (len(title) == 0) title = " "
            summary = repeat(" ", len(title))
        end if

        l = len(summary)
        if (l < min) summary = ibuffer_format_column(summary, min - l, spec%align)
    end function format_summary

    ! Not a port.
    function reversed(cells) result(r)
        type(ibuffer_cell_type), intent(in) :: cells(:)
        type(ibuffer_cell_type), allocatable :: r(:)
        integer :: k, n

        n = size(cells)
        allocate (r(n))

        do k = 1, n
            r(k) = cells(n - k + 1)
        end do
    end function reversed

    ! ibuffer-update-title-and-summary.  Port of
    ! ibuffer.el:ibuffer-update-title-and-summary.
    recursive subroutine update_title_and_summary(entries, status)
        type(ibuffer_column_spec_type), intent(in) :: entries(:)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: line, underline, piece
        integer(int64) :: opos, start, next, prev
        type(status_type) :: st
        logical :: found
        integer :: k

        call assert_ibuffer_mode(status)
        if (.not. status%ok()) return

        ! Insert the title names.
        if (ibuffer_use_header_line == IBUFFER_HEADER_LINE_TITLE) then
            call buffers(cur)%header_line_format%clear()
            call buffers(cur)%header_line_format%add("")

            do k = 1, size(entries)
                piece = format_title(entries(k), status)
                if (.not. status%ok()) return
                call buffers(cur)%header_line_format%add(piece)
            end do
        else
            if (has_prop(point_min(), "ibuffer_title")) then
                call next_single_property_change(point_min(), sym("ibuffer_title"), next, found, st)
                if (.not. found) next = point_max()
                call delete_region(point_min(), next, st)
            end if

            call goto_char(point_min())
            start = point()
            opos = point()
            line = ""

            do k = 1, size(entries)
                piece = format_title(entries(k), status)
                if (.not. status%ok()) return
                line = line//piece
            end do

            call insert(line, status)
            if (.not. status%ok()) return
            call put_text_property(opos, point(), sym("ibuffer_title_header"), t_value(), st)
            call insert(achar(10), status)
            ! Add the underlines
            underline = line

            do k = 1, len(underline)
                if (underline(k:k) /= " " .and. underline(k:k) /= achar(10)) then
                    underline(k:k) = "-"
                else
                    underline(k:k) = " "
                end if
            end do

            call insert(underline//achar(10), status)
            call put_text_property(start, point(), sym("ibuffer_title"), t_value(), st)
            call put_text_property(start, point(), sym("font_lock_face"), sym(ibuffer_title_face), st)
        end if

        ! Now, insert the summary columns.
        call goto_char(point_max())

        if (point_max() > point_min()) then
            if (has_prop(point_max() - 1, "ibuffer_summary")) then
                call previous_single_property_change(point_max(), sym("ibuffer_summary"), prev, found, st)
                if (.not. found) prev = point_min()
                call delete_region(prev, point_max(), st)
            end if
        end if

        if (ibuffer_display_summary) then
            start = point()
            line = achar(10)

            do k = 1, size(entries)
                piece = format_summary(entries(k), status)
                if (.not. status%ok()) return
                line = line//piece
            end do

            call insert(line, status)
            call put_text_property(start, point(), sym("ibuffer_summary"), t_value(), st)
        end if
    end subroutine update_title_and_summary

    !> Redisplay the current list of buffers.
    !> This does not show new buffers; use `ibuffer_update' for that.
    !>
    !> If optional arg SILENT is true, do not display progress messages.
    !> Port of ibuffer.el:ibuffer-redisplay.
    recursive subroutine ibuffer_redisplay(status, silent)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: silent
        type(ibuffer_state_type), allocatable :: blist(:)
        logical :: quiet
        integer :: k

        quiet = .false.
        if (present(silent)) quiet = silent
        call ibuffer_forward_line(0_int64)
        if (.not. quiet) call message("Redisplaying current buffer list...")
        call ibuffer_current_state_list(blist, status)
        if (.not. status%ok()) return
        k = ibuffer_local_index(.true.)

        if (size(blist) == 0 .and. (size(locals(k)%qualifiers) > 0 .or. size(locals(k)%hidden_groups) > 0)) then
            call message("No buffers! (note: filtering in effect)")
        end if

        call ibuffer_redisplay_engine(blist, status)
        if (.not. status%ok()) return
        if (.not. quiet) call message("Redisplaying current buffer list...done")
        call ibuffer_forward_line(0_int64)
    end subroutine ibuffer_redisplay

    recursive subroutine cmd_ibuffer_redisplay(status)
        type(status_type), intent(out) :: status

        call ibuffer_redisplay(status)
    end subroutine cmd_ibuffer_redisplay

    !> Regenerate the list of all buffers.
    !>
    !> ARG true means to toggle whether buffers that match
    !> `ibuffer_maybe_show_predicates' should be displayed.
    !>
    !> If optional arg SILENT is true, do not display progress messages.
    !> Port of ibuffer.el:ibuffer-update.
    recursive subroutine ibuffer_update(status, arg, silent)
        type(status_type), intent(out) :: status
        logical, intent(in) :: arg
        logical, intent(in), optional :: silent
        type(handle_type), allocatable :: bufs(:)
        type(ibuffer_state_type), allocatable :: marks(:), blist(:)
        type(handle_type) :: last
        logical :: quiet
        integer :: k

        quiet = .false.
        if (present(silent)) quiet = silent
        k = ibuffer_local_index(.true.)
        if (arg) locals(k)%display_maybe_show_predicates = .not. locals(k)%display_maybe_show_predicates
        call ibuffer_forward_line(0_int64)
        bufs = buffer_list()
        last = null_handle()

        if (size(bufs) >= 2) then
            last = bufs(2)

            if (ibuffer_always_show_last_buffer == IBUFFER_SHOW_LAST_NOMINI .and. is_minibuffer(bufs(2))) then
                last = null_handle()
                if (size(bufs) >= 3) last = bufs(3)
            end if
        end if

        call current_buffers_with_marks(bufs, marks, status)
        if (.not. status%ok()) return
        k = ibuffer_local_index(.true.)
        call filter_buffers(current_buffer(), last, marks, locals(k)%display_maybe_show_predicates, blist, status)
        if (.not. status%ok()) return
        k = ibuffer_local_index(.true.)

        if (size(blist) == 0 .and. size(locals(k)%qualifiers) > 0) then
            call message("No buffers! (note: filtering in effect)")
        end if

        if (.not. quiet) call message("Updating buffer list...")
        call ibuffer_redisplay_engine(blist, status)
        if (.not. status%ok()) return
        if (.not. quiet) call message("Updating buffer list...done")
        k = ibuffer_local_index(.true.)

        if (locals(k)%shrink == IBUFFER_SHRINK_ONEWINDOW) then
            call ibuffer_shrink_to_fit(.true.)
        else if (locals(k)%shrink /= IBUFFER_SHRINK_NIL) then
            call ibuffer_shrink_to_fit()
        end if

        call ibuffer_forward_line(0_int64)

        ! I tried to update this automatically from the mode-line-process
        ! format, but changing nil-ness of header-line-format while computing
        ! mode-line-format is asking a bit too much it seems.  --Stef
        if (ibuffer_use_header_line /= IBUFFER_HEADER_LINE_TITLE) then
            call buffers(cur)%header_line_format%clear()
            k = ibuffer_local_index(.true.)

            if (ibuffer_use_header_line /= IBUFFER_HEADER_LINE_NIL .and. size(locals(k)%qualifiers) > 0) then
                call buffers(cur)%header_line_format%add("")
            end if
        end if

        call update_mode_line_process()
    end subroutine ibuffer_update

    recursive subroutine cmd_ibuffer_update(status)
        type(status_type), intent(out) :: status

        call ibuffer_update(status,.not. nilp(current_prefix_arg))
    end subroutine cmd_ibuffer_update

    ! Not a port.
    recursive subroutine revert_function(ignore_auto, noconfirm, status)
        logical, intent(in) :: ignore_auto, noconfirm
        type(status_type), intent(out) :: status

        call ibuffer_update(status, ignore_auto, noconfirm)
    end subroutine revert_function

    !> The mode line's state info next to the mode name: " by " and the
    !> sorting mode, " [rev]", " (Auto)" and the filters unless they're
    !> already in the header line (upstream evaluated by the mode line);
    !> the header line showing the filters gets the same, after the mode
    !> name (ibuffer-header-line-format).  Not a port.
    recursive subroutine update_mode_line_process()
        type(mode_line_part_type), allocatable :: parts(:)

        if (ibuffer_use_header_line /= IBUFFER_HEADER_LINE_TITLE .and. &
            allocated(buffers(cur)%header_line_format%segments)) then
            ! Display the part that won't be in the mode-line.
            call buffers(cur)%header_line_format%clear()
            call buffers(cur)%header_line_format%add("")
            call buffers(cur)%header_line_format%add(mode_name_of(current_buffer()))
            call buffers(cur)%header_line_format%add(state_text()//qualifiers_text())
        end if

        allocate (parts(1))
        parts(1)%text = state_text()

        ! Only list the filters if they're not already in the header-line.
        if (.not. allocated(buffers(cur)%header_line_format%segments)) then
            parts(1)%text = parts(1)%text//qualifiers_text()
        end if

        parts(1)%face = ""
        call set_mode_line_process(parts)
    end subroutine update_mode_line_process

    ! " by " and the sorting mode, " [rev]" and " (Auto)".  Not a port.
    function state_text() result(text)
        character(len=:), allocatable :: text
        integer :: k

        k = ibuffer_local_index(.true.)

        if (len(locals(k)%sorting_mode) > 0) then
            text = " by "//locals(k)%sorting_mode
        else
            text = " by view time"
        end if

        if (locals(k)%sorting_reversep) text = text//" [rev]"
        if (minor_modes%ibuffer_auto_mode%enabled()) text = text//" (Auto)"
    end function state_text

    ! (mapconcat 'ibuffer-format-qualifier ibuffer-filtering-qualifiers "")
    ! Not a port.
    recursive function qualifiers_text() result(text)
        character(len=:), allocatable :: text
        type(ibuffer_filter_type), allocatable :: filters(:)
        integer :: j

        filters = ibuffer_filtering_qualifiers()
        text = ""

        do j = 1, size(filters)
            text = text//ibuffer_format_qualifier(filters(j))
        end do
    end function qualifiers_text

    ! ibuffer-sort-bufferlist: sort BMARKLIST in place by the current
    ! sorting mode (a stable sort, as `sort').  Port of
    ! ibuffer.el:ibuffer-sort-bufferlist.
    recursive subroutine sort_bufferlist(bmarklist)
        type(ibuffer_state_type), intent(inout) :: bmarklist(:)
        type(ibuffer_state_type), allocatable :: tmp(:)
        procedure(ibuffer_sorter_proc), pointer :: less
        integer :: k, n

        k = ibuffer_local_index(.true.)
        less => null()

        do n = 1, size(ibuffer_sorting_functions)
            if (ibuffer_sorting_functions(n)%name == locals(k)%sorting_mode) then
                less => ibuffer_sorting_functions(n)%less
                exit
            end if
        end do

        if (associated(less) .and. size(bmarklist) > 1) then
            allocate (tmp(size(bmarklist)))
            call merge_sort(bmarklist, tmp, 1, size(bmarklist))
        end if

        ! perhaps reverse the sorted buffer list
        if (locals(k)%sorting_reversep) bmarklist = bmarklist(size(bmarklist):1:-1)

    contains

        recursive subroutine merge_sort(a, t, lo, hi)
            type(ibuffer_state_type), intent(inout) :: a(:), t(:)
            integer, intent(in) :: lo, hi
            integer :: mid, i, j, o

            if (hi <= lo) return
            mid = (lo + hi)/2
            call merge_sort(a, t, lo, mid)
            call merge_sort(a, t, mid + 1, hi)
            i = lo
            j = mid + 1
            o = lo

            do while (i <= mid .and. j <= hi)
                if (less(a(j)%buffer, a(i)%buffer)) then
                    t(o) = a(j)
                    j = j + 1
                else
                    t(o) = a(i)
                    i = i + 1
                end if

                o = o + 1
            end do

            do while (i <= mid)
                t(o) = a(i)
                i = i + 1
                o = o + 1
            end do

            do while (j <= hi)
                t(o) = a(j)
                j = j + 1
                o = o + 1
            end do

            a(lo:hi) = t(lo:hi)
        end subroutine merge_sort
    end subroutine sort_bufferlist

    ! ibuffer-insert-filter-group: the group NAME, shown as DISPLAY_NAME,
    ! with the buffers of BMARKLIST.  Port of
    ! ibuffer.el:ibuffer-insert-filter-group.
    recursive subroutine insert_filter_group(name, display_name, entries, bmarklist, status)
        character(len=*), intent(in) :: name, display_name
        type(ibuffer_column_spec_type), intent(in) :: entries(:)
        type(ibuffer_state_type), intent(in) :: bmarklist(:)
        type(status_type), intent(out) :: status
        integer(int64) :: start
        type(status_type) :: st
        integer :: k

        start = point()
        call insert("[ "//display_name//" ]", status)
        if (.not. status%ok()) return
        call put_text_property(start, point(), sym("ibuffer_filter_group_name"), make_string(name), st)
        call put_text_property(start, point(), sym("font_lock_face"), sym(ibuffer_filter_group_name_face), st)
        call put_text_property(start, point(), sym("keymap"), keymap_value(ibuffer_mode_filter_group_map), st)
        call put_text_property(start, point(), sym("mouse_face"), sym("highlight"), st)
        call insert(achar(10), status)

        if (size(bmarklist) > 0) then
            start = point()

            do k = 1, size(bmarklist)
                call ibuffer_insert_buffer_line(bmarklist(k)%buffer, bmarklist(k)%mark, entries, status)
                if (.not. status%ok()) return
            end do

            call put_text_property(start, point(), sym("ibuffer_filter_group"), make_string(name), st)
        end if
    end subroutine insert_filter_group

    ! Not a port.
    function keymap_value(map) result(v)
        use lisp_fm, only: make_keymap_ref
        type(handle_type), intent(in) :: map
        type(value_type) :: v

        v = make_keymap_ref(map)
    end function keymap_value

    ! Port of ibuffer.el:ibuffer-redisplay-engine.
    recursive subroutine ibuffer_redisplay_engine(bmarklist, status)
        type(ibuffer_state_type), intent(in) :: bmarklist(:)
        type(status_type), intent(out) :: status
        type(ibuffer_column_spec_type), allocatable :: entries(:)
        type(ibuffer_state_type), allocatable :: members(:)
        character(len=256), allocatable :: names(:)
        integer, allocatable :: group_of(:)
        integer(int64) :: orig, shortage
        integer :: g, k, n, l
        logical :: disabled
        type(status_type) :: st

        call assert_ibuffer_mode(status)
        if (.not. status%ok()) return
        call current_format_entries(entries, status)
        if (.not. status%ok()) return
        orig = count_lines(point_min(), point())
        call ibuffer_generate_filter_groups(bmarklist, names, group_of, status)
        if (.not. status%ok()) return
        call clear_summary_columns()
        buffers(cur)%read_only = .false.
        call erase_buffer(st)
        call ibuffer_update_format(status)

        if (status%ok()) then
            allocate (members(size(bmarklist)))

            ! (The groups come in reverse: (nreverse bgroups).)
            do g = size(names), 1, -1
                n = 0

                do k = 1, size(bmarklist)
                    if (group_of(k) /= g) cycle
                    n = n + 1
                    members(n) = bmarklist(k)
                end do

                l = ibuffer_local_index(.true.)
                disabled = any(locals(l)%hidden_groups == names(g))
                if (n == 0 .and. .not. disabled .and. .not. ibuffer_show_empty_filter_groups) cycle

                if (disabled) then
                    call insert_filter_group(trim(names(g)), trim(names(g))//" ...", entries, members(1:0), status)
                else
                    call sort_bufferlist(members(1:n))
                    call insert_filter_group(trim(names(g)), trim(names(g)), entries, members(1:n), status)
                end if

                if (.not. status%ok()) exit
            end do

            if (status%ok()) call update_title_and_summary(entries, status)
        end if

        buffers(cur)%read_only = .true.
        k = ibuffer_local_index(.true.)
        call restore_buffer_modified(locals(k)%did_modification)
        locals(k)%did_modification = .false.
        call goto_char(point_min())
        shortage = forward_line(orig)
        if (.not. st%ok()) continue
    end subroutine ibuffer_redisplay_engine

    ! ------------------------------------------------------------------
    ! Entry points
    ! ------------------------------------------------------------------

    !> Display a list of buffers, in another window.
    !> If optional argument FILES_ONLY is true, then add a filter for
    !> buffers which are visiting a file.  Port of
    !> ibuffer.el:ibuffer-list-buffers.
    recursive subroutine ibuffer_list_buffers(status, files_only)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: files_only
        type(ibuffer_filter_type) :: files(1)
        logical :: only

        only = .false.
        if (present(files_only)) only = files_only

        if (only) then
            files(1) = ibuffer_filename(".*")
            call ibuffer(status, other_window_p=.true., qualifiers=files, noselect=.true.)
        else
            call ibuffer(status, other_window_p=.true., noselect=.true.)
        end if
    end subroutine ibuffer_list_buffers

    recursive subroutine cmd_ibuffer_list_buffers(status)
        type(status_type), intent(out) :: status

        call ibuffer_list_buffers(status,.not. nilp(current_prefix_arg))
    end subroutine cmd_ibuffer_list_buffers

    !> Like `ibuffer', but displayed in another window by default.
    !> If optional argument FILES_ONLY is true, then add a filter for
    !> buffers which are visiting a file.  Port of
    !> ibuffer.el:ibuffer-other-window.
    recursive subroutine ibuffer_other_window(status, files_only)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: files_only
        type(ibuffer_filter_type) :: files(1)
        logical :: only

        only = .false.
        if (present(files_only)) only = files_only

        if (only) then
            files(1) = ibuffer_filename(".*")
            call ibuffer(status, other_window_p=.true., qualifiers=files)
        else
            call ibuffer(status, other_window_p=.true.)
        end if
    end subroutine ibuffer_other_window

    recursive subroutine cmd_ibuffer_other_window(status)
        type(status_type), intent(out) :: status

        call ibuffer_other_window(status,.not. nilp(current_prefix_arg))
    end subroutine cmd_ibuffer_other_window

    !> Begin using Ibuffer to edit a list of buffers.
    !> Type \<ibuffer_mode_map>\[describe_mode] after entering ibuffer for
    !> more information.
    !>
    !> All arguments are optional.
    !> OTHER_WINDOW_P says to use another window.
    !> NAME specifies the name of the buffer (defaults to "*Ibuffer*").
    !> QUALIFIERS is an initial set of filtering qualifiers to use;
    !>   see `ibuffer_filtering_qualifiers'.
    !> NOSELECT means don't select the Ibuffer buffer.
    !> SHRINK means shrink the buffer to minimal size.  The special
    !>   value IBUFFER_SHRINK_ONEWINDOW means always use another window.
    !> FILTER_GROUPS is an initial set of filtering groups to use;
    !>   see `ibuffer_filter_groups'.
    !> FORMATS is the value to use for `ibuffer_formats'.
    !>   If specified, then the variable `ibuffer_formats' will have
    !>   that value locally in this buffer.  Port of ibuffer.el:ibuffer.
    recursive subroutine ibuffer(status, other_window_p, name, qualifiers, noselect, shrink, filter_groups, formats)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: other_window_p, noselect
        character(len=*), intent(in), optional :: name
        type(ibuffer_filter_type), intent(in), optional :: qualifiers(:)
        integer, intent(in), optional :: shrink
        type(ibuffer_filter_groups_type), intent(in), optional :: filter_groups
        type(ibuffer_formats_type), intent(in), optional :: formats
        type(handle_type) :: buf, window, old, old_window
        type(status_type) :: st
        logical :: other, nosel, done
        integer :: k

        other = .false.
        if (present(other_window_p)) other = other_window_p
        if (ibuffer_use_other_window) other = .true.
        nosel = .false.
        if (present(noselect)) nosel = noselect

        if (present(name)) then
            call get_buffer_create(name, buf, status)
        else
            call get_buffer_create("*Ibuffer*", buf, status)
        end if

        if (.not. status%ok()) return

        if (other) then
            done = .false.

            if (nosel) then
                call display_buffer(buf, window, status, table=display_action_table_type(inhibit_same_window=.true.))
                done = status%ok() .and. window%index /= 0
            end if

            if (.not. done) call pop_to_buffer(buf, status, table=display_action_table_type(inhibit_same_window=.true.))
        else if (nosel) then
            call display_buffer(buf, window, status)
        else
            call switch_to_buffer(buffers(buf%index)%name, status)
        end if

        if (.not. status%ok()) return
        old = current_buffer()
        old_window = selected_window()
        call set_buffer(buf, status)
        if (.not. status%ok()) return
        ! We switch to the buffer's window in order to be able to modify
        ! the value of point.
        window = get_buffer_window(buf)
        if (window%index /= 0) call select_window(window, st, norecord=.true.)
        call set_buffer(buf, st)
        if (.not. is_derived_mode("ibuffer_mode")) call ibuffer_mode(status)

        if (status%ok()) then
            k = ibuffer_local_index(.true.)

            if (present(shrink)) then
                if (shrink /= IBUFFER_SHRINK_NIL) locals(k)%shrink = shrink
            end if

            if (present(qualifiers)) then
                if (size(qualifiers) > 0) call ibuffer_set_filtering_qualifiers(qualifiers)
            end if

            if (present(filter_groups)) then
                if (filter_groups%count() > 0) call ibuffer_set_filter_groups(filter_groups)
            end if

            k = ibuffer_local_index(.true.)

            if (present(formats)) then
                locals(k)%has_formats = .true.
                locals(k)%formats = formats
            end if

            call ibuffer_update(status, .false.)
        end if

        if (status%ok()) then
            ! Skip the group name by default.
            call ibuffer_forward_line(0_int64, .true.)
            buffers(cur)%read_only = .false.
            call ibuffer_hook%run(status)
            buffers(cur)%read_only = .true.
        end if

        if (status%ok() .and. .not. ibuffer_expert) then
            call message(substitute_command_keys("Commands: \[ibuffer_mark_forward], " &
                                                 //"\[ibuffer_unmark_forward], " &
                                                 //"\[ibuffer_toggle_marks], " &
                                                 //"\[ibuffer_visit_buffer], " &
                                                 //"\[ibuffer_update], " &
                                                 //"\[ibuffer_do_kill_lines], " &
                                                 //"\[ibuffer_do_save], " &
                                                 //"\[ibuffer_do_delete], " &
                                                 //"\[ibuffer_do_query_replace]; " &
                                                 //"\[quit_window] to quit; " &
                                                 //"\[describe_mode] for help"))
        end if

        ! (save-selected-window, with-current-buffer)
        if (is_window_live(old_window)) call select_window(old_window, st, norecord=.true.)
        if (is_buffer_live(old)) call set_buffer(old, st)
    end subroutine ibuffer

    recursive subroutine cmd_ibuffer(status)
        type(status_type), intent(out) :: status

        call ibuffer(status, other_window_p=.not. nilp(current_prefix_arg))
    end subroutine cmd_ibuffer

    !> Call Ibuffer and set point at the line listing the current buffer.
    !> If optional arg OTHER_WINDOW is true, then use another window.
    !> Port of ibuffer.el:ibuffer-jump.
    recursive subroutine ibuffer_jump(status, other_window)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: other_window
        character(len=:), allocatable :: name
        type(status_type) :: st
        logical :: other

        other = .false.
        if (present(other_window)) other = other_window
        name = buffers(cur)%name
        call ibuffer(status, other_window_p=other)
        if (.not. status%ok()) return
        call jump_to_buffer_by_name(name, st)
    end subroutine ibuffer_jump

    recursive subroutine cmd_ibuffer_jump(status)
        type(status_type), intent(out) :: status

        call ibuffer_jump(status,.not. nilp(current_prefix_arg))
    end subroutine cmd_ibuffer_jump

    !> Move point to the line of the buffer NAME in the current Ibuffer
    !> buffer (the part of `ibuffer_jump_to_buffer' that finds it).  Port of
    !> ibuf-ext.el:ibuffer-jump-to-buffer, in part.
    recursive subroutine jump_to_buffer_by_name(name, status)
        character(len=*), intent(in) :: name
        type(status_type), intent(out) :: status
        type(ibuffer_state_type), allocatable :: states(:)
        integer :: k

        call ibuffer_current_state_list(states, status, pos=.true.)
        if (.not. status%ok()) return

        do k = 1, size(states)
            if (buffers(states(k)%buffer%index)%name == name) then
                call goto_char(states(k)%point)
                return
            end if
        end do

        call set_error(status, FM_GENERIC_ERROR, "No buffer with name "//name)
    end subroutine jump_to_buffer_by_name

    !> A major mode for viewing a list of buffers.
    !> In Ibuffer, you can conveniently perform many operations on the
    !> currently open buffers, in addition to filtering your view to a
    !> particular subset of them, and sorting by various criteria.
    !>
    !> Operations on marked buffers (see "Marking commands" below
    !>  for how to mark buffers):
    !> \<ibuffer_mode_map>
    !>   \[ibuffer_do_save] - Save the marked buffers.
    !>   \[ibuffer_do_view] - View the marked buffers in the selected frame.
    !>   \[ibuffer_do_revert] - Revert the marked buffers.
    !>   \[ibuffer_do_toggle_read_only] - Toggle read-only state of marked buffers.
    !>   \[ibuffer_do_delete] - Kill the marked buffers.
    !>   \[ibuffer_do_replace_regexp] - Replace by regexp in each of the marked
    !>         buffers.
    !>   \[ibuffer_do_query_replace] - Query replace in each of the marked buffers.
    !>   \[ibuffer_do_query_replace_regexp] - As above, with a regular expression.
    !>   \[ibuffer_do_occur] - List lines in all marked buffers which match
    !>         a given regexp (like the function `occur').
    !>   \[ibuffer_do_shell_command_pipe] - Pipe the contents of the marked
    !>         buffers to a shell command.
    !>   \[ibuffer_do_shell_command_pipe_replace] - Replace the contents of the marked
    !>         buffers with the output of a shell command.
    !>   \[ibuffer_do_shell_command_file] - Run a shell command with the
    !>         buffer's file as an argument.
    !>   \[ibuffer_do_kill_lines] - Remove the marked lines from the *Ibuffer* buffer,
    !>         but don't kill the associated buffer.
    !>   \[ibuffer_do_kill_on_deletion_marks] - Kill all buffers marked for deletion.
    !>
    !> Marking commands:
    !>
    !>   \[ibuffer_mark_forward] - Mark the buffer at point.
    !>   \[ibuffer_toggle_marks] - Unmark all currently marked buffers, and mark
    !>         all unmarked buffers.
    !>   \[ibuffer_change_marks] - Change the mark used on marked buffers.
    !>   \[ibuffer_unmark_forward] - Unmark the buffer at point.
    !>   \[ibuffer_unmark_backward] - Unmark the previous buffer.
    !>   \[ibuffer_unmark_all] - Unmark buffers marked with MARK.
    !>   \[ibuffer_unmark_all_marks] - Unmark all marked buffers.
    !>   \[ibuffer_mark_by_mode] - Mark buffers by major mode.
    !>   \[ibuffer_mark_unsaved_buffers] - Mark all "unsaved" buffers.
    !>         This means that the buffer is modified, and has an associated file.
    !>   \[ibuffer_mark_modified_buffers] - Mark all modified buffers,
    !>         regardless of whether they have an associated file.
    !>   \[ibuffer_mark_special_buffers] - Mark all buffers whose name begins and
    !>         ends with `*'.
    !>   \[ibuffer_mark_dissociated_buffers] - Mark all buffers which have
    !>         an associated file, but that file doesn't currently exist.
    !>   \[ibuffer_mark_read_only_buffers] - Mark all read-only buffers.
    !>   \[ibuffer_mark_dired_buffers] - Mark buffers in `dired_mode'.
    !>   \[ibuffer_mark_help_buffers] - Mark buffers in `help_mode', `apropos_mode', etc.
    !>   \[ibuffer_mark_old_buffers] - Mark buffers older than `ibuffer_old_time'.
    !>   \[ibuffer_mark_for_delete] - Mark the buffer at point for deletion.
    !>   \[ibuffer_mark_by_name_regexp] - Mark buffers by their name, using a regexp.
    !>   \[ibuffer_mark_by_mode_regexp] - Mark buffers by their major mode, using a regexp.
    !>   \[ibuffer_mark_by_file_name_regexp] - Mark buffers by their filename, using a regexp.
    !>   \[ibuffer_mark_by_content_regexp] - Mark buffers by their content, using a regexp.
    !>
    !> Filtering commands:
    !>
    !>   \[ibuffer_filter_chosen_by_completion] - Select and apply filter chosen by completion.
    !>   \[ibuffer_filter_by_mode] - Add a filter by any major mode.
    !>   \[ibuffer_filter_by_used_mode] - Add a filter by a major mode now in use.
    !>   \[ibuffer_filter_by_derived_mode] - Add a filter by derived mode.
    !>   \[ibuffer_filter_by_name] - Add a filter by buffer name.
    !>   \[ibuffer_filter_by_content] - Add a filter by buffer content.
    !>   \[ibuffer_filter_by_basename] - Add a filter by basename.
    !>   \[ibuffer_filter_by_directory] - Add a filter by directory name.
    !>   \[ibuffer_filter_by_filename] - Add a filter by filename.
    !>   \[ibuffer_filter_by_file_extension] - Add a filter by file extension.
    !>   \[ibuffer_filter_by_modified] - Add a filter by modified buffers.
    !>   \[ibuffer_filter_by_size_gt] - Add a filter by buffer size.
    !>   \[ibuffer_filter_by_size_lt] - Add a filter by buffer size.
    !>   \[ibuffer_filter_by_starred_name] - Add a filter by special buffers.
    !>   \[ibuffer_filter_by_visiting_file] - Add a filter by buffers visiting files.
    !>   \[ibuffer_save_filters] - Save the current filters with a name.
    !>   \[ibuffer_switch_to_saved_filters] - Switch to previously saved filters.
    !>   \[ibuffer_add_saved_filters] - Add saved filters to current filters.
    !>   \[ibuffer_and_filter] - Replace the top two filters with their logical AND.
    !>   \[ibuffer_or_filter] - Replace the top two filters with their logical OR.
    !>   \[ibuffer_pop_filter] - Remove the top filter.
    !>   \[ibuffer_negate_filter] - Invert the logical sense of the top filter.
    !>   \[ibuffer_decompose_filter] - Break down the topmost filter.
    !>   \[ibuffer_filter_disable] - Remove all filtering currently in effect.
    !>
    !> Filter group commands:
    !>
    !>   \[ibuffer_filters_to_filter_group] - Create filter group from filters.
    !>   \[ibuffer_pop_filter_group] - Remove top filter group.
    !>   \[ibuffer_forward_filter_group] - Move to the next filter group.
    !>   \[ibuffer_backward_filter_group] - Move to the previous filter group.
    !>   \[ibuffer_clear_filter_groups] - Remove all active filter groups.
    !>   \[ibuffer_save_filter_groups] - Save the current groups with a name.
    !>   \[ibuffer_switch_to_saved_filter_groups] - Restore previously saved groups.
    !>   \[ibuffer_delete_saved_filter_groups] - Delete previously saved groups.
    !>
    !> Sorting commands:
    !>
    !>   \[ibuffer_toggle_sorting_mode] - Rotate between the various sorting modes.
    !>   \[ibuffer_invert_sorting] - Reverse the current sorting order.
    !>   \[ibuffer_do_sort_by_alphabetic] - Sort the buffers lexicographically.
    !>   \[ibuffer_do_sort_by_filename_process] - Sort the buffers by the file name.
    !>   \[ibuffer_do_sort_by_recency] - Sort the buffers by last viewing time.
    !>   \[ibuffer_do_sort_by_size] - Sort the buffers by size.
    !>   \[ibuffer_do_sort_by_major_mode] - Sort the buffers by major mode.
    !>
    !> Other commands:
    !>
    !>   \[ibuffer_update] - Regenerate the list of all buffers.
    !>         Prefix arg means to toggle whether buffers that match
    !>         `ibuffer_maybe_show_predicates' should be displayed.
    !>   \[ibuffer_auto_mode] - Toggle automatic updates.
    !>
    !>   \[ibuffer_switch_format] - Change the current display format.
    !>   \[forward_line] - Move point to the next line.
    !>   \[previous_line] - Move point to the previous line.
    !>   \[describe_mode] - This help.
    !>   \[ibuffer_diff_with_file] - View the differences between this buffer
    !>         and its associated file.
    !>   \[ibuffer_visit_buffer] - View the buffer on this line.
    !>   \[ibuffer_visit_buffer_other_window] - As above, but in another window.
    !>   \[ibuffer_visit_buffer_other_window_noselect] - As both above, but don't select
    !>         the new window.
    !>   \[ibuffer_bury_buffer] - Bury (not kill!) the buffer on this line.
    !>
    !> ** Information on Filtering:
    !>
    !> You can filter your Ibuffer view via different criteria.  Each Ibuffer
    !> buffer has its own stack of active filters.  For example, suppose you
    !> are working on an Emacs Lisp project.  You can create an Ibuffer
    !> buffer displaying only `emacs_lisp_mode' buffers via
    !> `\[ibuffer_filter_by_mode] emacs_lisp_mode RET'.  In this case, there
    !> is just one entry on the filtering stack.
    !>
    !> You can also combine filters.  The various filtering commands push a
    !> new filter onto the stack, and the filters combine to show just
    !> buffers which satisfy ALL criteria on the stack.  For example, suppose
    !> you only want to see buffers in `emacs_lisp_mode', whose names begin
    !> with "gnus".  You can accomplish this via:
    !>
    !>   \[ibuffer_filter_by_mode] emacs_lisp_mode RET
    !>   \[ibuffer_filter_by_name] ^gnus RET
    !>
    !> Additionally, you can OR the top two filters together with
    !> \[ibuffer_or_filters].  To see all buffers in either
    !> `emacs_lisp_mode' or `lisp_interaction_mode', type:
    !>
    !>   \[ibuffer_filter_by_mode] emacs_lisp_mode RET
    !>   \[ibuffer_filter_by_mode] lisp_interaction_mode RET
    !>   \[ibuffer_or_filters]
    !>
    !> Filters can also be saved and restored using mnemonic names: see the
    !> functions `ibuffer_save_filters' and `ibuffer_switch_to_saved_filters'.
    !>
    !> To remove the top filter on the stack, use \[ibuffer_pop_filter], and
    !> to disable all filtering currently in effect, use
    !> \[ibuffer_filter_disable].
    !>
    !> ** Filter Groups:
    !>
    !> Once one has mastered filters, the next logical step up is "filter
    !> groups".  A filter group is basically a named group of buffers which
    !> match a filter, which are displayed together in an Ibuffer buffer.  To
    !> create a filter group, simply use the regular functions to create a
    !> filter, and then type \[ibuffer_filters_to_filter_group].
    !>
    !> A quick example will make things clearer.  Suppose that one wants to
    !> group all of one's Emacs Lisp buffers together.  To do this, type:
    !>
    !>   \[ibuffer_filter_by_mode] emacs_lisp_mode RET
    !>   \[ibuffer_filters_to_filter_group] emacs lisp buffers RET
    !>
    !> You may, of course, name the group whatever you want; it doesn't have
    !> to be "emacs lisp buffers".  Filter groups may be composed of any
    !> arbitrary combination of filters.
    !>
    !> Just like filters themselves, filter groups act as a stack.  Buffers
    !> will not be displayed multiple times if they would be included in
    !> multiple filter groups; instead, the first filter group is used.  The
    !> filter groups are displayed in this order of precedence.
    !>
    !> You may rearrange filter groups by using the usual pair
    !> \[ibuffer_kill_line] and \[ibuffer_yank].  Yanked groups
    !> will be inserted before the group at point.
    !>
    !> Port of ibuffer.el:ibuffer-mode.
    recursive subroutine ibuffer_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved
        integer :: k
        type(status_type) :: st

        call major_modes%ibuffer_mode%begin(saved, status)
        if (.not. status%ok()) return
        buffers(cur)%read_only = .true.
        buffers(cur)%undo_enabled = .false.
        call revert_buffer_function%set_local(revert_function)
        k = ibuffer_local_index(.true.)
        locals(k) = local_type(serial=buffers(cur)%serial)
        call init_local(k)
        locals(k)%sorting_mode = ibuffer_default_sorting_mode
        locals(k)%sorting_reversep = ibuffer_default_sorting_reversep
        if (ibuffer_default_shrink_to_minimum_size) locals(k)%shrink = IBUFFER_SHRINK_T
        locals(k)%display_maybe_show_predicates = ibuffer_default_display_maybe_show_predicates
        call ibuffer_update_format(st)
        call update_mode_line_process()
        if (allocated(ibuffer_default_directory)) buffers(cur)%directory = ibuffer_default_directory
        call change_major_mode_hook%add(defontify, local=.true.)
        call major_modes%ibuffer_mode%finish(saved, status)
    end subroutine ibuffer_mode

    ! Not a port.
    recursive subroutine defontify(status)
        use font_core_fm, only: font_lock_defontify
        type(status_type), intent(out) :: status

        call font_lock_defontify(status)
    end subroutine defontify

    ! ------------------------------------------------------------------
    ! ibuffer-save-marks
    ! ------------------------------------------------------------------

    !> Save the marked status of the buffers (the first half of
    !> ibuf-macs.el:ibuffer-save-marks; `ibuffer_save_marks_end' restores
    !> them).  Port of ibuf-macs.el:ibuffer-save-marks, in part.
    recursive subroutine ibuffer_save_marks_begin(status)
        type(status_type), intent(out) :: status

        call ibuffer_current_state_list(saved_marks, status)
    end subroutine ibuffer_save_marks_begin

    !> Restore the marks `ibuffer_save_marks_begin' saved.  Port of
    !> ibuf-macs.el:ibuffer-save-marks (its unwind forms).
    recursive subroutine ibuffer_save_marks_end(status)
        type(status_type), intent(out) :: status
        type(ibuffer_state_type), allocatable :: live(:)
        integer :: k, n

        status = status_type()
        if (.not. allocated(saved_marks)) return
        allocate (live(size(saved_marks)))
        n = 0

        ! Get rid of dead buffers
        do k = 1, size(saved_marks)
            if (.not. is_buffer_live(saved_marks(k)%buffer)) cycle
            n = n + 1
            live(n) = saved_marks(k)
        end do

        call ibuffer_redisplay_engine(live(1:n), status)
        if (status%ok()) call ibuffer_redisplay(status, .true.)
        deallocate (saved_marks)
    end subroutine ibuffer_save_marks_end

    ! ------------------------------------------------------------------
    ! Reading
    ! ------------------------------------------------------------------

    ! Read a character in the echo area after PROMPT (read-char): C-g
    ! quits, other non-characters are an error.  Not a port.
    subroutine read_char(prompt, c, status)
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
            call set_error(status, FM_QUIT)
        else if (.not. fixnump(ev)) then
            call set_error(status, FM_GENERIC_ERROR, "Non-character input-event")
        else if (xfixnum(ev) == 7) then
            call set_error(status, FM_QUIT)
        else if (xfixnum(ev) < 0 .or. xfixnum(ev) > int(z'3FFFFF', int64)) then
            call set_error(status, FM_GENERIC_ERROR, "Non-character input-event")
        else
            c = xfixnum(ev)
        end if
    end subroutine read_char

    ! ------------------------------------------------------------------
    ! Fontification forms (the defaults of `ibuffer_fontification_table')
    ! ------------------------------------------------------------------

    ! Not a port.
    recursive logical function form_read_only()
        form_read_only = buffers(cur)%read_only
    end function form_read_only

    !> Whether the current buffer visits a compressed file
    !> (`ibuffer_compressed_file_name_regexp'), a default form of
    !> `ibuffer_fontification_table'.  Port of ibuffer.el's fontification form.
    recursive logical function form_compressed()
        integer(int64) :: m
        type(status_type) :: st

        form_compressed = .false.
        if (.not. allocated(buffers(cur)%filename)) return
        if (len(buffers(cur)%filename) == 0) return
        m = string_match(ibuffer_compressed_file_name_regexp, buffers(cur)%filename, st)
        form_compressed = st%ok() .and. m > 0
    end function form_compressed

    ! Not a port.
    recursive logical function form_starred()
        form_starred = .false.
        if (len(buffers(cur)%name) > 0) form_starred = buffers(cur)%name(1:1) == "*"
    end function form_starred

    ! Not a port.
    recursive logical function form_help_mode()
        form_help_mode = .false.
        if (.not. allocated(buffers(cur)%major_mode)) return
        form_help_mode = any(ibuffer_help_buffer_modes == buffers(cur)%major_mode)
    end function form_help_mode

    ! Not a port.
    recursive logical function form_dired()
        form_dired = is_derived_mode("dired_mode")
    end function form_dired

    ! (and (boundp 'emacs-lock-mode) emacs-lock-mode): emacs-lock.el is
    ! not ported.  Not a port.
    recursive logical function form_locked()
        form_locked = .false.
    end function form_locked

    !> Define Ibuffer's settings, mode, keymaps, columns and commands.
    !> Not a port.
    subroutine syms_of_ibuffer()
        type(status_type) :: st
        type(handle_type) :: m

        call add_local_variables_killer(kill_ibuffer_locals)

        if (.not. allocated(ibuffer_formats%formats)) then
            call ibuffer_formats%add([ibuffer_column("mark"), ibuffer_column("modified"), ibuffer_column("read_only"), &
                                      ibuffer_column("locked"), ibuffer_text(" "), &
                                      ibuffer_column("name", 18, 18, IBUFFER_ALIGN_LEFT, .true.), ibuffer_text(" "), &
                                      ibuffer_column("size", 9, -1, IBUFFER_ALIGN_RIGHT), ibuffer_text(" "), &
                                      ibuffer_column("mode", 16, 16, IBUFFER_ALIGN_LEFT, .true.), ibuffer_text(" "), &
                                      ibuffer_column("filename_and_process")])
            call ibuffer_formats%add([ibuffer_column("mark"), ibuffer_text(" "), ibuffer_column("name", 16, -1), &
                                      ibuffer_text(" "), ibuffer_column("filename")])
        end if

        if (.not. allocated(ibuffer_fontification_table)) then
            allocate (ibuffer_fontification_table(7))
            ibuffer_fontification_table(1) = ibuffer_fontification_type(10, form_read_only, "font_lock_constant_face")
            ibuffer_fontification_table(2) = ibuffer_fontification_type(15, form_compressed, "font_lock_doc_face")
            ibuffer_fontification_table(3) = ibuffer_fontification_type(20, form_starred, "font_lock_keyword_face")
            ibuffer_fontification_table(4) = ibuffer_fontification_type(25, hidden_current, "italic")
            ibuffer_fontification_table(5) = ibuffer_fontification_type(30, form_help_mode, "font_lock_comment_face")
            ibuffer_fontification_table(6) = ibuffer_fontification_type(35, form_dired, "font_lock_function_name_face")
            ibuffer_fontification_table(7) = ibuffer_fontification_type(40, form_locked, "ibuffer_locked_buffer")
        end if

        if (.not. allocated(ibuffer_default_sorting_mode)) ibuffer_default_sorting_mode = "recency"
        if (.not. allocated(ibuffer_eliding_string)) ibuffer_eliding_string = "..."

        if (.not. allocated(ibuffer_maybe_show_predicates)) then
            allocate (ibuffer_maybe_show_predicates(1))
            ibuffer_maybe_show_predicates(1)%predicate => is_ibuffer_hidden_buffer
        end if

        if (.not. allocated(ibuffer_help_buffer_modes)) then
            allocate (ibuffer_help_buffer_modes(3))
            ibuffer_help_buffer_modes(1) = "help_mode"
            ibuffer_help_buffer_modes(2) = "apropos_mode"
            ibuffer_help_buffer_modes(3) = "info_mode"
        end if

        if (.not. allocated(ibuffer_compressed_file_name_regexp)) then
            ibuffer_compressed_file_name_regexp = "\.\(arj\|bgz\|bz2\|gz\|lzh\|taz\|tgz\|xz\|zip\|z\)$"
        end if

        if (.not. allocated(ibuffer_marked_face)) ibuffer_marked_face = "ibuffer_marked"
        if (.not. allocated(ibuffer_deletion_face)) ibuffer_deletion_face = "ibuffer_deletion"
        if (.not. allocated(ibuffer_title_face)) ibuffer_title_face = "ibuffer_title"
        if (.not. allocated(ibuffer_filter_group_name_face)) ibuffer_filter_group_name_face = "ibuffer_filter_group_name"
        if (.not. allocated(ibuffer_directory_abbrev_table)) allocate (ibuffer_directory_abbrev_table(0))
        if (.not. allocated(ibuffer_never_show_predicates)) allocate (ibuffer_never_show_predicates(0))
        if (.not. allocated(ibuffer_always_show_predicates)) allocate (ibuffer_always_show_predicates(0))

        if (.not. allocated(ibuffer_never_search_content_name)) then
            allocate (ibuffer_never_search_content_name(13))
            ibuffer_never_search_content_name(1) = "\`\*Completions\*\'"
            ibuffer_never_search_content_name(2) = "\`\*Help\*\'"
            ibuffer_never_search_content_name(3) = "\`\*Messages\*\'"
            ibuffer_never_search_content_name(4) = "\`\*Pp Eval Output\*\'"
            ibuffer_never_search_content_name(5) = "\`\*CompileLog\*\'"
            ibuffer_never_search_content_name(6) = "\`\*Info\*\'"
            ibuffer_never_search_content_name(7) = "\`\*Buffer List\*\'"
            ibuffer_never_search_content_name(8) = "\`\*Ibuffer\*\'"
            ibuffer_never_search_content_name(9) = "\`\*Apropos\*\'"
            ibuffer_never_search_content_name(10) = "\`\*Customize Option: "
            ibuffer_never_search_content_name(11) = "\`\*Async Shell Command\*"
            ibuffer_never_search_content_name(12) = "\`\*Shell Command Output\*"
            ibuffer_never_search_content_name(13) = "\`\*ediff "
        end if

        if (.not. allocated(ibuffer_never_search_content_mode)) then
            allocate (ibuffer_never_search_content_mode(1))
            ibuffer_never_search_content_mode(1) = "dired_mode"
        end if

        if (.not. allocated(ibuffer_saved_filters%entries)) then
            call ibuffer_saved_filters%add("programming", &
                                           ibuffer_or(ibuffer_derived_mode("prog_mode"), &
                                                      ibuffer_major_mode("ess_mode"), &
                                                      ibuffer_major_mode("compilation_mode")))
            call ibuffer_saved_filters%add("text document", &
                                           ibuffer_and(ibuffer_derived_mode("text_mode"), &
                                                       ibuffer_not(ibuffer_starred_name())))
            call ibuffer_saved_filters%add("TeX", &
                                           ibuffer_or(ibuffer_derived_mode("tex_mode"), &
                                                      ibuffer_major_mode("latex_mode"), &
                                                      ibuffer_major_mode("context_mode"), &
                                                      ibuffer_major_mode("ams_tex_mode"), &
                                                      ibuffer_major_mode("bibtex_mode")))
            call ibuffer_saved_filters%add("web", &
                                           ibuffer_or(ibuffer_derived_mode("sgml_mode"), &
                                                      ibuffer_derived_mode("css_base_mode"), &
                                                      ibuffer_derived_mode("js_base_mode"), &
                                                      ibuffer_derived_mode("typescript_ts_base_mode"), &
                                                      ibuffer_major_mode("js2_mode"), &
                                                      ibuffer_derived_mode("haml_mode"), &
                                                      ibuffer_major_mode("sass_mode")))
            call ibuffer_saved_filters%add("gnus", &
                                           ibuffer_or(ibuffer_major_mode("message_mode"), &
                                                      ibuffer_major_mode("mail_mode"), &
                                                      ibuffer_major_mode("gnus_group_mode"), &
                                                      ibuffer_major_mode("gnus_summary_mode"), &
                                                      ibuffer_major_mode("gnus_article_mode")))
        end if

        if (.not. allocated(ibuffer_saved_filter_groups%entries)) allocate (ibuffer_saved_filter_groups%entries(0))
        if (.not. allocated(ibuffer_filter_format_table%entries)) allocate (ibuffer_filter_format_table%entries(0))
        call ibuffer_marked_face_def%define("ibuffer_marked", "Face used by default in `ibuffer_marked_face'.")
        call ibuffer_marked_face_def%spec_entry(display_when(), face_attrs(inherit="warning"))
        call ibuffer_deletion_face_def%define("ibuffer_deletion", "Face used by default in `ibuffer_deletion_face'.")
        call ibuffer_deletion_face_def%spec_entry(display_when(), face_attrs(inherit="error"))
        call ibuffer_title_face_def%define("ibuffer_title", "Face used by default in `ibuffer_title_face'.")
        call ibuffer_title_face_def%spec_entry(display_when(), face_attrs(inherit="font_lock_type_face"))
        call ibuffer_filter_group_name_face_def%define("ibuffer_filter_group_name", &
                                                       "Face used by default in `ibuffer_filter_group_name_face'.")
        call ibuffer_filter_group_name_face_def%spec_entry(display_when(), face_attrs(inherit="bold"))
        call ibuffer_locked_buffer%define("ibuffer_locked_buffer", "Face used for locked buffers in Ibuffer.")
        call ibuffer_locked_buffer%spec_entry(display_when(backgrounds=[FACE_BACKGROUND_DARK]), &
                                              face_attrs(foreground="RosyBrown"))
        call ibuffer_locked_buffer%spec_entry(display_when(), face_attrs(foreground="brown4"))
        ! The columns (define-ibuffer-column).
        if (allocated(ibuffer_columns)) deallocate (ibuffer_columns)
        allocate (ibuffer_columns(0))
        call define_ibuffer_column("mark", column_mark, title=" ")
        call define_ibuffer_column("read_only", column_read_only, title="R")
        call define_ibuffer_column("locked", column_locked, title="L")
        call define_ibuffer_column("modified", column_modified, title="M")
        call define_ibuffer_column("name", column_name, summarizer=summarize_name)
        call define_ibuffer_column("size", column_size, summarizer=summarize_size)
        call define_ibuffer_column("recency", column_recency, summarizer=summarize_ignore)
        call define_ibuffer_column("mode", column_mode)
        call define_ibuffer_column("process", column_process, summarizer=summarize_process)
        call define_ibuffer_column("filename", column_filename, summarizer=summarize_filename)
        call define_ibuffer_column("filename_and_process", column_filename_and_process, title="Filename/Process", &
                                   summarizer=summarize_filename_and_process)
        if (allocated(ibuffer_sorting_functions)) deallocate (ibuffer_sorting_functions)
        allocate (ibuffer_sorting_functions(0))
        call define_ibuffer_sorter("recency", "recency", sort_recency)
        call major_modes%ibuffer_mode%define("ibuffer_mode", "IBuffer", ibuffer_mode, ibuffer_mode_hook, &
                                             parent=major_modes%special_mode)
        ibuffer_mode_map = major_modes%ibuffer_mode%map
        m = ibuffer_mode_map
        commands%ibuffer_mode%run => ibuffer_mode
        commands%ibuffer%run => cmd_ibuffer
        commands%ibuffer_other_window%run => cmd_ibuffer_other_window
        commands%ibuffer_list_buffers%run => cmd_ibuffer_list_buffers
        commands%ibuffer_jump%run => cmd_ibuffer_jump
        commands%ibuffer_find_file%run => ibuffer_find_file
        commands%ibuffer_backward_line%run => cmd_ibuffer_backward_line
        commands%ibuffer_forward_line%run => cmd_ibuffer_forward_line
        commands%ibuffer_visit_buffer%run => cmd_ibuffer_visit_buffer
        commands%ibuffer_visit_buffer_other_window%run => cmd_ibuffer_visit_buffer_other_window
        commands%ibuffer_visit_buffer_other_window_noselect%run => ibuffer_visit_buffer_other_window_noselect
        commands%ibuffer_visit_buffer_1_window%run => ibuffer_visit_buffer_1_window
        commands%ibuffer_bury_buffer%run => ibuffer_bury_buffer
        commands%ibuffer_do_view%run => ibuffer_do_view
        commands%ibuffer_do_view_horizontally%run => ibuffer_do_view_horizontally
        commands%ibuffer_do_save%run => ibuffer_do_save
        commands%ibuffer_do_toggle_modified%run => ibuffer_do_toggle_modified
        commands%ibuffer_do_toggle_read_only%run => cmd_ibuffer_do_toggle_read_only
        commands%ibuffer_do_toggle_lock%run => ibuffer_do_toggle_lock
        commands%ibuffer_do_delete%run => ibuffer_do_delete
        commands%ibuffer_do_kill_on_deletion_marks%run => ibuffer_do_kill_on_deletion_marks
        commands%ibuffer_unmark_all%run => cmd_ibuffer_unmark_all
        commands%ibuffer_unmark_all_marks%run => ibuffer_unmark_all_marks
        commands%ibuffer_toggle_marks%run => cmd_ibuffer_toggle_marks
        commands%ibuffer_change_marks%run => cmd_ibuffer_change_marks
        commands%ibuffer_mark_forward%run => cmd_ibuffer_mark_forward
        commands%ibuffer_unmark_forward%run => cmd_ibuffer_unmark_forward
        commands%ibuffer_unmark_backward%run => cmd_ibuffer_unmark_backward
        commands%ibuffer_mark_for_delete%run => cmd_ibuffer_mark_for_delete
        commands%ibuffer_mark_for_delete_backwards%run => cmd_ibuffer_mark_for_delete_backwards
        commands%ibuffer_recompile_formats%run => ibuffer_recompile_formats
        commands%ibuffer_do_sort_by_recency%run => ibuffer_do_sort_by_recency
        commands%ibuffer_switch_format%run => ibuffer_switch_format
        commands%ibuffer_redisplay%run => cmd_ibuffer_redisplay
        commands%ibuffer_update%run => cmd_ibuffer_update
        call keymap_set(m, "0", "digit_argument", st)
        call keymap_set(m, "1", "digit_argument", st)
        call keymap_set(m, "2", "digit_argument", st)
        call keymap_set(m, "3", "digit_argument", st)
        call keymap_set(m, "4", "digit_argument", st)
        call keymap_set(m, "5", "digit_argument", st)
        call keymap_set(m, "6", "digit_argument", st)
        call keymap_set(m, "7", "digit_argument", st)
        call keymap_set(m, "8", "digit_argument", st)
        call keymap_set(m, "9", "digit_argument", st)
        call keymap_set(m, "m", "ibuffer_mark_forward", st)
        call keymap_set(m, "t", "ibuffer_toggle_marks", st)
        call keymap_set(m, "u", "ibuffer_unmark_forward", st)
        call keymap_set(m, "DEL", "ibuffer_unmark_backward", st)
        call keymap_set(m, "M-DEL", "ibuffer_unmark_all", st)
        call keymap_set(m, "* *", "ibuffer_unmark_all", st)
        call keymap_set(m, "* c", "ibuffer_change_marks", st)
        call keymap_set(m, "U", "ibuffer_unmark_all_marks", st)
        call keymap_set(m, "d", "ibuffer_mark_for_delete", st)
        call keymap_set(m, "C-d", "ibuffer_mark_for_delete_backwards", st)
        call keymap_set(m, "x", "ibuffer_do_kill_on_deletion_marks", st)
        ! immediate operations
        call keymap_set(m, "n", "ibuffer_forward_line", st)
        call keymap_set(m, "SPC", "forward_line", st)
        call keymap_set(m, "p", "ibuffer_backward_line", st)
        call keymap_set(m, "l", "ibuffer_redisplay", st)
        call keymap_set(m, "g", "ibuffer_update", st)
        call keymap_set(m, "`", "ibuffer_switch_format", st)
        call keymap_set(m, "b", "ibuffer_bury_buffer", st)
        call keymap_set(m, "s v", "ibuffer_do_sort_by_recency", st)
        call keymap_set(m, "~", "ibuffer_do_toggle_modified", st)
        ! marked operations
        call keymap_set(m, "A", "ibuffer_do_view", st)
        call keymap_set(m, "D", "ibuffer_do_delete", st)
        call keymap_set(m, "M", "ibuffer_do_toggle_modified", st)
        call keymap_set(m, "S", "ibuffer_do_save", st)
        call keymap_set(m, "T", "ibuffer_do_toggle_read_only", st)
        call keymap_set(m, "L", "ibuffer_do_toggle_lock", st)
        call keymap_set(m, "RET", "ibuffer_visit_buffer", st)
        call keymap_set(m, "e", "ibuffer_visit_buffer", st)
        call keymap_set(m, "f", "ibuffer_visit_buffer", st)
        call keymap_set(m, "C-x C-f", "ibuffer_find_file", st)
        call keymap_set(m, "o", "ibuffer_visit_buffer_other_window", st)
        call keymap_set(m, "C-o", "ibuffer_visit_buffer_other_window_noselect", st)
        call keymap_set(m, "M-o", "ibuffer_visit_buffer_1_window", st)
        call keymap_set(m, "v", "ibuffer_do_view", st)
        call keymap_set(m, "C-x v", "ibuffer_do_view_horizontally", st)
        call keymap_set(m, "C-x 4 RET", "ibuffer_visit_buffer_other_window", st)
        call syms_of_ibuf_ext()
    end subroutine syms_of_ibuffer
end module ibuffer_fm
