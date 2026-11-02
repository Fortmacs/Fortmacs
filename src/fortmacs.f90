! fortmacs.f90 -- the configuration API: what init.f90 uses.
!
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

!> The configuration API.  A user's init.f90 is a module named
!> `user_config' with a public subroutine `configure', which runs once at
!> startup (where upstream loads the init file):
!>
!>     module user_config
!>         use fortmacs
!>         implicit none(type, external)
!>         private
!>         public :: configure
!>         type(command_type), target :: insert_date_stamp_command
!>     contains
!>         subroutine configure(status)
!>             type(status_type), intent(out) :: status
!>
!>             tab_width = 4
!>             create_backup_files = .false.
!>             insert_date_stamp_command = command_type("insert_date_stamp", insert_date_stamp)
!>             call insert_date_stamp_command%define()
!>             call keymap_global_set("C-c d", "insert_date_stamp", status)
!>         end subroutine configure
!>
!>         subroutine insert_date_stamp(status)
!>             type(status_type), intent(out) :: status
!>             character(len=8) :: date
!>
!>             call date_and_time(date=date)
!>             call insert(date, status)
!>         end subroutine insert_date_stamp
!>     end module user_config
!>
!> Settings are the variables below; assign them (that sets the default
!> value; `call fill_column%set_local(60)' sets the current buffer's own
!> value).  Commands are subroutines taking a `status_type', defined as
!> `command_type' objects: built-in ones are the components of
!> `commands', so that `commands%upcase_region%disabled = .false.'
!> enables C-x C-u, and `call commands%paste%execute(status)' executes one.  Keys
!> are bound with `keymap_global_set'.  Modes are customized from
!> their hooks and keymaps, named after their Emacs variables: `call text_mode_hook%append(turn_on_auto_fill)'.  New modes
!> are `major_mode_type' and `minor_mode_type' objects made known with
!> their `define'.  Faces are the components of `faces':
!> `faces%region%background = "blue"', with `face_underline' and `face_box'
!> for the property-list values of `:underline' and `:box'.  A `configure' that fails (returns a
!> status that is not ok) leaves the editor running and shows the error.
!> `fortmacs -q' starts without running `configure'.
!>
!> This module only gathers names from the modules that define them, so
!> the API stays one `use' away.
module fortmacs
    use, intrinsic :: iso_fortran_env, only: int64
    ! Errors and values.
    use error_mod, only: status_type, set_error, format_error, FM_GENERIC_ERROR
    ! Commands and keys.
    use commands_mod, only: commands
    ! Faces.
    use face_table_mod, only: faces
    use faces_mod, only: face_underline, face_box, FACE_STYLE_LINE, FACE_STYLE_DOUBLE_LINE, FACE_STYLE_WAVE, &
                         FACE_STYLE_DOTS, FACE_STYLE_DASHES, FACE_BOX_RELEASED_BUTTON, FACE_BOX_PRESSED_BUTTON, &
                         FACE_BOX_FLAT_BUTTON
    use callint_mod, only: command_procedure, command_type, set_command_disabled
    use keymap_mod, only: keymap_global_set, keymap_set, is_key_valid, current_global_map
    use subr_mod, only: global_set_key, read_y_or_n
    ! Buffers and editing.
    use handle_mod, only: handle_type
    use buffer_mod, only: current_buffer, get_or_make_buffer, set_buffer, buffer_name
    use insdel_mod, only: insert
    use editfns_mod, only: point, point_min, point_max, goto_char, buffer_substring, delete_region, is_bob, is_eob, is_bol, &
                           is_eol, region_beginning, region_end
    use cmds_mod, only: forward_char, backward_char, forward_line, beginning_of_line, end_of_line
    use files_mod, only: find_file, save_buffer
    use fns_mod, only: read_yes_or_no
    use minibuf_mod, only: read_string
    use xdisp_mod, only: message
    use search_mod, only: search_forward, search_backward, match_beginning, match_end, SEARCH_NOERROR, SEARCH_MOVE
    use isearch_mod, only: isearch_forward_command, isearch_backward_command
    use simple_mod, only: undo_only, undo_redo
    use undo_mod, only: undo_boundary
    use simple_mod, only: copy_to_clipboard_history, current_clipboard_entry, cut_region, push_mark, set_mark, mark, &
                          is_region_active, &
                          deactivate_mark
    use minibuffer_mod, only: read_file_name
    use syntax_mod, only: forward_word, modify_syntax_entry, char_syntax, standard_syntax_table, make_syntax_table, &
                          copy_syntax_table, set_syntax_table, syntax_table
    use simple_mod, only: backward_word, cut_word, backward_cut_word, mark_word
    use paragraphs_mod, only: forward_paragraph, backward_paragraph, mark_paragraph, cut_paragraph, &
                              backward_cut_paragraph
    use help_fns_mod, only: describe_function, describe_variable
    use help_mod, only: substitute_command_keys
    use window_mod, only: split_window, delete_window, delete_other_windows, other_window, select_window, &
                          other_buffer, switch_to_buffer, quit_window
    use frame_mod, only: selected_window
    use buffer_mod, only: close_buffer
    use minibuffer_mod, only: read_buffer
    use search_mod, only: re_search_forward, re_search_backward, posix_search_forward, posix_search_backward, &
                          looking_at, posix_looking_at, string_match, posix_string_match, match_data, set_match_data, &
                          regexp_quote
    use subr_mod, only: match_string, match_string_no_properties, string_matches, is_looking_at, looking_back
    use isearch_mod, only: isearch_forward_regexp_command, isearch_backward_regexp_command
    use paragraphs_mod, only: forward_sentence, backward_sentence, cut_sentence, backward_cut_sentence, &
                              mark_end_of_sentence, sentence_end_regexp
    use search_mod, only: replace_match
    use subr_mod, only: match_substitute_replacement
    use casefiddle_mod, only: upcase_string, downcase_string, capitalize_string, upcase_initials_string, upcase_char, &
                              downcase_char, capitalize_char, upcase_initials_char, upcase_region, downcase_region, &
                              capitalize_region, upcase_initials_region, upcase_word, downcase_word, capitalize_word
    use simple_mod, only: transpose_chars, transpose_words, transpose_lines, delete_horizontal_space, &
                          delete_all_space, just_one_space, fixup_whitespace, delete_blank_lines, delete_indentation
    use paragraphs_mod, only: transpose_sentences, transpose_paragraphs
    use syntax_mod, only: skip_chars_forward, skip_chars_backward, skip_syntax_forward, skip_syntax_backward
    use syntax_mod, only: is_syntax_table_live, internal_describe_syntax_value
    use chartab_mod, only: make_char_table, is_char_table, char_table_subtype, char_table_parent, set_char_table_parent, &
                           char_table_extra_slot, set_char_table_extra_slot, char_table_extra_table, &
                           set_char_table_extra_table, char_table_range, set_char_table_range, map_char_table, &
                           optimize_char_table, char_table_map_procedure, NO_VALUE
    use casetab_mod, only: is_case_table, current_case_table, standard_case_table, set_case_table, &
                           set_standard_case_table
    use category_mod, only: category_set_type, make_category_set, define_category, category_docstring, &
                            get_unused_category, is_category_table, category_table, standard_category_table, &
                            copy_category_table, make_category_table, set_category_table, char_category_set, &
                            category_set_mnemonics, modify_category_entry
    use novice_mod, only: enable_command, disable_command
    use rmc_mod, only: rmc_choice_type, read_multiple_choice
    use indent_mod, only: indent_to, current_indentation, indent_according_to_mode, indent_rigidly, indent_line_to, &
                          current_left_margin, move_to_left_margin, indent_region, indent_relative, tab_to_tab_stop, &
                          move_to_tab_stop, beginning_of_line_text, indent_line_procedure, indent_region_procedure
    use fill_mod, only: fill_paragraph, fill_region, fill_region_as_paragraph, unfill_paragraph, set_fill_prefix, &
                        justify_current_line, canonically_space_region, current_fill_column, current_justification, &
                        is_fill_single_word_nobreak, is_fill_french_nobreak, is_fill_polish_nobreak, &
                        is_fill_single_char_nobreak, fill_nobreak_function_type, DEFAULT_JUSTIFICATION_NONE, &
                        DEFAULT_JUSTIFICATION_LEFT, &
                        DEFAULT_JUSTIFICATION_RIGHT, DEFAULT_JUSTIFICATION_FULL, DEFAULT_JUSTIFICATION_CENTER, &
                        DEFAULT_JUSTIFICATION_CURRENT
    use simple_mod, only: back_to_indentation, do_auto_fill, default_indent_new_line
    use subr_mod, only: read_number
    use replace_mod, only: query_replace, query_replace_regexp, map_query_replace_regexp, replace_string, &
                           replace_regexp, replacement_procedure
    use simple_mod, only: count_lines, line_number_at_pos
    use rect_mod, only: apply_on_rectangle, rectangle_line_action_type, rectangle_line_type, rectangle_position_as_coordinates, &
                        is_rectangle_intersect, rectangle_dimensions, delete_rectangle, delete_extract_rectangle, &
                        extract_rectangle, extract_rectangle_bounds, cut_rectangle, copy_rectangle, &
                        paste_rectangle, insert_rectangle, open_rectangle, delete_whitespace_rectangle, string_rectangle, &
                        string_insert_rectangle, clear_rectangle, rectangle_number_lines, spaces_string, &
                        rectangle_mark_mode, rectangle_mark_mode_map, rectangle_mark_mode_hook
    use simple_mod, only: deactivate_mark_hook, region_bounds, is_region_noncontiguous
    use editfns_mod, only: interactive_region
    use register_mod, only: register_type, register_contents_type, get_register, set_register, point_to_register, &
                            window_configuration_to_register, jump_to_register, file_to_register, buffer_to_register, &
                            number_to_register, increment_register, view_register, list_registers, insert_register, &
                            copy_to_register, append_to_register, prepend_to_register, copy_rectangle_to_register, &
                            register_read_with_preview, REGISTER_EMPTY, REGISTER_MARKER, REGISTER_TEXT, &
                            REGISTER_RECTANGLE, REGISTER_NUMBER, REGISTER_WINDOW_CONFIGURATION, REGISTER_FILE, &
                            REGISTER_FILE_QUERY, REGISTER_BUFFER
    ! Buffer-local variables, hooks and modes.
    use subr_mod, only: execute_mode_hooks
    use simple_mod, only: fundamental_mode, special_mode
    use text_mode_mod, only: text_mode
    use prog_mode_mod, only: prog_mode
    use files_mod, only: normal_mode, set_auto_mode, prepend_auto_mode, file_name_sans_versions
    ! Settings.
    use xdisp_mod, only: tab_width, truncate_partial_width_windows
    use files_mod, only: create_backup_files, change_major_mode_with_file_name
    use simple_mod, only: line_move_visual, goal_column
    use subr_mod, only: history_length, history_delete_duplicates, read_y_or_n_use_read_key
    use fns_mod, only: use_short_answers
    use rmc_mod, only: read_char_choice_use_read_key
    use keyboard_mod, only: minibuffer_message_timeout
    use keyboard_mod, only: echo_keystrokes, echo_keystrokes_help, translate_upper_case_key_bindings, help_char, &
                            help_events, inhibit_interaction, num_nonmacro_input_events, read_char, &
                            read_char_exclusive, read_event, read_key_sequence_vs, this_command_key_events, &
                            this_single_command_keys, this_single_command_raw_keys, clear_this_command_keys, &
                            recursion_depth, top_level, recursive_edit, lossage_size, event_convert_list, &
                            event_name_parse_modifiers, pre_command_hook, post_command_hook
    use fileio_mod, only: open_dribble_file
    use minibuffer_mod, only: read_file_name_completion_ignore_case, read_buffer_completion_ignore_case
    use minibuf_mod, only: enable_recursive_minibuffers, history_append_new_input
    use minibuffer_mod, only: insert_default_directory
    use editfns_mod, only: inhibit_field_text_motion
    use buffer_mod, only: transient_mark_mode
    use callint_mod, only: mark_even_if_inactive
    use simple_mod, only: clipboard_history_max, mark_ring_max, cut_whole_line_option, delete_active_region, cut_read_only_ok
    use rect_mod, only: last_cut_rectangle
    use register_mod, only: registers, register_separator
    use term_mod, only: visible_bell
    use minibuf_mod, only: completion_ignore_case
    use minibuffer_mod, only: completion_auto_help, completion_show_inline_help, completion_fail_discreetly
    use simple_mod, only: suggest_key_bindings
    use dired_mod, only: completion_ignored_extensions
    use search_mod, only: case_fold_search
    ! Font Lock.
    use font_core_mod, only: font_lock_defaults, font_lock_defaults_setting_type, font_lock_global_modes, &
                             font_lock_global_modes_type, font_lock_mode_hook, global_font_lock_mode_hook, &
                             turn_on_font_lock, font_lock_keyword_type, font_lock_highlight_type, &
                             font_lock_match_highlight_type, font_lock_keywords_type, font_lock_syntax_entry_type, &
                             FONT_LOCK_OVERRIDE_NO, FONT_LOCK_OVERRIDE_YES, FONT_LOCK_OVERRIDE_KEEP, &
                             FONT_LOCK_OVERRIDE_PREPEND, FONT_LOCK_OVERRIDE_APPEND, font_lock_matcher_procedure, &
                             font_lock_face_procedure, font_lock_form_procedure
    use font_lock_mod, only: font_lock_maximum_decoration, FONT_LOCK_MAXIMUM_DECORATION_MAX, font_lock_verbose, &
                             font_lock_dont_widen, font_lock_support_mode, FONT_LOCK_SUPPORT_MODE_NONE, &
                             FONT_LOCK_SUPPORT_MODE_JIT_LOCK, font_lock_extra_managed_props, font_lock_syntactic_face_function, &
                             font_lock_keyword, font_lock_highlight, font_lock_anchored, font_lock_add_keywords, &
                             FONT_LOCK_ADD_BEGINNING, FONT_LOCK_ADD_END, FONT_LOCK_ADD_SET
    use jit_lock_mod, only: jit_lock_chunk_size
    use xdisp_mod, only: fontification_functions
    use hooks_mod, only: position_hook_type, position_procedure
    use isearch_mod, only: search_upper_case, search_exit_option, search_nonincremental_instead, search_ring_max, &
                           SEARCH_UPPER_CASE_NO, SEARCH_UPPER_CASE_YES, SEARCH_UPPER_CASE_NOT_PASTED
    use window_mod, only: next_screen_context_lines, scroll_error_top_bottom
    use syntax_mod, only: words_include_escapes
    use search_mod, only: search_spaces_regexp
    use isearch_mod, only: regexp_search_ring_max, search_whitespace_regexp, isearch_lax_whitespace, &
                           isearch_regexp_lax_whitespace
    use paragraphs_mod, only: paragraph_start, paragraph_separate, sentence_end, sentence_end_base, &
                              sentence_end_double_space, sentence_end_without_period, sentence_end_without_space
    use casefiddle_mod, only: case_identifiers_as_words
    use simple_mod, only: fill_prefix, cycle_spacing_actions, cycle_spacing_action_type, CYCLE_JUST_ONE_SPACE, &
                          CYCLE_DELETE_SPACE_AFTER, CYCLE_DELETE_SPACE_BEFORE, CYCLE_DELETE_ALL_SPACE, CYCLE_RESTORE, &
                          CYCLE_ARG_NONE, CYCLE_ARG_FIXED, CYCLE_ARG_INVERTED, CYCLE_ARG_MINUS
    use novice_mod, only: disabled_command_function_variable
    use buffer_mod, only: fill_column, left_margin, auto_fill_function
    use indent_mod, only: indent_tabs_mode, standard_indent, tab_always_indent, tab_stops, indent_line_function, &
                          indent_region_function
    use fill_mod, only: colon_double_space, fill_separate_heterogeneous_words_with_space, adaptive_fill_mode, &
                        adaptive_fill_regexp, adaptive_fill_first_line_regexp, fill_indent_according_to_mode, &
                        default_justification, fill_nobreak_functions
    use simple_mod, only: auto_fill_inhibit_regexp, inhibit_auto_fill, normal_auto_fill_function
    use replace_mod, only: case_replace, replace_lax_whitespace, replace_regexp_lax_whitespace, &
                           query_replace_skip_read_only, query_replace_show_replacement, query_replace_from_to_separator
    use replace_mod, only: occur, occur_in_region, multi_occur, multi_occur_in_matching_buffers, keep_lines, flush_lines, &
                           cut_matching_lines, copy_matching_lines, how_many, occur_mode, occur_edit_mode, &
                           occur_rename_buffer, occur_source_type, list_matching_lines_default_context_lines, &
                           list_matching_lines_face, list_matching_lines_buffer_name_face, &
                           list_matching_lines_current_line_face, list_matching_lines_jump_to_current_line, &
                           list_matching_lines_prefix_face, occur_excluded_properties, occur_excluded_properties_all, &
                           occur_mode_hook, occur_edit_mode_hook, occur_hook, occur_mode_find_occurrence_hook, &
                           occur_mode_map, occur_edit_mode_map
    ! Settings, hooks and modes as objects.
    use data_mod, only: setting_type, integer_setting_type, logical_setting_type, string_setting_type, function_setting_type
    use hooks_mod, only: hook_type, change_major_mode_hook, change_hook_type, after_change_hook_type, change_procedure, &
                         after_change_procedure
    use insdel_mod, only: before_change_functions, after_change_functions, first_change_hook, inhibit_modification_hooks
    use subr_mod, only: change_major_mode_after_body_hook, after_change_major_mode_hook
    use derived_mod, only: major_mode_type, is_derived_mode, provided_mode_derived_p, major_mode_named
    use easy_mmode_mod, only: minor_mode_type, minor_mode_body_procedure
    use text_mode_mod, only: text_mode_hook, text_mode_map, text_mode_syntax_table
    use prog_mode_mod, only: prog_mode_hook, prog_mode_map
    use simple_mod, only: special_mode_hook, special_mode_map, transient_mark_mode_hook, indent_tabs_mode_hook, &
                          auto_fill_mode_hook
    use help_mode_mod, only: help_mode_hook, help_mode_map
    use simple_mod, only: turn_on_auto_fill, turn_off_auto_fill
    use text_mode_mod, only: text_mode_variant
    use syntax_mod, only: parse_expression_ignore_comments, comment_end_can_be_escaped, multibyte_syntax_as_identifier, &
                          open_paren_in_column_0_is_subprogram_start, comment_use_syntax_ppss
    use syntax_mod, only: forward_comment, scan_groups, scan_expressions, parse_partial_expression, parse_state_type, &
                          ST_COMMENT_STYLE, ST_STRING_STYLE, COMMENTSTOP_NO, COMMENTSTOP_COMMENT, COMMENTSTOP_SYNTAX_TABLE
    use expression_motion_mod, only: forward_expression_function, forward_expression, backward_expression, forward_group, &
                                     backward_group, down_group, up_group, &
                                     backward_up_group, cut_expression, backward_cut_expression, mark_expression
    ! Keyboard macros.
    use keyboard_mod, only: kbd_macro_termination_hook
    use kmacro_mod, only: kmacro_ring_max, kmacro_execute_before_append, kmacro_repeat_no_prefix, kmacro_execute_repeat_key, &
                          kmacro_execute_repeat_with_arg, KMACRO_EXECUTE_REPEAT_KEY_NO, KMACRO_EXECUTE_REPEAT_KEY_LAST
    use edmacro_mod, only: edmacro_eight_bits
    use files_mod, only: require_final_newline, mode_require_final_newline, FINAL_NEWLINE_NO, FINAL_NEWLINE_SAVE, &
                         FINAL_NEWLINE_VISIT, FINAL_NEWLINE_VISIT_SAVE, FINAL_NEWLINE_ASK
    ! Dired.
    use dired_mode_mod, only: dired, dired_other_window, dired_noselect, dired_mode, dired_mode_map, dired_mode_hook, &
                              dired_before_readin_hook, dired_after_readin_hook, dired_initial_position_hook, &
                              dired_listing_switches, dired_use_ls_dired, DIRED_USE_LS_DIRED_NO, DIRED_USE_LS_DIRED_YES, &
                              DIRED_USE_LS_DIRED_UNSPECIFIED, dired_ls_f_marks_symlinks, dired_trivial_filenames, &
                              dired_free_space, DIRED_FREE_SPACE_NONE, DIRED_FREE_SPACE_SEPARATE, DIRED_FREE_SPACE_FIRST, &
                              dired_mark_region, DIRED_MARK_REGION_NO, DIRED_MARK_REGION_FILE, DIRED_MARK_REGION_LINE, &
                              dired_movement_style, DIRED_MOVEMENT_STYLE_PLAIN, DIRED_MOVEMENT_STYLE_CYCLE, &
                              DIRED_MOVEMENT_STYLE_BOUNDED, DIRED_MOVEMENT_STYLE_CYCLE_FILES, &
                              DIRED_MOVEMENT_STYLE_BOUNDED_FILES, dired_auto_revert_buffer, dired_check_symlinks, &
                              dired_recursive_deletes, DIRED_RECURSIVE_NO, DIRED_RECURSIVE_YES, DIRED_RECURSIVE_TOP, &
                              DIRED_RECURSIVE_ASK, dired_no_confirm, dired_no_confirm_type, dired_switches_in_mode_line, &
                              DIRED_SWITCHES_IN_MODE_LINE_SORT_ORDER, DIRED_SWITCHES_IN_MODE_LINE_AS_IS, dired_sort_inhibit, &
                              dired_garbage_files_regexp, dired_always_read_filesystem, dired_deletion_confirmer, &
                              dired_header_face, dired_mark_face, dired_marked_face, dired_flagged_face, &
                              dired_warning_face, dired_perm_write_face, dired_directory_face, dired_symlink_face, &
                              dired_ignored_face
    use files_mod, only: insert_directory_program, find_file_run_dired, find_file_other_window, revert_buffer
    ! Dired file operations (dired-aux.el) and View mode.
    use dired_mode_mod, only: dired_chown_program, dired_touch_program, DIRED_KEEP_MARKER, dired_keep_marker_rename, &
                              dired_keep_marker_copy, dired_keep_marker_hardlink, dired_keep_marker_symlink, &
                              dired_keep_marker_relsymlink, dired_copy_preserve_time, dired_copy_dereference, &
                              dired_recursive_copies, dired_dwim_target, DIRED_DWIM_TARGET_NO, DIRED_DWIM_TARGET_NEXT, &
                              DIRED_DWIM_TARGET_NEXT_VISIBLE, DIRED_DWIM_TARGET_RECENT, dired_create_destination_dirs, &
                              DIRED_CREATE_DESTINATION_DIRS_NO, DIRED_CREATE_DESTINATION_DIRS_YES, &
                              DIRED_CREATE_DESTINATION_DIRS_ASK, dired_create_destination_dirs_on_trailing_dirsep, &
                              dired_do_revert_buffer, dired_create_empty_file_in_current_directory, &
                              dired_compress_file_default_suffix, dired_compress_directory_default_suffix, &
                              dired_compress_suffix_type, dired_compress_rule_type, dired_compress_file_suffixes, &
                              dired_compress_file_commands, dired_compress_files_commands
    use files_mod, only: copy_directory, copy_directory_create_symlink, view_read_only, find_file_read_only, &
                         find_file_read_only_other_window, create_empty_file, file_modes_symbolic_to_number, &
                         file_modes_number_to_symbolic
    use fileio_mod, only: copy_file, rename_file, add_name_to_file, create_symbolic_link, set_file_modes, set_file_times
    use simple_mod, only: read_only_mode, read_only_mode_hook, what_line
    use view_mod, only: view_mode, view_mode_map, view_mode_hook, view_file, view_file_other_window, view_buffer, &
                        view_buffer_other_window, view_scroll_auto_exit, view_exits_all_viewing_windows, &
                        view_inhibit_help_message, view_no_disable_on_exit
    ! Processes and shell commands.
    use process_mod, only: process_arg_type, process_filter_procedure, process_sentinel_procedure, make_process, start_process, &
                           delete_process, destroy_process, is_process, is_process_live, get_process, get_buffer_process, &
                           process_list, process_status, process_exit_status, process_id, process_name, &
                           process_command, process_buffer, set_process_buffer, process_mark, set_process_filter, &
                           set_process_sentinel, process_tty_name, set_process_query_on_exit_flag, &
                           process_query_on_exit_flag, interrupt_process, stop_process, quit_process, signal_process, &
                           process_send_string, process_send_region, process_send_eof, accept_process_output, &
                           internal_default_process_filter, internal_default_process_sentinel, &
                           process_connection_type, delete_exited_processes, PROCESS_CONNECTION_DEFAULT, &
                           PROCESS_CONNECTION_PIPE, PROCESS_CONNECTION_PTY, num_processors
    use compile_mod, only: compile, recompile, compilation_start, compilation_mode, stop_compilation, &
                           compile_command, compilation_read_command, compilation_ask_about_save, &
                           compilation_always_stop, compilation_disable_input, compilation_scroll_output, &
                           COMPILATION_SCROLL_OUTPUT_NO, COMPILATION_SCROLL_OUTPUT_YES, COMPILATION_SCROLL_OUTPUT_FIRST_ERROR, &
                           compilation_filter_hook, compilation_filter_start, compilation_mode_hook, &
                           compilation_mode_map, compilation_directory
    use compile_mod, only: compilation_rule_type, compilation_highlight_type, compilation_directory_matcher_type, &
                           compilation_transform_type, compilation_number_procedure, compilation_file_procedure, &
                           compilation_face_procedure, compilation_minor_mode_hook, compilation_shell_minor_mode_hook, &
                           compilation_minor_mode_map, compilation_shell_minor_mode_map, &
                           compilation_error_regexps, compilation_error_regexp_names, &
                           compilation_error_case_fold_search, compilation_directory_matcher, &
                           compilation_transform_file_matches, compilation_error_screen_columns, &
                           compilation_first_column, compilation_skip_threshold, compilation_skip_visited, &
                           compilation_skip_to_next_location, compilation_search_all_directories, &
                           compilation_context_lines, COMPILATION_CONTEXT_TOP, COMPILATION_CONTEXT_LINES_NO_SCROLL, &
                           compilation_message_face, compilation_error_face, compilation_warning_face, &
                           compilation_info_face, compilation_line_face, compilation_column_face, &
                           compilation_enter_directory_face, compilation_leave_directory_face, &
                           compilation_minor_mode, compilation_shell_minor_mode, compile_goto_error, &
                           compilation_next_error, compilation_previous_error, compilation_next_file, &
                           compilation_previous_file
    use grep_mod, only: grep, grep_find, lgrep, rgrep, zrgrep, grep_mode, grep_compute_defaults, grep_apply_setting, &
                        GREP_USE_NULL_DEVICE_NO, GREP_USE_NULL_DEVICE_YES, GREP_USE_NULL_DEVICE_AUTO_DETECT, &
                        GREP_USE_NULL_FILENAME_SEPARATOR_NO, GREP_USE_NULL_FILENAME_SEPARATOR_YES, &
                        GREP_USE_NULL_FILENAME_SEPARATOR_AUTO_DETECT, GREP_HIGHLIGHT_MATCHES_NO, GREP_HIGHLIGHT_MATCHES_YES, &
                        GREP_HIGHLIGHT_MATCHES_AUTO_DETECT, GREP_HIGHLIGHT_MATCHES_ALWAYS, GREP_HIGHLIGHT_MATCHES_AUTO, &
                        GREP_FIND_USE_XARGS_UNKNOWN, &
                        GREP_FIND_USE_XARGS_EXEC, GREP_FIND_USE_XARGS_EXEC_PLUS, GREP_FIND_USE_XARGS_GNU, &
                        GREP_FIND_USE_XARGS_GNU_SORT, GREP_FIND_USE_XARGS_OTHER, GREP_SAVE_BUFFERS_NO, &
                        GREP_SAVE_BUFFERS_YES, GREP_SAVE_BUFFERS_ASK, grep_files_alias_type, grep_ignore_type, &
                       grep_ignore_test_procedure, grep_read_files_procedure, grep_save_buffers_procedure, grep_highlight_matches, &
                        grep_match_regexp, grep_scroll_output, grep_command, grep_command_position, grep_template, &
                        grep_use_null_device, grep_use_null_filename_separator, grep_find_command, &
                        grep_find_command_position, grep_find_template, rgrep_find_ignores_in_f, grep_files_aliases, &
                        grep_find_ignored_directories, grep_find_ignored_files_variable, grep_save_buffers, &
                        grep_save_buffers_function, grep_error_screen_columns, grep_setup_hook, grep_mode_hook, &
                        grep_mode_map, grep_find_abbreviate, grep_use_headings, grep_program, find_program, &
                        xargs_program, grep_find_use_xargs, grep_read_files_function, find_name_arg
    use comint_mod, only: comint_inhibit_carriage_motion, comint_mode, make_comint, make_comint_in_buffer, comint_execute, &
                          comint_exec, comint_send_input, comint_send_string, comint_prompt_regexp, &
                          comint_prompt_read_only, comint_highlight_input, comint_input_ignoredups, &
                          comint_input_ring_file_name, comint_pager, comint_input_ring_file_prefix, &
                          comint_move_point_for_output, comint_move_point_for_matching_input, COMINT_MOVE_POINT_FOR_OUTPUT_NO, &
                          COMINT_MOVE_POINT_FOR_OUTPUT_YES, COMINT_MOVE_POINT_FOR_OUTPUT_ALL, &
                          COMINT_MOVE_POINT_FOR_OUTPUT_THIS, COMINT_MOVE_POINT_FOR_OUTPUT_OTHERS, &
                          COMINT_MOVE_POINT_FOR_MATCHING_INPUT_AFTER_INPUT, COMINT_MOVE_POINT_FOR_MATCHING_INPUT_END_OF_LINE, &
                          comint_scroll_show_maximum_output, &
                          comint_buffer_maximum_size, comint_input_ring_size, comint_input_ring_separator, &
                          comint_input_history_ignore, comint_process_echoes, comint_eol_on_send, &
                          comint_use_prompt_regexp, comint_terminfo_terminal, comint_delimiter_arguments, &
                          comint_mode_hook, comint_exec_hook, comint_output_filter_functions, &
                          comint_preoutput_filter_functions, comint_input_filter_functions, comint_mode_map, &
                          comint_truncate_buffer, &
                          comint_strip_ctrl_m, comint_postoutput_scroll_to_bottom, comint_show_maximum_output, &
                          comint_input_autoexpand, COMINT_AUTOEXPAND_NO, COMINT_AUTOEXPAND_ON_COMPLETION, COMINT_AUTOEXPAND_INPUT, &
                          COMINT_AUTOEXPAND_HISTORY, comint_scroll_to_bottom_on_input, COMINT_SCROLL_TO_BOTTOM_ON_INPUT_NO, &
                          COMINT_SCROLL_TO_BOTTOM_ON_INPUT_YES, COMINT_SCROLL_TO_BOTTOM_ON_INPUT_ALL, &
                          COMINT_SCROLL_TO_BOTTOM_ON_INPUT_THIS, &
                          comint_insert_previous_argument_from_end, comint_history_isearch, COMINT_HISTORY_ISEARCH_NO, &
                          COMINT_HISTORY_ISEARCH_DWIM, COMINT_HISTORY_ISEARCH_ALWAYS, comint_password_prompt_regexp, &
                          comint_password_prompt_max_length
    use mule_conf_mod, only: password_word_equivalents, password_colon_equivalents
    use ansi_color_mod, only: ansi_color_for_comint_mode, ansi_color_for_compilation_mode, ANSI_COLOR_NO, &
                              ANSI_COLOR_FILTER, ANSI_COLOR_YES, ansi_color_bold_is_bright, ansi_color_basic_faces, &
                              ansi_color_normal_colors, ansi_color_bright_colors, &
                              ansi_color_apply_face_function, ansi_color_face_procedure, ansi_color_face_type, &
                              ansi_color_face_ref_type, &
                              ansi_color_compilation_filter, &
                              ansi_color_process_output
    use minibuf_mod, only: read_hide_char
    use shell_mod, only: shell, shell_mode, shell_dumb_shell_regexp, shell_prompt_regexp, shell_delimiter_arguments, &
                         shell_command_regexp, shell_command_separator_regexp, shell_popd_regexp, shell_pushd_regexp, &
                         shell_pushd_tohome, shell_pushd_dextract, shell_pushd_dunique, shell_cd_regexp, &
                         shell_dirtrack_verbose, explicit_shell_file_name, explicit_csh_args, explicit_bash_args, &
                         shell_has_auto_cd, shell_get_old_input_include_continuation_lines, shell_close_buffer_on_exit, &
                         shell_history_file_name, shell_dirstack_query, shell_mode_hook, shell_command_mode_hook, &
                         shell_dirtrack_mode_hook, shell_mode_map, shell_command_mode_map, shell_dirtrack_mode
    use simple_mod, only: async_shell_command, async_shell_command_buffer, async_shell_command_display_buffer, &
                          async_shell_command_width, async_shell_command_mode, ASYNC_SHELL_COMMAND_BUFFER_CONFIRM_STOP_PROCESS, &
                          ASYNC_SHELL_COMMAND_BUFFER_CONFIRM_NEW_BUFFER, ASYNC_SHELL_COMMAND_BUFFER_NEW_BUFFER, &
                          ASYNC_SHELL_COMMAND_BUFFER_CONFIRM_RENAME_BUFFER, ASYNC_SHELL_COMMAND_BUFFER_RENAME_BUFFER, &
                          next_error, next_error_hook, next_error_verbose, next_error_last_buffer
    ! Abbrevs and dynamic abbrevs.
    use abbrev_mod, only: abbrev_table_type, abbrev_type, abbrev_ref_type, abbrev_table_ref_type, abbrev_hook_procedure, &
                          abbrev_enable_procedure, abbrev_expand_procedure, global_abbrev_table, fundamental_mode_abbrev_table, &
                          abbrev_tables, abbrev_mode, only_global_abbrevs, abbrev_all_caps, abbrev_file_name, &
                          abbrevs_changed, abbrev_start_location, last_abbrev, last_abbrev_text, last_abbrev_location, &
                          abbrev_expand_function, save_abbrevs, SAVE_ABBREVS_NO, SAVE_ABBREVS_ASK, &
                          SAVE_ABBREVS_SILENTLY, edit_abbrevs_mode_hook, edit_abbrevs_mode_map, &
                          edit_abbrevs_mode_abbrev_table, clear_all_abbrevs, copy_abbrev_table, insert_abbrevs, &
                          list_abbrevs, abbrev_table_name, edit_abbrevs, write_abbrev_file, inverse_add_mode_abbrev, &
                          inverse_add_global_abbrev, inverse_add_abbrev, abbrev_prefix_mark, expand_region_abbrevs, &
                          define_global_abbrev, define_mode_abbrev, abbrev_name, abbrev_expansion, abbrev_insert, &
                          expand_abbrev, abbrev_default_expand, unexpand_abbrev, insert_abbrev_table_description, &
                          local_abbrev_table, abbrev_table_set_local
    use text_mode_mod, only: text_mode_abbrev_table
    use prog_mode_mod, only: prog_mode_abbrev_table
    use simple_mod, only: special_mode_abbrev_table
    use help_mode_mod, only: help_mode_abbrev_table
    use files_mod, only: save_some_buffers_procedure, save_some_buffers_function_type, save_some_buffers_functions, &
                         append_save_some_buffers_function
    use dabbrev_mod, only: dabbrev_expand, is_dabbrev_same_major_mode, dabbrev_select_buffers, dabbrev_friend_buffer_procedure, &
                           dabbrev_select_buffers_procedure, dabbrev_backward_only, dabbrev_limit, &
                           dabbrev_abbrev_skip_leading_regexp, dabbrev_eliminate_newlines, dabbrev_case_fold_search, &
                           dabbrev_upcase_means_case_search, dabbrev_case_distinction, dabbrev_case_replace, &
                           dabbrev_abbrev_char_regexp, dabbrev_check_all_buffers, dabbrev_ignored_buffer_names, &
                           dabbrev_ignored_buffer_regexps, dabbrev_ignored_buffer_modes, dabbrev_check_other_buffers, &
                           dabbrev_select_buffers_function, dabbrev_friend_buffer_function, &
                           dabbrev_search_these_buffers_only, DABBREV_NO, DABBREV_YES, DABBREV_LIKE_CASE_FOLD_SEARCH, &
                           DABBREV_LIKE_CASE_REPLACE, DABBREV_ASK
    use coding_mod, only: buffer_file_coding_system, coding_system_for_read, coding_system_for_write, &
                          last_coding_system_used, inhibit_eol_conversion, inhibit_null_byte_detection, &
                          inhibit_iso_escape_detection, eol_mnemonic_unix, eol_mnemonic_dos, eol_mnemonic_mac, &
                          eol_mnemonic_undecided, default_process_coding_system, file_coding_systems, &
                          file_coding_type, locale_coding_system, is_coding_system, check_coding_system, &
                          coding_system_base, coding_system_eol_type, coding_system_change_eol_conversion, &
                          coding_system_change_text_conversion, coding_system_type, coding_system_priority_list, &
                          set_coding_system_priority, coding_system_list_entry_type, define_coding_system_alias, &
                          decode_coding_string, encode_coding_string, decode_coding_region, encode_coding_region, &
                          detect_coding_string, detect_coding_region, EOL_UNIX, EOL_DOS, EOL_MAC, EOL_UNDECIDED
    use mule_mod, only: auto_codings, auto_coding_regexps, auto_coding_type, prefer_coding_system, &
                        set_default_coding_systems, set_buffer_file_coding_system, describe_coding_system
    use process_mod, only: set_process_coding_system, process_coding_system
    use callproc_mod, only: shell_file_name
    use simple_mod, only: shell_command, shell_command_on_region, read_shell_command, display_message_or_buffer, &
                          shell_command_switch, shell_command_default_error_buffer, shell_command_prompt_show_cwd, &
                          shell_command_keep_output, shell_command_buffer_name, SHELL_COMMAND_KEEP_OUTPUT_NO, &
                          SHELL_COMMAND_KEEP_OUTPUT_ERASE, &
                          SHELL_COMMAND_KEEP_OUTPUT_BEGINNING_OF_LAST_OUTPUT, SHELL_COMMAND_KEEP_OUTPUT_END_OF_LAST_OUTPUT, &
                          SHELL_COMMAND_KEEP_OUTPUT_SAVE_POINT
    use help_fns_mod, only: describe_mode, describe_name
    use help_mod, only: describe_bindings, view_lossage, view_echo_area_messages
    use xdisp_mod, only: message_log_max, MESSAGE_LOG_MAX_NO, MESSAGE_LOG_MAX_UNLIMITED
    use simple_mod, only: messages_buffer_mode_hook, messages_buffer_mode_map
    use apropos_mod, only: apropos_mode_hook, apropos_mode_map
    use cmds_mod, only: post_self_insert_hook
    use timer_mod, only: execute_at_time, execute_with_timer, execute_with_idle_timer, cancel_timer, destroy_timer, is_timer, &
                         timer_max_repeats
    use autorevert_mod, only: auto_revert_mode, auto_revert_tail_mode, global_auto_revert_mode, &
                              turn_on_auto_revert_mode, turn_on_auto_revert_tail_mode, auto_revert_interval, &
                              auto_revert_stop_on_user_input, auto_revert_verbose, auto_revert_mode_text, &
                              auto_revert_tail_mode_text, auto_revert_mode_hook, auto_revert_tail_mode_hook, &
                              global_auto_revert_mode_text, global_auto_revert_mode_hook, &
                              global_auto_revert_non_file_buffers, global_auto_revert_ignore_modes, &
                              auto_revert_check_vc_info, global_auto_revert_ignore_buffer, auto_revert_remote_files, &
                              auto_revert_use_notify, auto_revert_notify_exclude_dir_regexp, auto_revert_avoid_polling
    use savehist_mod, only: savehist_mode, savehist_save, savehist_save_minibuffer_history, savehist_variable_type, &
                            savehist_additional_variables, savehist_ignored_variables, savehist_file, &
                            savehist_file_modes, savehist_autosave_interval, savehist_mode_hook, savehist_save_hook
    use saveplace_mod, only: save_place_mode, save_place_local_mode, save_place_file, save_place_limit, &
                             SAVE_PLACE_LIMIT_UNLIMITED, save_place_forget_unreadable_files_option, &
                             save_place_abbreviate_file_names, save_place_save_skipped, save_place_skip_check_regexp, &
                             save_place_ignore_files_regexp, save_place_autosave_interval, &
                             save_place_after_find_file_hook, save_place_mode_hook, save_place_local_mode_hook, &
                             save_place_version_control, SAVE_PLACE_VERSION_CONTROL_NUMBERED, SAVE_PLACE_VERSION_CONTROL_EXISTING, &
                             SAVE_PLACE_VERSION_CONTROL_NO, SAVE_PLACE_VERSION_CONTROL_NOSPECIAL
    use recentf_mod, only: recentf_mode, recentf_open, recentf_open_most_recent_file, recentf_save_list, recentf_cleanup, &
                           recentf_file_type, recentf_files, recentf_check_type, recentf_filename_handler_type, &
                           RECENTF_MAX_SAVED_ITEMS_UNLIMITED, recentf_max_saved_items, recentf_save_file, recentf_save_file_modes, &
                           recentf_exclude, recentf_exclude_ignored_extensions, recentf_keep, recentf_menu_action, &
                           RECENTF_AUTO_CLEANUP_MODE, RECENTF_AUTO_CLEANUP_NO, recentf_auto_cleanup, &
                           recentf_initialize_file_name_history, recentf_filename_handlers, recentf_show_messages, &
                           recentf_autosave_interval, recentf_mode_hook, recentf_mode_map, recentf_keep_default_test, &
                           abbreviate_file_name_handler, file_truename_handler
    use bookmark_mod, only: bookmark_type, bookmark_inhibit_context_function_type, bookmark_use_annotations, &
                            bookmark_save_flag, BOOKMARK_SAVE_FLAG_NO, BOOKMARK_SAVE_FLAG_ON_EXIT, bookmark_default_file, &
                            bookmark_watch_bookmark_file, BOOKMARK_WATCH_BOOKMARK_FILE_NO, &
                            BOOKMARK_WATCH_BOOKMARK_FILE_ASK, BOOKMARK_WATCH_BOOKMARK_FILE_SILENT, &
                            bookmark_version_control, BOOKMARK_VERSION_CONTROL_NUMBERED, BOOKMARK_VERSION_CONTROL_EXISTING, &
                            BOOKMARK_VERSION_CONTROL_NO, BOOKMARK_VERSION_CONTROL_NOSPECIAL, &
                            bookmark_completion_ignore_case, bookmark_sort_flag, BOOKMARK_SORT_FLAG_CREATED, &
                            BOOKMARK_SORT_FLAG_ALPHABETICAL, BOOKMARK_SORT_FLAG_LAST_MODIFIED, bookmark_menu_confirm_deletion, &
                            bookmark_automatically_show_annotations, bookmark_fringe_mark, bookmark_map, &
                            bookmark_search_size, bookmark_make_record_function, bookmark_inhibit_context_functions, &
                            bookmark_edit_annotation_text_func, bookmark_edit_annotation_mode_map, &
                            bookmark_edit_annotation_mode_hook, bookmark_after_jump_hook, &
                            bookmark_after_load_file_hook, bookmark_exit_hook, bookmark_bmenu_buffer, &
                            bookmark_bmenu_file_column, bookmark_bmenu_type_column_width, &
                            bookmark_bmenu_toggle_filenames, bookmark_search_delay, bookmark_bmenu_mode_map, &
                            bookmark_bmenu_mode_hook
    use tabulated_list_mod, only: tabulated_list_gui_sort_indicator_asc, tabulated_list_gui_sort_indicator_desc, &
                                  tabulated_list_tty_sort_indicator_asc, tabulated_list_tty_sort_indicator_desc, &
                                  tabulated_list_mode_map, tabulated_list_mode_hook, tabulated_list_revert_hook
    use mule_util_mod, only: truncate_string_ellipsis_variable
    use info_mod, only: info_history_skip_intermediate_nodes, info_default_directories, info_directories, &
                        info_additional_directories, info_scroll_prefer_subnodes, info_mode_hook, &
                        info_selection_hook, info_url_table, info_url_type, info_suffixes, info_suffix_type, &
                        info_streamline_headings, info_heading_type, info_mode_map, info_mode_syntax_table, &
                        info_fontify_maximum_menu_size, INFO_FONTIFY_MAXIMUM_MENU_SIZE_NONE, &
                        INFO_FONTIFY_MAXIMUM_MENU_SIZE_UNLIMITED, info_fontify_visited_nodes, info_use_header_line, &
                        info_isearch_search, &
                        info_hide_note_references, INFO_HIDE_NOTE_REFERENCES_NO, INFO_HIDE_NOTE_REFERENCES_SEE, &
                        INFO_HIDE_NOTE_REFERENCES_HIDE, INFO_HIDE_NOTE_REFERENCES_TAG, info_breadcrumbs_depth, &
                        info_search_whitespace_regexp, info_warn_on_index_alternatives_wrap, &
                        info_files_for_emacs, info_file_prefix_type
    use info_look_mod, only: info_lookup_other_window_flag, info_lookup_highlight_face, info_lookup_file_names, &
                             info_lookup_file_name_type, info_lookup_add_help, info_lookup_maybe_add_help, &
                             info_lookup_doc_specs_type, info_lookup_trans_procedure, info_lookup_parse_procedure, &
                             info_lookup_doc_spec_procedure
    use ibuffer_mod, only: ibuffer_formats, ibuffer_always_compile_formats, ibuffer_fontifications, &
                           ibuffer_human_readable_size, ibuffer_use_other_window, &
                           ibuffer_default_shrink_to_minimum_size, ibuffer_display_summary, ibuffer_truncate_lines, &
                           ibuffer_case_fold_search, ibuffer_default_sorting_mode, ibuffer_default_sorting_reversep, &
                           ibuffer_eliding_string, ibuffer_maybe_show_tests, &
                           ibuffer_default_display_maybe_show_tests, ibuffer_movement_cycle, &
                           ibuffer_modified_char, ibuffer_read_only_char, ibuffer_marked_char, ibuffer_locked_char, &
                           ibuffer_deletion_char, ibuffer_expert, ibuffer_view_ibuffer, &
                           ibuffer_always_show_last_buffer, IBUFFER_ALWAYS_SHOW_LAST_BUFFER_NO, &
                           IBUFFER_ALWAYS_SHOW_LAST_BUFFER_YES, &
                           IBUFFER_ALWAYS_SHOW_LAST_BUFFER_NOMINI, ibuffer_jump_offer_only_visible_buffers, &
                           ibuffer_use_header_line, &
                           IBUFFER_USE_HEADER_LINE_NO, IBUFFER_USE_HEADER_LINE_FILTERS, IBUFFER_USE_HEADER_LINE_TITLE, &
                           ibuffer_default_directory, ibuffer_help_buffer_modes, ibuffer_compressed_file_name_regexp, &
                           ibuffer_hook, ibuffer_mode_hook, ibuffer_marked_face, ibuffer_deletion_face, &
                           ibuffer_title_face, ibuffer_filter_group_name_face, ibuffer_directory_abbrevs, &
                           ibuffer_never_show_tests, ibuffer_always_show_tests, &
                           ibuffer_never_search_content_name, ibuffer_never_search_content_mode, ibuffer_old_time, &
                           ibuffer_show_empty_filter_groups, ibuffer_mode_map, ibuffer_mode_filter_group_map, &
                           ibuffer_columns, ibuffer_sorting_functions, define_ibuffer_column, define_ibuffer_sorter, &
                           ibuffer_column, ibuffer_text, ibuffer_saved_filters, ibuffer_saved_filter_groups, &
                           ibuffer_filterings, ibuffer_filter_formats, ibuffer_save_with_custom, &
                           define_ibuffer_filter, ibuffer_filtering_qualifiers, ibuffer_set_filtering_qualifiers, &
                           ibuffer_filter_groups, ibuffer_set_filter_groups, ibuffer_switch_to_saved_filter_groups, &
                           ibuffer_switch_to_saved_filters, is_ibuffer_included_in_filters
    use ibuf_macs_mod, only: ibuffer_formats_type, ibuffer_column_spec_type, ibuffer_cell_type, ibuffer_column_procedure, &
                             ibuffer_summarizer_procedure, ibuffer_sorter_procedure, ibuffer_size_procedure, &
                             ibuffer_buffer_test_procedure, ibuffer_form_procedure, ibuffer_show_test_type, &
                             ibuffer_fontification_type, ibuffer_directory_abbrev_type, IBUFFER_ALIGN_LEFT, &
                             IBUFFER_ALIGN_CENTER, IBUFFER_ALIGN_RIGHT, ibuffer_filter_type, ibuffer_filter_node_type, &
                             ibuffer_saved_filters_type, ibuffer_saved_filter_groups_type, ibuffer_filter_groups_type, &
                             ibuffer_filter_formats_type, ibuffer_filter_def_type, ibuffer_filter_test_procedure, ibuffer_and, &
                             ibuffer_or, ibuffer_not, ibuffer_saved, ibuffer_filter, ibuffer_major_mode, ibuffer_used_mode, &
                             ibuffer_derived_mode, ibuffer_name, ibuffer_process, ibuffer_starred_name, ibuffer_filename, &
                             ibuffer_basename, ibuffer_file_extension, ibuffer_directory, ibuffer_size_gt, ibuffer_size_lt, &
                             ibuffer_modified, ibuffer_visiting_file, ibuffer_content, ibuffer_test
    use crm_mod, only: completing_read_multiple, crm_separator, crm_prompt, crm_local_completion_map, &
                       crm_local_must_match_map
    use buff_menu_mod, only: buffer_menu_marker_char, buffer_menu_del_char, buffer_menu_use_header_line, &
                             buffer_menu_name_width, buffer_menu_name_width_function, buffer_menu_name_width_procedure, &
                             buffer_menu_dynamic_name_width, buffer_menu_size_width, buffer_menu_mode_width, &
                             buffer_menu_use_frame_buffer_list, buffer_menu_human_readable_sizes, &
                             buffer_menu_mode_hook, buffer_menu_mode_map
    use repeat_mod, only: repeat_mode, repeat_mode_hook, repeat_too_dangerous, repeat_on_final_keystroke, &
                          repeat_exit_key, repeat_exit_timeout, repeat_keep_prefix, repeat_check_key, &
                          repeat_echo_function, repeat_echo_procedure, repeat_echo_message, repeat_exit
    use value_mod, only: REPEAT_CHECK_KEY_DEFAULT, REPEAT_CHECK_KEY_YES, REPEAT_CHECK_KEY_NO
    use keymap_mod, only: set_repeat_map
    use comint_mod, only: comint_repeat_map
    use shell_mod, only: shell_repeat_map
    use files_mod, only: buffer_stale_function, buffer_stale_procedure, BUFFER_STALE_NO, BUFFER_STALE_YES, &
                         BUFFER_STALE_FAST, buffer_auto_revert_by_notification, mounted_file_systems, &
                         after_set_visited_file_name_hook
    use paren_mod, only: show_paren_mode, show_paren_local_mode, show_paren_style, SHOW_PAREN_STYLE_PARENTHESIS, &
                         SHOW_PAREN_STYLE_EXPRESSION, SHOW_PAREN_STYLE_MIXED, show_paren_delay, show_paren_priority, &
                         show_paren_ring_bell_on_mismatch, show_paren_when_point_inside_paren, &
                         show_paren_when_point_in_periphery, show_paren_highlight_openparen, &
                         show_paren_context_when_offscreen, show_paren_not_in_comments_or_strings, &
                         SHOW_PAREN_NOT_IN_COMMENTS_OR_STRINGS_NO, SHOW_PAREN_NOT_IN_COMMENTS_OR_STRINGS_ALL, &
                         SHOW_PAREN_NOT_IN_COMMENTS_OR_STRINGS_ON_MISMATCH, show_paren_data_function, show_paren_data_type, &
                         show_paren_data_procedure, show_paren_default, show_paren_test_function, show_paren_test_procedure, &
                         show_paren_default_test, show_paren_mode_hook, show_paren_local_mode_hook
    use simple_mod, only: blink_matching_paren, BLINK_MATCHING_PAREN_NO, BLINK_MATCHING_PAREN_YES, &
                          BLINK_MATCHING_PAREN_JUMP, BLINK_MATCHING_PAREN_JUMP_OFFSCREEN, blink_matching_paren_on_screen, &
                          blink_matching_paren_distance, blink_matching_delay, blink_matching_paren_dont_ignore_comments, &
                          blink_matching_check_function, blink_matching_check_procedure, blink_matching_check_mismatch, &
                          blink_paren_function, blink_matching_open, newline_and_indent, backward_delete_char_untabify, &
                          backward_delete_char_untabify_method, BACKWARD_DELETE_CHAR_UNTABIFY_METHOD_NONE, &
                          BACKWARD_DELETE_CHAR_UNTABIFY_METHOD_UNTABIFY, &
                          BACKWARD_DELETE_CHAR_UNTABIFY_METHOD_HUNGRY, BACKWARD_DELETE_CHAR_UNTABIFY_METHOD_ALL
    use electric_mod, only: electric_indent_mode, electric_indent_local_mode, electric_indent_chars, &
                            electric_indent_inhibit, electric_indent_functions_without_reindent, &
                            electric_indent_mode_hook, electric_indent_local_mode_hook, electric_indent_just_newline, &
                            electric_newline_and_maybe_indent
    use elec_pair_mod, only: electric_pair_mode, electric_pair_local_mode, electric_pair_type, electric_pair_pairs, &
                             electric_pair_text_pairs, electric_pair_skip_self, electric_pair_inhibit_test_function, &
                             electric_pair_char_procedure, electric_pair_default_skip_self, electric_pair_default_inhibit, &
                             electric_pair_conservative_inhibit, electric_pair_preserve_balance, &
                             electric_pair_open_newline_between_pairs, electric_pair_skip_whitespace, &
                             ELECTRIC_PAIR_SKIP_WHITESPACE_NO, ELECTRIC_PAIR_SKIP_WHITESPACE_YES, &
                             ELECTRIC_PAIR_SKIP_WHITESPACE_CHOMP, electric_pair_skip_whitespace_chars, &
                             electric_pair_skip_whitespace_function, electric_pair_text_syntax_table, &
                             electric_pair_string_bound_function, electric_pair_position_procedure, electric_pair_mode_hook, &
                             electric_pair_local_mode_hook, electric_pair_delete_pair
    use newcomment_mod, only: comment_use_syntax, comment_fill_column, comment_column, comment_start, comment_start_skip, &
                              comment_end_skip, comment_end, comment_indent_function, comment_indent_type, &
                              comment_insert_comment_function, comment_region_function, uncomment_region_function, &
                              comment_quote_nested, comment_quote_nested_function, comment_continue, comment_add, &
                              comment_style, comment_padding, comment_inline_offset, comment_multi_line, &
                              comment_empty_lines, comment_setup_function, comment_auto_fill_only_comments, &
                              COMMENT_USE_SYNTAX_NO, COMMENT_USE_SYNTAX_YES, COMMENT_USE_SYNTAX_UNDECIDED, &
                              COMMENT_STYLE_PLAIN, COMMENT_STYLE_INDENT_OR_TRIPLE, COMMENT_STYLE_INDENT, &
                              COMMENT_STYLE_ALIGNED, COMMENT_STYLE_BOX, COMMENT_STYLE_EXTRA_LINE, COMMENT_STYLE_MULTI_LINE, &
                              COMMENT_STYLE_BOX_MULTI, COMMENT_EMPTY_LINES_NO, COMMENT_EMPTY_LINES_YES, &
                              COMMENT_EMPTY_LINES_EOL, COMMENT_INDENT_NONE, COMMENT_INDENT_COLUMN, COMMENT_INDENT_RANGE
    use conf_mode_mod, only: conf_assignment_column, conf_javaprop_assignment_column, conf_colon_assignment_column, &
                             conf_assignment_space, conf_colon_assignment_space, conf_mode_map, conf_mode_syntax_table, &
                             conf_unix_mode_syntax_table, conf_space_keywords_by_file, conf_space_keywords_entry_type, &
                             conf_mode_hook, conf_unix_mode_hook, conf_windows_mode_hook, conf_javaprop_mode_hook, &
                             conf_space_mode_hook, conf_colon_mode_hook, conf_ppd_mode_hook, conf_xdefaults_mode_hook, &
                             conf_toml_mode_hook, conf_desktop_mode_hook, conf_npmrc_mode_hook
    use expression_motion_mod, only: subprogram_prompt_regexp, parens_require_spaces, end_of_subprogram_moves_to_eol, &
                                   narrow_to_subprogram_include_comments, insert_pairs, insert_pair_type, delete_pair_blink_delay, &
                                     delete_pair_push_mark, beginning_of_subprogram_function, end_of_subprogram_function
    use simple_mod, only: widen_automatically
    use make_mode_mod, only: makefile_target_colon, makefile_macro_assign, makefile_electric_keys, &
                             makefile_use_curly_braces_for_macros, makefile_tab_after_target_colon, makefile_backslash_column, &
                             makefile_backslash_align, makefile_pickup_everything_picks_up_filenames, &
                             makefile_cleanup_continuations, makefile_special_targets, makefile_runtime_macros, &
                             makefile_dependency_regexp, makefile_dependency_skip, makefile_rule_action_regexp, &
                             makefile_brave_make, makefile_query_one_target_method_function, makefile_query_procedure, &
                             makefile_up_to_date_buffer_name, makefile_gnumake_functions, &
                             makefile_gnumake_function_type, makefile_mode_map, makefile_mode_syntax_table, &
                             makefile_imake_mode_syntax_table, makefile_mode_abbrev_table, makefile_mode_hook, &
                             makefile_automake_mode_hook, makefile_gmake_mode_hook, makefile_makepp_mode_hook, &
                             makefile_bsdmake_mode_hook, makefile_imake_mode_hook
    use f90_mod, only: f90_do_indent, f90_if_indent, f90_type_indent, f90_program_indent, f90_associate_indent, &
                       f90_critical_indent, f90_continuation_indent, f90_comment_region, f90_indented_comment_regexp, &
                       f90_directive_comment_regexp, f90_beginning_ampersand, f90_smart_end, f90_smart_end_names, &
                       f90_break_delimiters, f90_break_before_delimiters, f90_auto_keyword_case, f90_leave_line_no, &
                       f90_mode_hook, f90_mode_map, f90_mode_syntax_table, f90_mode_abbrev_table, F90_SMART_END_NO, &
                       F90_SMART_END_BLINK, F90_SMART_END_NO_BLINK, F90_AUTO_KEYWORD_CASE_NONE, &
                       F90_AUTO_KEYWORD_CASE_DOWNCASE_WORD, &
                       F90_AUTO_KEYWORD_CASE_UPCASE_WORD, F90_AUTO_KEYWORD_CASE_CAPITALIZE_WORD
    use smie_mod, only: smie_indent_basic, smie_blink_matching_inners, smie_indent_functions
    use sh_script_mod, only: sh_ancestors, sh_aliases, sh_ancestor_type, sh_alias_type, sh_shell_file, sh_shell, &
                             sh_shell_arg, sh_assignment_regexp, sh_remember_variable_min, sh_beginning_of_command, &
                             sh_end_of_command, sh_here_document_word, sh_builtins, sh_indent_statement_after_and, &
                             sh_leading_keywords, sh_other_keywords, sh_variables, sh_font_lock_keywords_var, &
                             sh_font_lock_keywords_var_1, sh_font_lock_keywords_var_2, sh_set_shell_hook, sh_mode_hook, &
                             sh_base_mode_hook, sh_basic_offset, sh_indent_for_fi, sh_indent_for_done, sh_indent_after_else, &
                             sh_indent_after_if, sh_indent_for_then, sh_indent_for_do, sh_indent_after_do, &
                             sh_indent_after_loop_construct, sh_indent_after_done, sh_indent_for_case_label, &
                             sh_indent_for_case_alt, sh_indent_for_continuation, sh_indent_after_open, &
                             sh_indent_after_function, sh_indent_after_switch, sh_indent_after_case, sh_backslash_column, &
                             sh_backslash_align, sh_indent_supported, sh_make_vars_local, sh_indent_after_continuation, &
                             sh_mode_map, sh_base_mode_map, sh_mode_syntax_table, sh_mode_abbrev_table, &
                             sh_strings_feature_type, sh_keywords_feature_type, sh_string_feature_type, sh_string_type, &
                             SH_PLUS, SH_MINUS, SH_PLUS_PLUS, SH_MINUS_MINUS, SH_STAR, SH_SLASH, SH_INDENT_AFTER_CONTINUATION_NO, &
                             SH_INDENT_AFTER_CONTINUATION_DEEPER, SH_INDENT_AFTER_CONTINUATION_ALWAYS, &
                             sh_dynamic_complete_functions, sh_styles, &
                             sh_style_type, sh_vars
    use python_mod, only: python_2_support, python_indent_offset, python_indent_guess_indent_offset_variable, &
                          python_indent_guess_indent_offset_verbose, python_indent_trigger_commands, &
                          python_indent_def_block_scale, python_indent_block_paren_deeper, python_forward_expression_function, &
                          python_fill_docstring_style, python_fill_comment_function, python_fill_string_function, &
                          python_fill_decorator_function, python_fill_paren_function, python_base_mode_map, &
                          python_mode_map, python_indent_repeat_map, python_mode_syntax_table, python_dotty_syntax_table, &
                          python_base_mode_hook, python_mode_hook, python_font_lock_keywords_level_1, &
                          python_font_lock_keywords_level_2, python_font_lock_keywords_maximum_decoration, &
                          PYTHON_FILL_DOCSTRING_STYLE_NONE, PYTHON_FILL_DOCSTRING_STYLE_DJANGO, &
                          PYTHON_FILL_DOCSTRING_STYLE_ONETWO, PYTHON_FILL_DOCSTRING_STYLE_PEP_257, &
                          PYTHON_FILL_DOCSTRING_STYLE_PEP_257_NN, PYTHON_FILL_DOCSTRING_STYLE_SYMMETRIC
    use executable_mod, only: executable_insert, executable_query, executable_magicless_file_regexp, executable_prefix, &
                              executable_prefix_env, executable_chmod, executable_self_display, &
                              executable_font_lock_keywords, executable_make_buffer_file_executable_if_script, &
                              EXECUTABLE_QUERY_NO, EXECUTABLE_QUERY_FUNCTION, EXECUTABLE_QUERY_ALWAYS, EXECUTABLE_CHMOD_NONE
    use files_mod, only: before_save_hook, after_save_hook, write_file_functions
    use fill_mod, only: fill_paragraph_function, fill_paragraph_handle_comment, adaptive_fill_function
    use sgml_mode_mod, only: sgml_basic_offset, sgml_attribute_offset, sgml_xml_mode, sgml_specials, &
                             sgml_validate_command, sgml_slash_distance, sgml_char_names, sgml_empty_tags, &
                             sgml_unclosed_tags, sgml_whitespace_sensitive_tags, html_tag_faces, sgml_tag_face_type, &
                             sgml_mode_hook, html_mode_hook, sgml_mode_map, html_mode_map, sgml_mode_syntax_table, &
                             sgml_tag_syntax_table
    use js_mod, only: js_indent_level, js_expr_indent_offset, js_paren_indent_offset, js_square_indent_offset, &
                      js_curly_indent_offset, js_switch_indent_offset, js_flat_functions, &
                      js_indent_align_list_continuation, js_comment_lineup_func, js_enabled_frameworks, &
                      js_indent_first_init, js_chain_indent, js_base_mode_hook, js_mode_hook, js_json_mode_hook, &
                      js_mode_map, js_base_mode_map, js_json_mode_map, js_mode_syntax_table, JS_INDENT_FIRST_INIT_NO, &
                      JS_INDENT_FIRST_INIT_YES, JS_INDENT_FIRST_INIT_DYNAMIC
    use css_mode_mod, only: css_electric_keys, css_fontify_colors, css_indent_offset, css_lookup_url_format, &
                            css_class_list_function, css_id_list_function, css_base_mode_hook, css_mode_hook, &
                            scss_mode_hook, css_mode_map, css_base_mode_map, scss_mode_map, css_mode_syntax_table, &
                            scss_mode_syntax_table
    implicit none(type, external)
    private

    public :: int64
    public :: process_arg_type, process_filter_procedure, process_sentinel_procedure, make_process, start_process, &
              delete_process, destroy_process, is_process, is_process_live, get_process, get_buffer_process, process_list, &
              process_status, process_exit_status, process_id, process_name, process_command, process_buffer, &
              set_process_buffer, process_mark, set_process_filter, set_process_sentinel, process_tty_name, &
              set_process_query_on_exit_flag, process_query_on_exit_flag, interrupt_process, stop_process, &
              quit_process, signal_process, process_send_string, process_send_region, process_send_eof, &
              accept_process_output, internal_default_process_filter, internal_default_process_sentinel, &
              process_connection_type, delete_exited_processes, PROCESS_CONNECTION_DEFAULT, PROCESS_CONNECTION_PIPE, &
              PROCESS_CONNECTION_PTY, shell_file_name, shell_command, shell_command_on_region, read_shell_command, &
              display_message_or_buffer, shell_command_switch, shell_command_default_error_buffer, &
              shell_command_prompt_show_cwd, shell_command_keep_output, shell_command_buffer_name, &
              SHELL_COMMAND_KEEP_OUTPUT_NO, SHELL_COMMAND_KEEP_OUTPUT_ERASE, &
              SHELL_COMMAND_KEEP_OUTPUT_BEGINNING_OF_LAST_OUTPUT, SHELL_COMMAND_KEEP_OUTPUT_END_OF_LAST_OUTPUT, &
              SHELL_COMMAND_KEEP_OUTPUT_SAVE_POINT, &
              num_processors, compile, recompile, compilation_start, compilation_mode, stop_compilation, compile_command, &
              compilation_read_command, compilation_ask_about_save, compilation_always_stop, compilation_disable_input, &
              compilation_scroll_output, COMPILATION_SCROLL_OUTPUT_NO, COMPILATION_SCROLL_OUTPUT_YES, &
              COMPILATION_SCROLL_OUTPUT_FIRST_ERROR, &
              compilation_filter_hook, compilation_filter_start, compilation_mode_hook, compilation_mode_map, &
              compilation_directory, comint_inhibit_carriage_motion
    public :: compilation_rule_type, compilation_highlight_type, compilation_directory_matcher_type, &
              compilation_transform_type, compilation_number_procedure, compilation_file_procedure, &
              compilation_face_procedure, compilation_minor_mode_hook, compilation_shell_minor_mode_hook, &
              compilation_minor_mode_map, compilation_shell_minor_mode_map, compilation_error_regexps, &
              compilation_error_regexp_names, compilation_error_case_fold_search, compilation_directory_matcher, &
              compilation_transform_file_matches, compilation_error_screen_columns, compilation_first_column, &
              compilation_skip_threshold, compilation_skip_visited, compilation_skip_to_next_location, &
              compilation_search_all_directories, compilation_context_lines, COMPILATION_CONTEXT_TOP, &
              COMPILATION_CONTEXT_LINES_NO_SCROLL, compilation_message_face, compilation_error_face, compilation_warning_face, &
              compilation_info_face, compilation_line_face, compilation_column_face, compilation_enter_directory_face, &
              compilation_leave_directory_face, &
              compilation_minor_mode, &
              compilation_shell_minor_mode, compile_goto_error, compilation_next_error, compilation_previous_error, &
              compilation_next_file, compilation_previous_file
    public :: comint_mode, make_comint, make_comint_in_buffer, comint_execute, comint_exec, comint_send_input, &
              comint_send_string, comint_prompt_regexp, comint_prompt_read_only, comint_highlight_input, &
              comint_input_ignoredups, comint_input_ring_file_name, comint_pager, comint_input_ring_file_prefix, &
              comint_move_point_for_output, comint_move_point_for_matching_input, COMINT_MOVE_POINT_FOR_OUTPUT_NO, &
              COMINT_MOVE_POINT_FOR_OUTPUT_YES, &
              COMINT_MOVE_POINT_FOR_OUTPUT_ALL, COMINT_MOVE_POINT_FOR_OUTPUT_THIS, COMINT_MOVE_POINT_FOR_OUTPUT_OTHERS, &
              COMINT_MOVE_POINT_FOR_MATCHING_INPUT_AFTER_INPUT, &
              COMINT_MOVE_POINT_FOR_MATCHING_INPUT_END_OF_LINE, comint_scroll_show_maximum_output, comint_buffer_maximum_size, &
              comint_input_ring_size, comint_input_ring_separator, comint_input_history_ignore, comint_process_echoes, &
              comint_eol_on_send, comint_use_prompt_regexp, comint_terminfo_terminal, comint_delimiter_arguments, &
              comint_mode_hook, comint_exec_hook, comint_output_filter_functions, comint_preoutput_filter_functions, &
              comint_input_filter_functions, comint_mode_map, &
              comint_truncate_buffer, comint_strip_ctrl_m, comint_postoutput_scroll_to_bottom, &
              comint_show_maximum_output, comint_input_autoexpand, COMINT_AUTOEXPAND_NO, COMINT_AUTOEXPAND_ON_COMPLETION, &
              COMINT_AUTOEXPAND_INPUT, COMINT_AUTOEXPAND_HISTORY, comint_scroll_to_bottom_on_input, &
              COMINT_SCROLL_TO_BOTTOM_ON_INPUT_NO, COMINT_SCROLL_TO_BOTTOM_ON_INPUT_YES, COMINT_SCROLL_TO_BOTTOM_ON_INPUT_ALL, &
              COMINT_SCROLL_TO_BOTTOM_ON_INPUT_THIS, comint_insert_previous_argument_from_end, comint_history_isearch, &
              COMINT_HISTORY_ISEARCH_NO, COMINT_HISTORY_ISEARCH_DWIM, COMINT_HISTORY_ISEARCH_ALWAYS, &
              comint_password_prompt_regexp, comint_password_prompt_max_length
    public :: password_word_equivalents, password_colon_equivalents, read_hide_char
    public :: ansi_color_for_comint_mode, ansi_color_for_compilation_mode, ANSI_COLOR_NO, ANSI_COLOR_FILTER, &
              ANSI_COLOR_YES, ansi_color_bold_is_bright, ansi_color_basic_faces, ansi_color_normal_colors, &
              ansi_color_bright_colors, ansi_color_apply_face_function, ansi_color_face_procedure, ansi_color_face_type, &
              ansi_color_face_ref_type, &
              ansi_color_compilation_filter, ansi_color_process_output
    public :: shell, shell_mode, shell_dumb_shell_regexp, shell_prompt_regexp, shell_delimiter_arguments, &
              shell_command_regexp, shell_command_separator_regexp, shell_popd_regexp, shell_pushd_regexp, &
              shell_pushd_tohome, shell_pushd_dextract, shell_pushd_dunique, shell_cd_regexp, shell_dirtrack_verbose, &
              explicit_shell_file_name, explicit_csh_args, explicit_bash_args, shell_has_auto_cd, &
              shell_get_old_input_include_continuation_lines, shell_close_buffer_on_exit, shell_history_file_name, &
              shell_dirstack_query, shell_mode_hook, shell_command_mode_hook, shell_dirtrack_mode_hook, shell_mode_map, &
              shell_command_mode_map, shell_dirtrack_mode
    public :: async_shell_command, async_shell_command_buffer, async_shell_command_display_buffer, &
              async_shell_command_width, async_shell_command_mode, ASYNC_SHELL_COMMAND_BUFFER_CONFIRM_STOP_PROCESS, &
              ASYNC_SHELL_COMMAND_BUFFER_CONFIRM_NEW_BUFFER, &
              ASYNC_SHELL_COMMAND_BUFFER_NEW_BUFFER, ASYNC_SHELL_COMMAND_BUFFER_CONFIRM_RENAME_BUFFER, &
              ASYNC_SHELL_COMMAND_BUFFER_RENAME_BUFFER, next_error, next_error_hook, &
              next_error_verbose, next_error_last_buffer
    public :: status_type, set_error, format_error, FM_GENERIC_ERROR
    public :: command_procedure, command_type, commands, set_command_disabled
    public :: faces, face_underline, face_box, FACE_STYLE_LINE, FACE_STYLE_DOUBLE_LINE, FACE_STYLE_WAVE, FACE_STYLE_DOTS, &
              FACE_STYLE_DASHES, FACE_BOX_RELEASED_BUTTON, FACE_BOX_PRESSED_BUTTON, FACE_BOX_FLAT_BUTTON
    public :: keymap_global_set, keymap_set, is_key_valid, current_global_map, global_set_key
    public :: handle_type, current_buffer, get_or_make_buffer, set_buffer, buffer_name
    public :: insert, point, point_min, point_max, goto_char, buffer_substring, delete_region, is_bob, is_eob, is_bol, is_eol
    public :: forward_char, backward_char, forward_line, beginning_of_line, end_of_line
    public :: find_file, save_buffer, read_yes_or_no, read_y_or_n, read_string, message
    public :: search_forward, search_backward, match_beginning, match_end, SEARCH_NOERROR, SEARCH_MOVE, &
              isearch_forward_command, isearch_backward_command
    public :: undo_only, undo_redo, undo_boundary
    public :: region_beginning, region_end, copy_to_clipboard_history, current_clipboard_entry, cut_region, push_mark, &
              set_mark, mark, &
              is_region_active, deactivate_mark
    public :: read_file_name
    public :: forward_word, backward_word, cut_word, backward_cut_word, mark_word, modify_syntax_entry, &
              char_syntax, standard_syntax_table, make_syntax_table, copy_syntax_table, set_syntax_table, &
              syntax_table, forward_paragraph, backward_paragraph, mark_paragraph, cut_paragraph, &
              backward_cut_paragraph, describe_function, describe_variable, substitute_command_keys
    public :: make_char_table, is_char_table, char_table_subtype, char_table_parent, set_char_table_parent, &
              char_table_extra_slot, set_char_table_extra_slot, char_table_extra_table, set_char_table_extra_table, &
              char_table_range, set_char_table_range, map_char_table, optimize_char_table, char_table_map_procedure, NO_VALUE
    public :: is_case_table, current_case_table, standard_case_table, set_case_table, set_standard_case_table
    public :: is_syntax_table_live, internal_describe_syntax_value
    public :: category_set_type, make_category_set, define_category, category_docstring, get_unused_category, &
              is_category_table, category_table, standard_category_table, copy_category_table, make_category_table, &
              set_category_table, char_category_set, category_set_mnemonics, modify_category_entry
    public :: split_window, delete_window, delete_other_windows, other_window, select_window, selected_window, &
              other_buffer, switch_to_buffer, quit_window, close_buffer, read_buffer
    public :: re_search_forward, re_search_backward, posix_search_forward, posix_search_backward, looking_at, &
              posix_looking_at, string_match, posix_string_match, match_data, set_match_data, regexp_quote, &
              match_string, match_string_no_properties, string_matches, is_looking_at, looking_back, &
              isearch_forward_regexp_command, isearch_backward_regexp_command, forward_sentence, backward_sentence, &
              cut_sentence, backward_cut_sentence, mark_end_of_sentence, sentence_end_regexp, replace_match, &
              match_substitute_replacement, upcase_string, downcase_string, capitalize_string, &
              upcase_initials_string, upcase_char, downcase_char, capitalize_char, upcase_initials_char, &
              upcase_region, downcase_region, capitalize_region, upcase_initials_region, upcase_word, downcase_word, &
              capitalize_word, query_replace, query_replace_regexp, map_query_replace_regexp, replace_string, &
              replace_regexp, replacement_procedure, transpose_chars, transpose_words, transpose_lines, &
              transpose_sentences, transpose_paragraphs, delete_horizontal_space, delete_all_space, just_one_space, &
              fixup_whitespace, delete_blank_lines, delete_indentation, skip_chars_forward, skip_chars_backward, &
              skip_syntax_forward, skip_syntax_backward, forward_comment, scan_groups, scan_expressions, &
              parse_partial_expression, parse_state_type, ST_COMMENT_STYLE, ST_STRING_STYLE, COMMENTSTOP_NO, &
              COMMENTSTOP_COMMENT, COMMENTSTOP_SYNTAX_TABLE, forward_expression, backward_expression, forward_group, &
              backward_group, &
              down_group, up_group, backward_up_group, cut_expression, backward_cut_expression, mark_expression, enable_command, &
              disable_command, rmc_choice_type, &
              read_multiple_choice, indent_to, current_indentation, indent_according_to_mode, indent_rigidly, &
              indent_line_to, current_left_margin, move_to_left_margin, indent_region, indent_relative, &
              tab_to_tab_stop, move_to_tab_stop, beginning_of_line_text, indent_line_procedure, indent_region_procedure, &
              fill_paragraph, fill_region, fill_region_as_paragraph, unfill_paragraph, set_fill_prefix, &
              justify_current_line, canonically_space_region, current_fill_column, current_justification, &
              is_fill_single_word_nobreak, is_fill_french_nobreak, is_fill_polish_nobreak, is_fill_single_char_nobreak, &
              fill_nobreak_function_type, DEFAULT_JUSTIFICATION_NONE, DEFAULT_JUSTIFICATION_LEFT, DEFAULT_JUSTIFICATION_RIGHT, &
              DEFAULT_JUSTIFICATION_FULL, DEFAULT_JUSTIFICATION_CENTER, &
              DEFAULT_JUSTIFICATION_CURRENT, back_to_indentation, do_auto_fill, default_indent_new_line, read_number
    public :: count_lines, line_number_at_pos, apply_on_rectangle, rectangle_line_action_type, rectangle_line_type, &
              rectangle_position_as_coordinates, is_rectangle_intersect, rectangle_dimensions, delete_rectangle, &
              delete_extract_rectangle, extract_rectangle, extract_rectangle_bounds, cut_rectangle, &
              copy_rectangle, paste_rectangle, insert_rectangle, open_rectangle, delete_whitespace_rectangle, &
              string_rectangle, string_insert_rectangle, clear_rectangle, rectangle_number_lines, spaces_string, &
              rectangle_mark_mode, rectangle_mark_mode_map, rectangle_mark_mode_hook, deactivate_mark_hook, &
              region_bounds, is_region_noncontiguous, interactive_region, &
              last_cut_rectangle, register_type, register_contents_type, registers, register_separator, get_register, &
              set_register, point_to_register, window_configuration_to_register, jump_to_register, file_to_register, &
              buffer_to_register, number_to_register, increment_register, view_register, list_registers, &
              insert_register, copy_to_register, append_to_register, prepend_to_register, copy_rectangle_to_register, &
              register_read_with_preview, REGISTER_EMPTY, REGISTER_MARKER, REGISTER_TEXT, REGISTER_RECTANGLE, &
              REGISTER_NUMBER, REGISTER_WINDOW_CONFIGURATION, REGISTER_FILE, REGISTER_FILE_QUERY, REGISTER_BUFFER
    public :: execute_mode_hooks, fundamental_mode, special_mode, text_mode, prog_mode, normal_mode, set_auto_mode, &
              prepend_auto_mode, file_name_sans_versions
    public :: history_delete_duplicates, read_y_or_n_use_read_key, use_short_answers, minibuffer_message_timeout, &
              read_file_name_completion_ignore_case, read_buffer_completion_ignore_case, read_char_choice_use_read_key
    public :: tab_width, create_backup_files, change_major_mode_with_file_name, line_move_visual, goal_column, history_length, &
              truncate_partial_width_windows, enable_recursive_minibuffers, history_append_new_input, insert_default_directory, &
              inhibit_field_text_motion, visible_bell, transient_mark_mode, mark_even_if_inactive, clipboard_history_max, &
              mark_ring_max, cut_whole_line_option, delete_active_region, cut_read_only_ok, case_fold_search, &
              search_upper_case, search_exit_option, search_nonincremental_instead, search_ring_max, SEARCH_UPPER_CASE_NO, &
              SEARCH_UPPER_CASE_YES, SEARCH_UPPER_CASE_NOT_PASTED, next_screen_context_lines, scroll_error_top_bottom, &
              completion_ignore_case, completion_auto_help, completion_show_inline_help, completion_fail_discreetly, &
              suggest_key_bindings, completion_ignored_extensions, words_include_escapes, search_spaces_regexp, &
              regexp_search_ring_max, search_whitespace_regexp, isearch_lax_whitespace, &
              isearch_regexp_lax_whitespace, paragraph_start, paragraph_separate, sentence_end, sentence_end_base, &
              sentence_end_double_space, sentence_end_without_period, sentence_end_without_space, &
              case_identifiers_as_words, case_replace, replace_lax_whitespace, replace_regexp_lax_whitespace, &
              query_replace_skip_read_only, query_replace_show_replacement, query_replace_from_to_separator, &
              fill_prefix, cycle_spacing_actions, cycle_spacing_action_type, CYCLE_JUST_ONE_SPACE, &
              CYCLE_DELETE_SPACE_AFTER, CYCLE_DELETE_SPACE_BEFORE, CYCLE_DELETE_ALL_SPACE, CYCLE_RESTORE, &
              CYCLE_ARG_NONE, CYCLE_ARG_FIXED, CYCLE_ARG_INVERTED, CYCLE_ARG_MINUS, disabled_command_function_variable, &
              fill_column, left_margin, auto_fill_function, indent_tabs_mode, standard_indent, tab_always_indent, &
              tab_stops, indent_line_function, indent_region_function, colon_double_space, &
              fill_separate_heterogeneous_words_with_space, adaptive_fill_mode, adaptive_fill_regexp, &
              adaptive_fill_first_line_regexp, fill_indent_according_to_mode, default_justification, &
              fill_nobreak_functions, auto_fill_inhibit_regexp, inhibit_auto_fill, normal_auto_fill_function
    public :: setting_type, integer_setting_type, logical_setting_type, string_setting_type, function_setting_type, hook_type, &
              change_major_mode_hook, change_major_mode_after_body_hook, after_change_major_mode_hook, &
              major_mode_type, is_derived_mode, provided_mode_derived_p, minor_mode_type, minor_mode_body_procedure, &
              major_mode_named, text_mode_hook, text_mode_map, text_mode_syntax_table, prog_mode_hook, &
              prog_mode_map, special_mode_hook, special_mode_map, transient_mark_mode_hook, indent_tabs_mode_hook, &
              auto_fill_mode_hook, help_mode_hook, help_mode_map, turn_on_auto_fill, turn_off_auto_fill, text_mode_variant, &
              parse_expression_ignore_comments, comment_end_can_be_escaped, multibyte_syntax_as_identifier, &
              open_paren_in_column_0_is_subprogram_start, comment_use_syntax_ppss, require_final_newline, &
              mode_require_final_newline, FINAL_NEWLINE_NO, &
              FINAL_NEWLINE_SAVE, FINAL_NEWLINE_VISIT, FINAL_NEWLINE_VISIT_SAVE, FINAL_NEWLINE_ASK, change_hook_type, &
              after_change_hook_type, change_procedure, after_change_procedure, before_change_functions, &
              after_change_functions, first_change_hook, inhibit_modification_hooks
    public :: font_lock_defaults, font_lock_defaults_setting_type, font_lock_global_modes, font_lock_global_modes_type, &
              font_lock_mode_hook, global_font_lock_mode_hook, turn_on_font_lock, font_lock_keyword_type, &
              font_lock_highlight_type, font_lock_match_highlight_type, font_lock_keywords_type, &
              font_lock_syntax_entry_type, FONT_LOCK_OVERRIDE_NO, FONT_LOCK_OVERRIDE_YES, FONT_LOCK_OVERRIDE_KEEP, &
              FONT_LOCK_OVERRIDE_PREPEND, FONT_LOCK_OVERRIDE_APPEND, font_lock_matcher_procedure, font_lock_face_procedure, &
              font_lock_form_procedure, font_lock_maximum_decoration, FONT_LOCK_MAXIMUM_DECORATION_MAX, font_lock_verbose, &
              font_lock_dont_widen, font_lock_support_mode, FONT_LOCK_SUPPORT_MODE_NONE, FONT_LOCK_SUPPORT_MODE_JIT_LOCK, &
              font_lock_extra_managed_props, font_lock_syntactic_face_function, font_lock_keyword, &
              font_lock_highlight, font_lock_anchored, font_lock_add_keywords, FONT_LOCK_ADD_BEGINNING, &
              FONT_LOCK_ADD_END, FONT_LOCK_ADD_SET, jit_lock_chunk_size, fontification_functions, &
              position_hook_type, position_procedure
    public :: kbd_macro_termination_hook, kmacro_ring_max, kmacro_execute_before_append, kmacro_repeat_no_prefix, &
              kmacro_execute_repeat_key, kmacro_execute_repeat_with_arg, KMACRO_EXECUTE_REPEAT_KEY_NO, &
              KMACRO_EXECUTE_REPEAT_KEY_LAST, edmacro_eight_bits
    public :: describe_mode, describe_name, describe_bindings, view_lossage, view_echo_area_messages, &
              message_log_max, MESSAGE_LOG_MAX_NO, MESSAGE_LOG_MAX_UNLIMITED, messages_buffer_mode_hook, &
              messages_buffer_mode_map, apropos_mode_hook, apropos_mode_map
    public :: abbrev_table_type, abbrev_type, abbrev_ref_type, abbrev_table_ref_type, abbrev_hook_procedure, &
              abbrev_enable_procedure, &
              abbrev_expand_procedure, global_abbrev_table, fundamental_mode_abbrev_table, abbrev_tables, abbrev_mode, &
              only_global_abbrevs, abbrev_all_caps, abbrev_file_name, abbrevs_changed, abbrev_start_location, last_abbrev, &
              last_abbrev_text, last_abbrev_location, abbrev_expand_function, save_abbrevs, SAVE_ABBREVS_NO, &
              SAVE_ABBREVS_ASK, SAVE_ABBREVS_SILENTLY, edit_abbrevs_mode_hook, edit_abbrevs_mode_map, &
              edit_abbrevs_mode_abbrev_table, clear_all_abbrevs, copy_abbrev_table, insert_abbrevs, list_abbrevs, &
              abbrev_table_name, edit_abbrevs, write_abbrev_file, inverse_add_mode_abbrev, inverse_add_global_abbrev, &
              inverse_add_abbrev, abbrev_prefix_mark, expand_region_abbrevs, define_global_abbrev, define_mode_abbrev, &
              abbrev_name, abbrev_expansion, abbrev_insert, expand_abbrev, abbrev_default_expand, unexpand_abbrev, &
              insert_abbrev_table_description, local_abbrev_table, abbrev_table_set_local, text_mode_abbrev_table, &
              prog_mode_abbrev_table, special_mode_abbrev_table, help_mode_abbrev_table, save_some_buffers_procedure, &
              save_some_buffers_function_type, save_some_buffers_functions, append_save_some_buffers_function, &
              dabbrev_expand, is_dabbrev_same_major_mode, dabbrev_select_buffers, dabbrev_friend_buffer_procedure, &
              dabbrev_select_buffers_procedure, dabbrev_backward_only, dabbrev_limit, dabbrev_abbrev_skip_leading_regexp, &
              dabbrev_eliminate_newlines, dabbrev_case_fold_search, dabbrev_upcase_means_case_search, &
              dabbrev_case_distinction, dabbrev_case_replace, dabbrev_abbrev_char_regexp, dabbrev_check_all_buffers, &
              dabbrev_ignored_buffer_names, dabbrev_ignored_buffer_regexps, dabbrev_ignored_buffer_modes, &
              dabbrev_check_other_buffers, dabbrev_select_buffers_function, dabbrev_friend_buffer_function, &
              dabbrev_search_these_buffers_only, DABBREV_NO, DABBREV_YES, DABBREV_LIKE_CASE_FOLD_SEARCH, &
              DABBREV_LIKE_CASE_REPLACE, DABBREV_ASK
    public :: buffer_file_coding_system, coding_system_for_read, coding_system_for_write, last_coding_system_used, &
              inhibit_eol_conversion, inhibit_null_byte_detection, inhibit_iso_escape_detection, eol_mnemonic_unix, &
              eol_mnemonic_dos, eol_mnemonic_mac, eol_mnemonic_undecided, default_process_coding_system, &
              file_coding_systems, file_coding_type, locale_coding_system, is_coding_system, check_coding_system, &
              coding_system_base, coding_system_eol_type, coding_system_change_eol_conversion, &
              coding_system_change_text_conversion, coding_system_type, coding_system_priority_list, &
              set_coding_system_priority, coding_system_list_entry_type, define_coding_system_alias, &
              decode_coding_string, encode_coding_string, decode_coding_region, encode_coding_region, &
              detect_coding_string, detect_coding_region, EOL_UNIX, EOL_DOS, EOL_MAC, EOL_UNDECIDED, &
              auto_codings, auto_coding_regexps, auto_coding_type, prefer_coding_system, &
              set_default_coding_systems, set_buffer_file_coding_system, describe_coding_system, &
              set_process_coding_system, process_coding_system
    public :: post_self_insert_hook, execute_at_time, execute_with_timer, execute_with_idle_timer, cancel_timer, destroy_timer, &
              is_timer, timer_max_repeats
    public :: auto_revert_mode, auto_revert_tail_mode, global_auto_revert_mode, turn_on_auto_revert_mode, &
              turn_on_auto_revert_tail_mode, auto_revert_interval, auto_revert_stop_on_user_input, auto_revert_verbose, &
              auto_revert_mode_text, auto_revert_tail_mode_text, auto_revert_mode_hook, auto_revert_tail_mode_hook, &
              global_auto_revert_mode_text, global_auto_revert_mode_hook, global_auto_revert_non_file_buffers, &
              global_auto_revert_ignore_modes, auto_revert_check_vc_info, global_auto_revert_ignore_buffer, &
              auto_revert_remote_files, auto_revert_use_notify, auto_revert_notify_exclude_dir_regexp, &
              auto_revert_avoid_polling
    public :: savehist_mode, savehist_save, savehist_save_minibuffer_history, savehist_variable_type, &
              savehist_additional_variables, savehist_ignored_variables, savehist_file, savehist_file_modes, &
              savehist_autosave_interval, savehist_mode_hook, savehist_save_hook
    public :: save_place_mode, save_place_local_mode, save_place_file, save_place_limit, SAVE_PLACE_LIMIT_UNLIMITED, &
              save_place_forget_unreadable_files_option, save_place_abbreviate_file_names, save_place_save_skipped, &
              save_place_skip_check_regexp, save_place_ignore_files_regexp, save_place_autosave_interval, &
              save_place_after_find_file_hook, save_place_mode_hook, save_place_local_mode_hook, &
              save_place_version_control, SAVE_PLACE_VERSION_CONTROL_NUMBERED, SAVE_PLACE_VERSION_CONTROL_EXISTING, &
              SAVE_PLACE_VERSION_CONTROL_NO, SAVE_PLACE_VERSION_CONTROL_NOSPECIAL
    public :: recentf_mode, recentf_open, recentf_open_most_recent_file, recentf_save_list, recentf_cleanup, &
              recentf_file_type, recentf_files, recentf_check_type, recentf_filename_handler_type, &
              RECENTF_MAX_SAVED_ITEMS_UNLIMITED, &
              recentf_max_saved_items, recentf_save_file, recentf_save_file_modes, recentf_exclude, &
              recentf_exclude_ignored_extensions, recentf_keep, recentf_menu_action, RECENTF_AUTO_CLEANUP_MODE, &
              RECENTF_AUTO_CLEANUP_NO, recentf_auto_cleanup, recentf_initialize_file_name_history, &
              recentf_filename_handlers, recentf_show_messages, recentf_autosave_interval, recentf_mode_hook, &
              recentf_mode_map, recentf_keep_default_test, abbreviate_file_name_handler, file_truename_handler
    public :: bookmark_type, bookmark_inhibit_context_function_type, bookmark_use_annotations, bookmark_save_flag, &
              BOOKMARK_SAVE_FLAG_NO, BOOKMARK_SAVE_FLAG_ON_EXIT, bookmark_default_file, bookmark_watch_bookmark_file, &
              BOOKMARK_WATCH_BOOKMARK_FILE_NO, BOOKMARK_WATCH_BOOKMARK_FILE_ASK, BOOKMARK_WATCH_BOOKMARK_FILE_SILENT, &
              bookmark_version_control, BOOKMARK_VERSION_CONTROL_NUMBERED, BOOKMARK_VERSION_CONTROL_EXISTING, &
              BOOKMARK_VERSION_CONTROL_NO, BOOKMARK_VERSION_CONTROL_NOSPECIAL, bookmark_completion_ignore_case, &
              bookmark_sort_flag, BOOKMARK_SORT_FLAG_CREATED, BOOKMARK_SORT_FLAG_ALPHABETICAL, BOOKMARK_SORT_FLAG_LAST_MODIFIED, &
              bookmark_menu_confirm_deletion, bookmark_automatically_show_annotations, bookmark_fringe_mark, &
              bookmark_map, bookmark_search_size, bookmark_make_record_function, bookmark_inhibit_context_functions, &
              bookmark_edit_annotation_text_func, bookmark_edit_annotation_mode_map, bookmark_edit_annotation_mode_hook, &
              bookmark_after_jump_hook, bookmark_after_load_file_hook, bookmark_exit_hook, bookmark_bmenu_buffer, &
              bookmark_bmenu_file_column, bookmark_bmenu_type_column_width, bookmark_bmenu_toggle_filenames, &
              bookmark_search_delay, bookmark_bmenu_mode_map, bookmark_bmenu_mode_hook
    public :: tabulated_list_gui_sort_indicator_asc, tabulated_list_gui_sort_indicator_desc, &
              tabulated_list_tty_sort_indicator_asc, tabulated_list_tty_sort_indicator_desc, tabulated_list_mode_map, &
              tabulated_list_mode_hook, tabulated_list_revert_hook, truncate_string_ellipsis_variable
    public :: info_history_skip_intermediate_nodes, info_default_directories, info_directories, &
              info_additional_directories, info_scroll_prefer_subnodes, info_mode_hook, info_selection_hook, &
              info_url_table, info_url_type, info_suffixes, info_suffix_type, info_streamline_headings, &
              info_heading_type, info_mode_map, info_mode_syntax_table, info_fontify_maximum_menu_size, &
              INFO_FONTIFY_MAXIMUM_MENU_SIZE_NONE, INFO_FONTIFY_MAXIMUM_MENU_SIZE_UNLIMITED, info_fontify_visited_nodes, &
              info_isearch_search, &
              info_use_header_line, info_hide_note_references, INFO_HIDE_NOTE_REFERENCES_NO, &
              INFO_HIDE_NOTE_REFERENCES_SEE, INFO_HIDE_NOTE_REFERENCES_HIDE, INFO_HIDE_NOTE_REFERENCES_TAG, &
              info_breadcrumbs_depth, info_search_whitespace_regexp, info_warn_on_index_alternatives_wrap, &
              info_files_for_emacs, info_file_prefix_type
    public :: info_lookup_other_window_flag, info_lookup_highlight_face, info_lookup_file_names, &
              info_lookup_file_name_type, info_lookup_add_help, info_lookup_maybe_add_help, &
              info_lookup_doc_specs_type, info_lookup_trans_procedure, info_lookup_parse_procedure, info_lookup_doc_spec_procedure
    public :: ibuffer_formats, ibuffer_always_compile_formats, ibuffer_fontifications, &
              ibuffer_human_readable_size, ibuffer_use_other_window, ibuffer_default_shrink_to_minimum_size, &
              ibuffer_display_summary, ibuffer_truncate_lines, ibuffer_case_fold_search, &
              ibuffer_default_sorting_mode, ibuffer_default_sorting_reversep, ibuffer_eliding_string, &
              ibuffer_maybe_show_tests, ibuffer_default_display_maybe_show_tests, ibuffer_movement_cycle, &
              ibuffer_modified_char, ibuffer_read_only_char, ibuffer_marked_char, ibuffer_locked_char, &
              ibuffer_deletion_char, ibuffer_expert, ibuffer_view_ibuffer, ibuffer_always_show_last_buffer, &
              IBUFFER_ALWAYS_SHOW_LAST_BUFFER_NO, IBUFFER_ALWAYS_SHOW_LAST_BUFFER_YES, IBUFFER_ALWAYS_SHOW_LAST_BUFFER_NOMINI, &
              ibuffer_jump_offer_only_visible_buffers, ibuffer_use_header_line, IBUFFER_USE_HEADER_LINE_NO, &
              IBUFFER_USE_HEADER_LINE_FILTERS, IBUFFER_USE_HEADER_LINE_TITLE, ibuffer_default_directory, &
              ibuffer_help_buffer_modes, &
              ibuffer_compressed_file_name_regexp, ibuffer_hook, ibuffer_mode_hook, ibuffer_marked_face, &
              ibuffer_deletion_face, ibuffer_title_face, ibuffer_filter_group_name_face, &
              ibuffer_directory_abbrevs, ibuffer_never_show_tests, ibuffer_always_show_tests, &
              ibuffer_never_search_content_name, ibuffer_never_search_content_mode, ibuffer_old_time, &
              ibuffer_show_empty_filter_groups, ibuffer_mode_map, ibuffer_mode_filter_group_map, ibuffer_columns, &
              ibuffer_sorting_functions, define_ibuffer_column, define_ibuffer_sorter, ibuffer_column, ibuffer_text, &
              ibuffer_formats_type, ibuffer_column_spec_type, ibuffer_cell_type, ibuffer_column_procedure, &
              ibuffer_summarizer_procedure, ibuffer_sorter_procedure, ibuffer_size_procedure, ibuffer_buffer_test_procedure, &
              ibuffer_form_procedure, ibuffer_show_test_type, ibuffer_fontification_type, &
              ibuffer_directory_abbrev_type, IBUFFER_ALIGN_LEFT, IBUFFER_ALIGN_CENTER, IBUFFER_ALIGN_RIGHT
    public :: ibuffer_saved_filters, ibuffer_saved_filter_groups, ibuffer_filterings, ibuffer_filter_formats, &
              ibuffer_save_with_custom, define_ibuffer_filter, ibuffer_filtering_qualifiers, &
              ibuffer_set_filtering_qualifiers, ibuffer_filter_groups, ibuffer_set_filter_groups, &
              ibuffer_switch_to_saved_filter_groups, ibuffer_switch_to_saved_filters, is_ibuffer_included_in_filters, &
              ibuffer_filter_type, ibuffer_filter_node_type, ibuffer_saved_filters_type, &
              ibuffer_saved_filter_groups_type, ibuffer_filter_groups_type, ibuffer_filter_formats_type, &
              ibuffer_filter_def_type, ibuffer_filter_test_procedure, ibuffer_and, ibuffer_or, ibuffer_not, ibuffer_saved, &
              ibuffer_filter, ibuffer_major_mode, ibuffer_used_mode, ibuffer_derived_mode, ibuffer_name, &
              ibuffer_process, ibuffer_starred_name, ibuffer_filename, ibuffer_basename, ibuffer_file_extension, &
              ibuffer_directory, ibuffer_size_gt, ibuffer_size_lt, ibuffer_modified, ibuffer_visiting_file, &
              ibuffer_content, ibuffer_test
    public :: completing_read_multiple, crm_separator, crm_prompt, crm_local_completion_map, crm_local_must_match_map
    public :: buffer_menu_marker_char, buffer_menu_del_char, buffer_menu_use_header_line, buffer_menu_name_width, &
              buffer_menu_name_width_function, buffer_menu_name_width_procedure, buffer_menu_dynamic_name_width, &
              buffer_menu_size_width, buffer_menu_mode_width, buffer_menu_use_frame_buffer_list, &
              buffer_menu_human_readable_sizes, buffer_menu_mode_hook, buffer_menu_mode_map
    public :: repeat_mode, repeat_mode_hook, repeat_too_dangerous, repeat_on_final_keystroke, repeat_exit_key, &
              repeat_exit_timeout, repeat_keep_prefix, repeat_check_key, repeat_echo_function, repeat_echo_procedure, &
            repeat_echo_message, repeat_exit, REPEAT_CHECK_KEY_DEFAULT, REPEAT_CHECK_KEY_YES, REPEAT_CHECK_KEY_NO, set_repeat_map, &
              comint_repeat_map, shell_repeat_map
    public :: buffer_stale_function, buffer_stale_procedure, BUFFER_STALE_NO, BUFFER_STALE_YES, BUFFER_STALE_FAST, &
              buffer_auto_revert_by_notification, mounted_file_systems, after_set_visited_file_name_hook
    public :: show_paren_mode, show_paren_local_mode, show_paren_style, SHOW_PAREN_STYLE_PARENTHESIS, SHOW_PAREN_STYLE_EXPRESSION, &
              SHOW_PAREN_STYLE_MIXED, show_paren_delay, show_paren_priority, show_paren_ring_bell_on_mismatch, &
              show_paren_when_point_inside_paren, show_paren_when_point_in_periphery, show_paren_highlight_openparen, &
              show_paren_context_when_offscreen, show_paren_not_in_comments_or_strings, &
              SHOW_PAREN_NOT_IN_COMMENTS_OR_STRINGS_NO, SHOW_PAREN_NOT_IN_COMMENTS_OR_STRINGS_ALL, &
              SHOW_PAREN_NOT_IN_COMMENTS_OR_STRINGS_ON_MISMATCH, show_paren_data_function, show_paren_data_type, &
              show_paren_data_procedure, show_paren_default, show_paren_test_function, show_paren_test_procedure, &
              show_paren_default_test, show_paren_mode_hook, show_paren_local_mode_hook
    public :: blink_matching_paren, BLINK_MATCHING_PAREN_NO, BLINK_MATCHING_PAREN_YES, BLINK_MATCHING_PAREN_JUMP, &
              BLINK_MATCHING_PAREN_JUMP_OFFSCREEN, blink_matching_paren_on_screen, blink_matching_paren_distance, &
              blink_matching_delay, blink_matching_paren_dont_ignore_comments, blink_matching_check_function, &
              blink_matching_check_procedure, blink_matching_check_mismatch, blink_paren_function, blink_matching_open, &
              newline_and_indent, backward_delete_char_untabify, backward_delete_char_untabify_method, &
              BACKWARD_DELETE_CHAR_UNTABIFY_METHOD_NONE, BACKWARD_DELETE_CHAR_UNTABIFY_METHOD_UNTABIFY, &
              BACKWARD_DELETE_CHAR_UNTABIFY_METHOD_HUNGRY, BACKWARD_DELETE_CHAR_UNTABIFY_METHOD_ALL
    public :: electric_indent_mode, electric_indent_local_mode, electric_indent_chars, electric_indent_inhibit, &
              electric_indent_functions_without_reindent, electric_indent_mode_hook, electric_indent_local_mode_hook, &
              electric_indent_just_newline, electric_newline_and_maybe_indent, electric_pair_mode, &
              electric_pair_local_mode, electric_pair_type, electric_pair_pairs, electric_pair_text_pairs, &
              electric_pair_skip_self, electric_pair_inhibit_test_function, electric_pair_char_procedure, &
              electric_pair_default_skip_self, electric_pair_default_inhibit, electric_pair_conservative_inhibit, &
              electric_pair_preserve_balance, electric_pair_open_newline_between_pairs, electric_pair_skip_whitespace, &
              ELECTRIC_PAIR_SKIP_WHITESPACE_NO, ELECTRIC_PAIR_SKIP_WHITESPACE_YES, ELECTRIC_PAIR_SKIP_WHITESPACE_CHOMP, &
              electric_pair_skip_whitespace_chars, electric_pair_skip_whitespace_function, &
              electric_pair_text_syntax_table, electric_pair_string_bound_function, electric_pair_position_procedure, &
              electric_pair_mode_hook, electric_pair_local_mode_hook, electric_pair_delete_pair
    public :: dired, dired_other_window, dired_noselect, dired_mode, dired_mode_map, dired_mode_hook, &
              dired_before_readin_hook, dired_after_readin_hook, dired_initial_position_hook, dired_listing_switches, &
              dired_use_ls_dired, DIRED_USE_LS_DIRED_NO, DIRED_USE_LS_DIRED_YES, DIRED_USE_LS_DIRED_UNSPECIFIED, &
              dired_ls_f_marks_symlinks, dired_trivial_filenames, dired_free_space, DIRED_FREE_SPACE_NONE, &
              DIRED_FREE_SPACE_SEPARATE, DIRED_FREE_SPACE_FIRST, dired_mark_region, DIRED_MARK_REGION_NO, &
              DIRED_MARK_REGION_FILE, DIRED_MARK_REGION_LINE, dired_movement_style, DIRED_MOVEMENT_STYLE_PLAIN, &
              DIRED_MOVEMENT_STYLE_CYCLE, DIRED_MOVEMENT_STYLE_BOUNDED, DIRED_MOVEMENT_STYLE_CYCLE_FILES, &
              DIRED_MOVEMENT_STYLE_BOUNDED_FILES, dired_auto_revert_buffer, dired_check_symlinks, &
              dired_recursive_deletes, DIRED_RECURSIVE_NO, DIRED_RECURSIVE_YES, DIRED_RECURSIVE_TOP, &
              DIRED_RECURSIVE_ASK, dired_no_confirm, dired_no_confirm_type, dired_switches_in_mode_line, &
              DIRED_SWITCHES_IN_MODE_LINE_SORT_ORDER, DIRED_SWITCHES_IN_MODE_LINE_AS_IS, dired_sort_inhibit, &
              dired_garbage_files_regexp, &
              dired_always_read_filesystem, dired_deletion_confirmer, dired_header_face, dired_mark_face, &
              dired_marked_face, dired_flagged_face, dired_warning_face, dired_perm_write_face, &
              dired_directory_face, dired_symlink_face, dired_ignored_face, insert_directory_program, &
              find_file_run_dired, find_file_other_window, revert_buffer
    public :: dired_chown_program, dired_touch_program, DIRED_KEEP_MARKER, dired_keep_marker_rename, &
              dired_keep_marker_copy, dired_keep_marker_hardlink, dired_keep_marker_symlink, dired_keep_marker_relsymlink, &
              dired_copy_preserve_time, dired_copy_dereference, dired_recursive_copies, dired_dwim_target, &
              DIRED_DWIM_TARGET_NO, DIRED_DWIM_TARGET_NEXT, DIRED_DWIM_TARGET_NEXT_VISIBLE, DIRED_DWIM_TARGET_RECENT, &
              dired_create_destination_dirs, DIRED_CREATE_DESTINATION_DIRS_NO, DIRED_CREATE_DESTINATION_DIRS_YES, &
              DIRED_CREATE_DESTINATION_DIRS_ASK, dired_create_destination_dirs_on_trailing_dirsep, &
              dired_do_revert_buffer, dired_create_empty_file_in_current_directory, &
              dired_compress_file_default_suffix, dired_compress_directory_default_suffix, dired_compress_suffix_type, &
              dired_compress_rule_type, dired_compress_file_suffixes, dired_compress_file_commands, &
              dired_compress_files_commands
    public :: copy_directory, copy_directory_create_symlink, view_read_only, find_file_read_only, &
              find_file_read_only_other_window, create_empty_file, file_modes_symbolic_to_number, &
              file_modes_number_to_symbolic, copy_file, rename_file, add_name_to_file, create_symbolic_link, &
              set_file_modes, set_file_times, read_only_mode, read_only_mode_hook, what_line, view_mode, view_mode_map, &
              view_mode_hook, view_file, view_file_other_window, view_buffer, view_buffer_other_window, &
              view_scroll_auto_exit, view_exits_all_viewing_windows, view_inhibit_help_message, view_no_disable_on_exit
    public :: comment_use_syntax, comment_fill_column, comment_column, comment_start, comment_start_skip, &
              comment_end_skip, comment_end, comment_indent_function, comment_indent_type, &
              comment_insert_comment_function, comment_region_function, uncomment_region_function, &
              comment_quote_nested, comment_quote_nested_function, comment_continue, comment_add, &
              comment_style, comment_padding, comment_inline_offset, comment_multi_line, &
              comment_empty_lines, comment_setup_function, comment_auto_fill_only_comments, &
              COMMENT_USE_SYNTAX_NO, COMMENT_USE_SYNTAX_YES, COMMENT_USE_SYNTAX_UNDECIDED, &
              COMMENT_STYLE_PLAIN, COMMENT_STYLE_INDENT_OR_TRIPLE, COMMENT_STYLE_INDENT, &
              COMMENT_STYLE_ALIGNED, COMMENT_STYLE_BOX, COMMENT_STYLE_EXTRA_LINE, COMMENT_STYLE_MULTI_LINE, &
              COMMENT_STYLE_BOX_MULTI, COMMENT_EMPTY_LINES_NO, COMMENT_EMPTY_LINES_YES, &
              COMMENT_EMPTY_LINES_EOL, COMMENT_INDENT_NONE, COMMENT_INDENT_COLUMN, COMMENT_INDENT_RANGE
    public :: conf_assignment_column, conf_javaprop_assignment_column, conf_colon_assignment_column, &
              conf_assignment_space, conf_colon_assignment_space, conf_mode_map, conf_mode_syntax_table, &
              conf_unix_mode_syntax_table, conf_space_keywords_by_file, conf_space_keywords_entry_type, &
              conf_mode_hook, conf_unix_mode_hook, conf_windows_mode_hook, conf_javaprop_mode_hook, &
              conf_space_mode_hook, conf_colon_mode_hook, conf_ppd_mode_hook, conf_xdefaults_mode_hook, &
              conf_toml_mode_hook, conf_desktop_mode_hook, conf_npmrc_mode_hook
    public :: subprogram_prompt_regexp, parens_require_spaces, end_of_subprogram_moves_to_eol, &
              narrow_to_subprogram_include_comments, insert_pairs, insert_pair_type, delete_pair_blink_delay, &
              delete_pair_push_mark, beginning_of_subprogram_function, end_of_subprogram_function
    public :: widen_automatically
    public :: makefile_target_colon, makefile_macro_assign, makefile_electric_keys, &
              makefile_use_curly_braces_for_macros, makefile_tab_after_target_colon, makefile_backslash_column, &
              makefile_backslash_align, makefile_pickup_everything_picks_up_filenames, &
              makefile_cleanup_continuations, makefile_special_targets, makefile_runtime_macros, &
              makefile_dependency_regexp, makefile_dependency_skip, makefile_rule_action_regexp, &
              makefile_brave_make, makefile_query_one_target_method_function, makefile_query_procedure, &
              makefile_up_to_date_buffer_name, makefile_gnumake_functions, &
              makefile_gnumake_function_type, makefile_mode_map, makefile_mode_syntax_table, &
              makefile_imake_mode_syntax_table, makefile_mode_abbrev_table, makefile_mode_hook, &
              makefile_automake_mode_hook, makefile_gmake_mode_hook, makefile_makepp_mode_hook, &
              makefile_bsdmake_mode_hook, makefile_imake_mode_hook
    public :: f90_do_indent, f90_if_indent, f90_type_indent, f90_program_indent, f90_associate_indent, &
              f90_critical_indent, f90_continuation_indent, f90_comment_region, f90_indented_comment_regexp, &
              f90_directive_comment_regexp, f90_beginning_ampersand, f90_smart_end, f90_smart_end_names, &
              f90_break_delimiters, f90_break_before_delimiters, f90_auto_keyword_case, f90_leave_line_no, &
              f90_mode_hook, f90_mode_map, f90_mode_syntax_table, f90_mode_abbrev_table, F90_SMART_END_NO, &
              F90_SMART_END_BLINK, F90_SMART_END_NO_BLINK, F90_AUTO_KEYWORD_CASE_NONE, F90_AUTO_KEYWORD_CASE_DOWNCASE_WORD, &
              F90_AUTO_KEYWORD_CASE_UPCASE_WORD, F90_AUTO_KEYWORD_CASE_CAPITALIZE_WORD
    public :: smie_indent_basic, smie_blink_matching_inners, smie_indent_functions, forward_expression_function
    public :: sh_ancestors, sh_aliases, sh_ancestor_type, sh_alias_type, sh_shell_file, sh_shell, &
              sh_shell_arg, sh_assignment_regexp, sh_remember_variable_min, sh_beginning_of_command, &
              sh_end_of_command, sh_here_document_word, sh_builtins, sh_indent_statement_after_and, &
              sh_leading_keywords, sh_other_keywords, sh_variables, sh_font_lock_keywords_var, &
              sh_font_lock_keywords_var_1, sh_font_lock_keywords_var_2, sh_set_shell_hook, sh_mode_hook, &
              sh_base_mode_hook, sh_basic_offset, sh_indent_for_fi, sh_indent_for_done, sh_indent_after_else, &
              sh_indent_after_if, sh_indent_for_then, sh_indent_for_do, sh_indent_after_do, &
              sh_indent_after_loop_construct, sh_indent_after_done, sh_indent_for_case_label, &
              sh_indent_for_case_alt, sh_indent_for_continuation, sh_indent_after_open, &
              sh_indent_after_function, sh_indent_after_switch, sh_indent_after_case, sh_backslash_column, &
              sh_backslash_align, sh_indent_supported, sh_make_vars_local, sh_indent_after_continuation, &
              sh_mode_map, sh_base_mode_map, sh_mode_syntax_table, sh_mode_abbrev_table, &
              sh_strings_feature_type, sh_keywords_feature_type, sh_string_feature_type, sh_string_type, &
              SH_PLUS, SH_MINUS, SH_PLUS_PLUS, SH_MINUS_MINUS, SH_STAR, SH_SLASH, SH_INDENT_AFTER_CONTINUATION_NO, &
              SH_INDENT_AFTER_CONTINUATION_DEEPER, SH_INDENT_AFTER_CONTINUATION_ALWAYS, sh_dynamic_complete_functions, sh_styles, &
              sh_style_type, sh_vars
    public :: python_2_support, python_indent_offset, python_indent_guess_indent_offset_variable, &
              python_indent_guess_indent_offset_verbose, python_indent_trigger_commands, &
              python_indent_def_block_scale, python_indent_block_paren_deeper, python_forward_expression_function, &
              python_fill_docstring_style, python_fill_comment_function, python_fill_string_function, &
              python_fill_decorator_function, python_fill_paren_function, python_base_mode_map, &
              python_mode_map, python_indent_repeat_map, python_mode_syntax_table, python_dotty_syntax_table, &
              python_base_mode_hook, python_mode_hook, python_font_lock_keywords_level_1, &
              python_font_lock_keywords_level_2, python_font_lock_keywords_maximum_decoration, &
              PYTHON_FILL_DOCSTRING_STYLE_NONE, PYTHON_FILL_DOCSTRING_STYLE_DJANGO, PYTHON_FILL_DOCSTRING_STYLE_ONETWO, &
              PYTHON_FILL_DOCSTRING_STYLE_PEP_257, &
              PYTHON_FILL_DOCSTRING_STYLE_PEP_257_NN, PYTHON_FILL_DOCSTRING_STYLE_SYMMETRIC
    public :: executable_insert, executable_query, executable_magicless_file_regexp, executable_prefix, &
              executable_prefix_env, executable_chmod, executable_self_display, &
              executable_font_lock_keywords, executable_make_buffer_file_executable_if_script, &
              EXECUTABLE_QUERY_NO, EXECUTABLE_QUERY_FUNCTION, EXECUTABLE_QUERY_ALWAYS, EXECUTABLE_CHMOD_NONE
    public :: before_save_hook, after_save_hook, write_file_functions
    public :: fill_paragraph_function, fill_paragraph_handle_comment, adaptive_fill_function
    public :: sgml_basic_offset, sgml_attribute_offset, sgml_xml_mode, sgml_specials, sgml_validate_command, &
              sgml_slash_distance, sgml_char_names, sgml_empty_tags, sgml_unclosed_tags, sgml_whitespace_sensitive_tags, &
              html_tag_faces, sgml_tag_face_type, sgml_mode_hook, html_mode_hook, sgml_mode_map, html_mode_map, &
              sgml_mode_syntax_table, sgml_tag_syntax_table
    public :: js_indent_level, js_expr_indent_offset, js_paren_indent_offset, js_square_indent_offset, &
              js_curly_indent_offset, js_switch_indent_offset, js_flat_functions, js_indent_align_list_continuation, &
              js_comment_lineup_func, js_enabled_frameworks, js_indent_first_init, js_chain_indent, js_base_mode_hook, &
              js_mode_hook, js_json_mode_hook, js_mode_map, js_base_mode_map, js_json_mode_map, js_mode_syntax_table, &
              JS_INDENT_FIRST_INIT_NO, JS_INDENT_FIRST_INIT_YES, JS_INDENT_FIRST_INIT_DYNAMIC
    public :: css_electric_keys, css_fontify_colors, css_indent_offset, css_lookup_url_format, css_class_list_function, &
              css_id_list_function, css_base_mode_hook, css_mode_hook, scss_mode_hook, css_mode_map, css_base_mode_map, &
              scss_mode_map, css_mode_syntax_table, scss_mode_syntax_table
    public :: echo_keystrokes, echo_keystrokes_help, translate_upper_case_key_bindings, help_char, help_events, &
              inhibit_interaction, num_nonmacro_input_events, read_char, read_char_exclusive, read_event, &
              read_key_sequence_vs, this_command_key_events, this_single_command_keys, this_single_command_raw_keys, &
              clear_this_command_keys, recursion_depth, top_level, recursive_edit, lossage_size, event_convert_list, &
              event_name_parse_modifiers, open_dribble_file, pre_command_hook, post_command_hook
end module fortmacs
