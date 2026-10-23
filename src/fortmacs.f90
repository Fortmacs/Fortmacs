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
!> `user_config` with a public subroutine `configure`, which runs once at
!> startup (where upstream loads the init file):
!>
!>     module user_config
!>         use fortmacs
!>         implicit none(type, external)
!>         private
!>         public :: configure
!>     contains
!>         subroutine configure(status)
!>             type(status_type), intent(out) :: status
!>
!>             tab_width = 4
!>             make_backup_files = .false.
!>             call defcommand("insert-date-stamp", insert_date_stamp)
!>             call keymap_global_set("C-c d", "insert-date-stamp", status)
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
!> Settings are the variables below; assign them.  Commands are
!> subroutines taking a `status_type`, registered with `defcommand` and
!> bound with `keymap_global_set`.  A `configure` that fails (returns a
!> status that is not ok) leaves the editor running and shows the error.
!> `fortmacs -q` starts without running `configure`.
!>
!> This module only gathers names from the modules that define them, so
!> the API stays one `use` away.
module fortmacs
    use, intrinsic :: iso_fortran_env, only: int64
    ! Errors and values.
    use error_fm, only: status_type, set_error, format_error, FM_GENERIC_ERROR
    use lisp_fm, only: value_type, nil, t_value, make_fixnum, make_string, list
    use lread_fm, only: sym
    use data_fm, only: symbol_value, set
    ! Commands and keys.
    use callint_fm, only: command_proc, defcommand, call_interactively, &
                          current_prefix_arg, prefix_numeric_value
    use keymap_fm, only: keymap_global_set, keymap_set, key_valid_p, key_parse, &
                         key_description, define_key, current_global_map, make_sparse_keymap
    use subr_fm, only: global_set_key, y_or_n_p
    ! Buffers and editing.
    use handle_fm, only: handle_type
    use buffer_fm, only: current_buffer, get_buffer_create, set_buffer, buffer_name
    use insdel_fm, only: insert
    use editfns_fm, only: point, point_min, point_max, goto_char, buffer_substring, &
                          delete_region, bobp, eobp, bolp, eolp, region_beginning, region_end
    use cmds_fm, only: forward_char, backward_char, forward_line, beginning_of_line, end_of_line
    use files_fm, only: find_file, save_buffer
    use fns_fm, only: yes_or_no_p
    use minibuf_fm, only: read_string
    use xdisp_fm, only: message
    use search_fm, only: search_forward, search_backward, match_beginning, match_end, &
                         SEARCH_NOERROR, SEARCH_MOVE
    use isearch_fm, only: isearch_forward_command, isearch_backward_command
    use window_fm, only: scroll_up_command, scroll_down_command, recenter, recenter_top_bottom
    use simple_fm, only: undo, undo_only, undo_redo
    use undo_fm, only: undo_boundary
    use simple_fm, only: kill_new, current_kill, kill_region, yank, push_mark, set_mark, mark, &
                         region_active_p, deactivate_mark
    use minibuf_fm, only: completion_table_type, completion_list_table, completion_obarray_table, &
                          completion_function_table, completion_predicate, completion_function, &
                          try_completion, all_completions, test_completion
    use minibuffer_fm, only: completing_read, read_file_name, REQUIRE_MATCH_NIL, REQUIRE_MATCH_T, &
                             REQUIRE_MATCH_CONFIRM, REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION
    use simple_fm, only: execute_extended_command
    use keymap_fm, only: where_is_internal
    use syntax_fm, only: forward_word, modify_syntax_entry, char_syntax, standard_syntax_table, make_syntax_table, &
                         copy_syntax_table, set_syntax_table, syntax_table
    use simple_fm, only: backward_word, kill_word, backward_kill_word, mark_word
    use paragraphs_fm, only: forward_paragraph, backward_paragraph, mark_paragraph, kill_paragraph, &
                             backward_kill_paragraph
    use help_fns_fm, only: describe_function, describe_variable
    use help_fm, only: substitute_command_keys
    use window_fm, only: split_window, delete_window, delete_other_windows, other_window, select_window, &
                         other_buffer, switch_to_buffer, quit_window
    use frame_fm, only: selected_window
    use buffer_fm, only: kill_buffer
    use minibuffer_fm, only: read_buffer
    use search_fm, only: re_search_forward, re_search_backward, posix_search_forward, posix_search_backward, &
                         looking_at, posix_looking_at, string_match, posix_string_match, match_data, &
                         set_match_data, regexp_quote
    use subr_fm, only: match_string, match_string_no_properties, string_match_p, looking_at_p, looking_back
    use isearch_fm, only: isearch_forward_regexp_command, isearch_backward_regexp_command
    use paragraphs_fm, only: forward_sentence, backward_sentence, kill_sentence, backward_kill_sentence, &
                             mark_end_of_sentence, sentence_end_regexp
    use search_fm, only: replace_match
    use subr_fm, only: match_substitute_replacement
    use casefiddle_fm, only: upcase_string, downcase_string, capitalize_string, upcase_initials_string, &
                             upcase_char, downcase_char, capitalize_char, upcase_initials_char, upcase_region, &
                             downcase_region, capitalize_region, upcase_initials_region, upcase_word, &
                             downcase_word, capitalize_word
    use simple_fm, only: transpose_chars, transpose_words, transpose_lines, delete_horizontal_space, &
                         delete_all_space, just_one_space, cycle_spacing, fixup_whitespace, delete_blank_lines, &
                         delete_indentation
    use paragraphs_fm, only: transpose_sentences, transpose_paragraphs
    use syntax_fm, only: skip_chars_forward, skip_chars_backward, skip_syntax_forward, skip_syntax_backward
    use novice_fm, only: enable_command, disable_command
    use rmc_fm, only: rmc_choice_type, read_multiple_choice
    use indent_fm, only: indent_to, current_indentation, indent_according_to_mode, indent_for_tab_command, &
                         indent_rigidly, indent_line_to, current_left_margin, move_to_left_margin, indent_region, &
                         indent_relative, tab_to_tab_stop, move_to_tab_stop, beginning_of_line_text, &
                         indent_line_proc, indent_region_proc
    use fill_fm, only: fill_paragraph, fill_region, fill_region_as_paragraph, unfill_paragraph, set_fill_prefix, &
                       justify_current_line, canonically_space_region, current_fill_column, current_justification, &
                       fill_single_word_nobreak_p, fill_french_nobreak_p, fill_polish_nobreak_p, &
                       fill_single_char_nobreak_p, fill_nobreak_predicate_type, JUSTIFY_NONE, JUSTIFY_LEFT, &
                       JUSTIFY_RIGHT, JUSTIFY_FULL, JUSTIFY_CENTER, JUSTIFY_T
    use simple_fm, only: back_to_indentation, auto_fill_mode, do_auto_fill, default_indent_new_line, &
                         set_fill_column, beginning_of_buffer, end_of_buffer
    use subr_fm, only: read_number
    use replace_fm, only: query_replace, query_replace_regexp, map_query_replace_regexp, replace_string, &
                          replace_regexp, perform_replace, replacement_function
    ! Settings.
    use xdisp_fm, only: tab_width
    use files_fm, only: make_backup_files
    use simple_fm, only: line_move_visual, goal_column
    use subr_fm, only: history_length
    use minibuf_fm, only: enable_recursive_minibuffers, history_add_new_input
    use minibuffer_fm, only: insert_default_directory
    use editfns_fm, only: inhibit_field_text_motion
    use buffer_fm, only: transient_mark_mode
    use callint_fm, only: mark_even_if_inactive
    use simple_fm, only: kill_ring_max, mark_ring_max, kill_whole_line_option, delete_active_region, &
                         kill_read_only_ok
    use term_fm, only: visible_bell
    use minibuf_fm, only: completion_ignore_case
    use minibuffer_fm, only: completion_auto_help, completion_show_inline_help, completion_fail_discreetly
    use simple_fm, only: suggest_key_bindings
    use dired_fm, only: completion_ignored_extensions
    use search_fm, only: case_fold_search
    use isearch_fm, only: search_upper_case, search_exit_option, search_nonincremental_instead, &
                          search_ring_max, UPPER_CASE_NIL, UPPER_CASE_T, UPPER_CASE_NOT_YANKS
    use window_fm, only: next_screen_context_lines, scroll_error_top_bottom
    use syntax_fm, only: words_include_escapes
    use search_fm, only: search_spaces_regexp
    use isearch_fm, only: regexp_search_ring_max, search_whitespace_regexp, isearch_lax_whitespace, &
                          isearch_regexp_lax_whitespace
    use paragraphs_fm, only: paragraph_start, paragraph_separate, sentence_end, sentence_end_base, &
                             sentence_end_double_space, sentence_end_without_period, sentence_end_without_space
    use casefiddle_fm, only: case_symbols_as_words
    use simple_fm, only: fill_prefix, cycle_spacing_actions, cycle_spacing_action_type, CYCLE_JUST_ONE_SPACE, &
                         CYCLE_DELETE_SPACE_AFTER, CYCLE_DELETE_SPACE_BEFORE, CYCLE_DELETE_ALL_SPACE, CYCLE_RESTORE, &
                         CYCLE_ARG_NONE, CYCLE_ARG_FIXED, CYCLE_ARG_INVERTED, CYCLE_ARG_MINUS
    use novice_fm, only: disabled_command_function_p
    use buffer_fm, only: fill_column, left_margin, auto_fill_function
    use indent_fm, only: indent_tabs_mode, standard_indent, tab_always_indent, tab_stop_list, &
                         indent_line_function, indent_region_function
    use fill_fm, only: colon_double_space, fill_separate_heterogeneous_words_with_space, adaptive_fill_mode, &
                       adaptive_fill_regexp, adaptive_fill_first_line_regexp, fill_indent_according_to_mode, &
                       default_justification, fill_nobreak_predicate
    use simple_fm, only: auto_fill_inhibit_regexp, inhibit_auto_fill, normal_auto_fill_function
    use replace_fm, only: case_replace, replace_lax_whitespace, replace_regexp_lax_whitespace, &
                          query_replace_skip_read_only, query_replace_show_replacement, &
                          query_replace_from_to_separator
    implicit none(type, external)
    private

    public :: int64
    public :: status_type, set_error, format_error, FM_GENERIC_ERROR
    public :: value_type, nil, t_value, make_fixnum, make_string, list, sym
    public :: symbol_value, set
    public :: command_proc, defcommand, call_interactively, current_prefix_arg, &
              prefix_numeric_value
    public :: keymap_global_set, keymap_set, key_valid_p, key_parse, key_description, &
              define_key, current_global_map, make_sparse_keymap, global_set_key
    public :: handle_type, current_buffer, get_buffer_create, set_buffer, buffer_name
    public :: insert, point, point_min, point_max, goto_char, buffer_substring, &
              delete_region, bobp, eobp, bolp, eolp
    public :: forward_char, backward_char, forward_line, beginning_of_line, end_of_line
    public :: find_file, save_buffer, yes_or_no_p, y_or_n_p, read_string, message
    public :: search_forward, search_backward, match_beginning, match_end, SEARCH_NOERROR, SEARCH_MOVE, &
              isearch_forward_command, isearch_backward_command
    public :: scroll_up_command, scroll_down_command, recenter, recenter_top_bottom
    public :: undo, undo_only, undo_redo, undo_boundary
    public :: region_beginning, region_end, kill_new, current_kill, kill_region, yank, push_mark, &
              set_mark, mark, region_active_p, deactivate_mark
    public :: completion_table_type, completion_list_table, completion_obarray_table, &
              completion_function_table, completion_predicate, completion_function, try_completion, &
              all_completions, test_completion, completing_read, read_file_name, REQUIRE_MATCH_NIL, &
              REQUIRE_MATCH_T, REQUIRE_MATCH_CONFIRM, REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION, &
              execute_extended_command, where_is_internal
    public :: forward_word, backward_word, kill_word, backward_kill_word, mark_word, modify_syntax_entry, &
              char_syntax, standard_syntax_table, make_syntax_table, copy_syntax_table, set_syntax_table, &
              syntax_table, forward_paragraph, backward_paragraph, mark_paragraph, kill_paragraph, &
              backward_kill_paragraph, describe_function, describe_variable, substitute_command_keys
    public :: split_window, delete_window, delete_other_windows, other_window, select_window, selected_window, &
              other_buffer, switch_to_buffer, quit_window, kill_buffer, read_buffer
    public :: re_search_forward, re_search_backward, posix_search_forward, posix_search_backward, looking_at, &
              posix_looking_at, string_match, posix_string_match, match_data, set_match_data, regexp_quote, &
              match_string, match_string_no_properties, string_match_p, looking_at_p, looking_back, &
              isearch_forward_regexp_command, isearch_backward_regexp_command, forward_sentence, &
              backward_sentence, kill_sentence, backward_kill_sentence, mark_end_of_sentence, sentence_end_regexp, &
              replace_match, match_substitute_replacement, upcase_string, downcase_string, capitalize_string, &
              upcase_initials_string, upcase_char, downcase_char, capitalize_char, upcase_initials_char, &
              upcase_region, downcase_region, capitalize_region, upcase_initials_region, upcase_word, &
              downcase_word, capitalize_word, query_replace, query_replace_regexp, map_query_replace_regexp, &
              replace_string, replace_regexp, perform_replace, replacement_function, transpose_chars, &
              transpose_words, transpose_lines, transpose_sentences, transpose_paragraphs, delete_horizontal_space, &
              delete_all_space, just_one_space, cycle_spacing, fixup_whitespace, delete_blank_lines, &
              delete_indentation, skip_chars_forward, skip_chars_backward, skip_syntax_forward, &
              skip_syntax_backward, enable_command, disable_command, rmc_choice_type, read_multiple_choice, &
              indent_to, current_indentation, indent_according_to_mode, indent_for_tab_command, indent_rigidly, &
              indent_line_to, current_left_margin, move_to_left_margin, indent_region, indent_relative, &
              tab_to_tab_stop, move_to_tab_stop, beginning_of_line_text, indent_line_proc, indent_region_proc, &
              fill_paragraph, fill_region, fill_region_as_paragraph, unfill_paragraph, set_fill_prefix, &
              justify_current_line, canonically_space_region, current_fill_column, current_justification, &
              fill_single_word_nobreak_p, fill_french_nobreak_p, fill_polish_nobreak_p, &
              fill_single_char_nobreak_p, fill_nobreak_predicate_type, JUSTIFY_NONE, JUSTIFY_LEFT, &
              JUSTIFY_RIGHT, JUSTIFY_FULL, JUSTIFY_CENTER, JUSTIFY_T, back_to_indentation, auto_fill_mode, &
              do_auto_fill, default_indent_new_line, set_fill_column, beginning_of_buffer, end_of_buffer, &
              read_number
    public :: tab_width, make_backup_files, line_move_visual, goal_column, &
              history_length, enable_recursive_minibuffers, history_add_new_input, &
              insert_default_directory, inhibit_field_text_motion, visible_bell, &
              transient_mark_mode, mark_even_if_inactive, kill_ring_max, mark_ring_max, &
              kill_whole_line_option, delete_active_region, kill_read_only_ok, &
              case_fold_search, search_upper_case, search_exit_option, search_nonincremental_instead, &
              search_ring_max, UPPER_CASE_NIL, UPPER_CASE_T, UPPER_CASE_NOT_YANKS, &
              next_screen_context_lines, scroll_error_top_bottom, completion_ignore_case, &
              completion_auto_help, completion_show_inline_help, completion_fail_discreetly, &
              suggest_key_bindings, completion_ignored_extensions, words_include_escapes, &
              search_spaces_regexp, regexp_search_ring_max, search_whitespace_regexp, isearch_lax_whitespace, &
              isearch_regexp_lax_whitespace, paragraph_start, paragraph_separate, sentence_end, &
              sentence_end_base, sentence_end_double_space, sentence_end_without_period, &
              sentence_end_without_space, case_symbols_as_words, case_replace, replace_lax_whitespace, &
              replace_regexp_lax_whitespace, query_replace_skip_read_only, query_replace_show_replacement, &
              query_replace_from_to_separator, fill_prefix, cycle_spacing_actions, cycle_spacing_action_type, &
              CYCLE_JUST_ONE_SPACE, CYCLE_DELETE_SPACE_AFTER, CYCLE_DELETE_SPACE_BEFORE, CYCLE_DELETE_ALL_SPACE, &
              CYCLE_RESTORE, CYCLE_ARG_NONE, CYCLE_ARG_FIXED, CYCLE_ARG_INVERTED, CYCLE_ARG_MINUS, &
              disabled_command_function_p, fill_column, left_margin, auto_fill_function, indent_tabs_mode, &
              standard_indent, tab_always_indent, tab_stop_list, indent_line_function, indent_region_function, &
              colon_double_space, fill_separate_heterogeneous_words_with_space, adaptive_fill_mode, &
              adaptive_fill_regexp, adaptive_fill_first_line_regexp, fill_indent_according_to_mode, &
              default_justification, fill_nobreak_predicate, auto_fill_inhibit_regexp, inhibit_auto_fill, &
              normal_auto_fill_function
end module fortmacs
