! command_interpreter_mod.f90 -- general command interpreter in a window stuff.
! Ported from GNU Emacs 31.1 lisp/comint.el.
!
! Copyright (C) 1988, 1990, 1992-2026 Free Software Foundation, Inc.
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

!> Comint mode: a buffer talking to an interactive subprocess.  Output
!> goes in through `command_interpreter_output_filter' (carriage motion, the prompt
!> highlighted and marked as output fields); RET (`command_interpreter_send_input')
!> sends what follows the process mark, keeping it in the input ring
!> (M-p, M-n, C-c M-r, C-c M-s); C-c C-c, C-c C-\ and C-c C-d interrupt,
!> quit and end the input of the current subjob; C-c C-o, C-c C-r and
!> C-c C-e work on the last output.
!>
!> A comint buffer's own state (the input ring, its markers, its input
!> sender and old-input function) is kept by buffer here; the input ring
!> and its indices are permanent, as upstream's are.
!>
!> Input history expansion (`command_interpreter_input_autoexpand',
!> `command_interpreter_replace_by_expanded_history', `command_interpreter_magic_space',
!> `command_interpreter_insert_previous_argument' on C-c .), Isearch in the input
!> history (`command_interpreter_history_search_backward_regular_expression' on M-r,
!> `command_interpreter_history_incremental_search'), `command_interpreter_suspend_subjob' (C-c C-z) and
!> `command_interpreter_continue_subjob', and `command_interpreter_scroll_to_bottom_on_input' are
!> ported.
!>
!> Password prompts are recognized (`command_interpreter_watch_for_password_prompt'
!> on `command_interpreter_output_filter_functions', `command_interpreter_password_prompt_regular_expression')
!> and answered with `command_interpreter_send_invisible'.
!>
!> Completion: `command_interpreter_completion_at_point' on
!> `completion_at_point_functions' runs `command_interpreter_dynamic_complete_functions'
!> (history references, then file names quoted with `\' for
!> `command_interpreter_file_name_quote_characters'), `command_interpreter_dynamic_complete_filename',
!> `command_interpreter_replace_by_expanded_filename',
!> `command_interpreter_dynamic_list_filename_completions' and
!> `command_interpreter_complete_input_ring' (C-x <up>).
!>
!> Not ported yet (Deferred): `command_interpreter_dynamic_list_input_ring' (C-c C-l)
!> and `command_interpreter_dynamic_list_completions' (choosing from the list needs
!> `choose_completion_string'); the status of a stopped
!> process (process_mod reports none); redirection (`command_interpreter_redirect_*'); network
!> connections and ptys without a program as PROGRAM; `command_interpreter_fontify_input_mode'
!> and indirect buffers; the mouse (`command_interpreter_insert_input'), menus and
!> `command_interpreter_repeat_map'; `insert_in_front_hooks' on output (the
!> property is set, text property hooks are not ported);
!> `filter_buffer_substring_function'; `window_point_insertion_type';
!> `command_interpreter_osc_process_output'; source file commands
!> (`command_interpreter_get_source' and friends); `command_interpreter_execute' with switches read
!> after C-u (an error says so); coding systems.
module command_interpreter_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use execute_interactively_mod, only: PREFIX_NONE, PREFIX_RAW, PREFIX_NUMBER, prefix_argument_type, command_type
    use value_mod, only: vtag, V_NULL, V_NAME, V_STRING, V_ARRAY, value_type
    use error_mod, only: status_type, set_error, format_error, substitute_quotes, GENERIC_ERROR_CODE, USER_ERROR_CODE
    use handle_mod, only: handle_type, null_handle
    use value_mod, only: value_type, list, symbols, xsymbol, list_elements, command_procedure
    use data_mod, only: string_setting_type, logical_setting_type, integer_setting_type, symbol_value, set, &
                        append_locals_clearer, forward_setting, choice_setting_type
    use buffer_local_setting_mod, only: buffer_local_setting_type
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_or_make_buffer, is_buffer_live, &
                          barf_if_buffer_read_only, MODE_LINE_BUFFER_PROCESS_STATUS
    use marker_mod, only: make_marker, destroy_marker, marker_buffer, marker_position, set_marker, copy_marker_at, &
                          detach_marker
    use insert_delete_mod, only: insert
    use editing_functions_mod, only: interactive_region, point, point_minimum, point_maximum, goto_character, widen, &
                                     narrow_to_region, delete_region, buffer_substring, line_beginning_position, &
                                     line_end_position, following_character, is_end_of_buffer, is_beginning_of_buffer, &
                                     is_beginning_of_line, field_beginning, field_end, field_string_no_properties, &
                                     constrain_to_field, inhibit_field_text_motion, is_end_of_line
    use text_properties_mod, only: get_character_property, put_text_property, get_text_property, add_text_properties, &
                                   remove_list_of_text_properties, remove_text_properties, next_single_character_property_change, &
                                   previous_single_character_property_change
    use editing_commands_mod, only: delete_character, forward_character, forward_line, beginning_of_line, end_of_line, &
                                    self_insert_command
    use syntax_mod, only: skip_characters_forward
    use search_mod, only: looking_at, string_match, regular_expression_search_backward, match_beginning, match_end, &
                          regular_expression_quote, SEARCH_NOERROR, replace_match, case_fold_search
    use basic_subprograms_mod, only: silent_modifications_type, silent_modifications_begin, silent_modifications_end, match_string
    use hooks_mod, only: hook_type, string_hook_type, string_filter_hook_type
    use keymap_mod, only: keymap_set, lookup_key, key_description, command_remapping, current_global_map, &
                          make_sparse_keymap, is_keymap_live, set_repeat_map
    use modes_mod, only: major_modes
    use simple_mod, only: cut_region, copy_region_quietly, push_mark, cut_whole_line, just_one_space
    use process_mod, only: make_process, get_buffer_process, process_status, &
                           process_mark, process_buffer, delete_process, set_process_filter, process_send_string, &
                           process_send_region, process_send_eof, interrupt_process, stop_process, quit_process, &
                           accept_process_output, is_process, process_connection_type, PROCESS_CONNECTION_PIPE, &
                           PROCESS_CONNECTION_PTY, suspend_process, continue_process
    use window_mod, only: windows, window_type, window_point, set_window_point, set_window_start, recenter, &
                          window_leaves, window_text_columns, select_window, same_handle
    use frame_mod, only: selected_frame
    use display_engine_mod, only: message, set_mode_line_process, mode_line_part_type
    use font_core_mod, only: font_lock_defaults
    use font_lock_mod, only: font_lock_append_text_property, font_lock_remove_face_from_text_property
    use faces_mod, only: named_face_type, display_when, face_attributes
    use file_input_output_mod, only: is_file_readable, is_file_writable, is_file_accessible_directory, insert_file_contents, &
                                     write_region, expand_file_name
    use minibuffer_reading_mod, only: read_from_minibuffer, read_string
    use minibuffer_mod, only: read_file_name
    use paragraphs_mod, only: paragraph_start, forward_paragraph
    use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value, execute_interactively, this_command
    use keyboard_mod, only: last_command, this_command_keys
    use display_update_mod, only: ding
    use commands_mod, only: commands
    use ring_mod, only: ring_type
    use version_mod, only: upstream_version
    use incremental_search_mod, only: incremental_search_mode, incremental_search_forward, incremental_search_success, &
                                      incremental_search_error, incremental_search_nonincremental, &
                                      incremental_search_message_prefix, incremental_search_message_display, &
                                      incremental_search_search_procedure, incremental_search_pop_procedure, &
                                      incremental_search_search_function_default, incremental_search_search_function_function, &
                                      incremental_search_message_function, incremental_search_wrap_function, &
                                      incremental_search_push_state_function, incremental_search_mode_hook, &
                                      incremental_search_mode_end_hook, incremental_search_message_prefix_add, &
                                      set_incremental_search_barrier, set_incremental_search_original_point
    use interval_tree_mod, only: make_overlay, move_overlay, delete_overlay, overlay_put, is_overlay
    use keyboard_mod, only: pre_command_hook
    use authentication_source_mod, only: read_password
    use multilingual_environment_configuration_mod, only: password_word_equivalents, password_colon_equivalents, &
                                                          initialize_multilingual_environment_configuration
    use regular_expression_optimization_mod, only: regular_expression_optimization, REGULAR_EXPRESSION_OPTIMIZATION_GROUP
    use character_mod, only: character_string, MAXIMUM_MULTIBYTE_LENGTH
    use timer_mod, only: execute_at_time
    use value_mod, only: make_list, xstring, value_type
    use minibuffer_reading_tables_mod, only: completion_table_type, completion_test_procedure, completion_unquote_procedure, &
                                             completion_requote_procedure, completion_quote_procedure, &
                                             completion_table_with_quoting, completion_at_point_function_data_type, &
                                             COMPLETION_FINISHED, COMPLETE_TRY, COMPLETE_ALL, COMPLETE_TEST, &
                                             completion_list_table, completion_function_table
    use minibuffer_reading_mod, only: completion_ignore_case, quoting_table_call
    use minibuffer_mod, only: completion_in_region, completion_in_region_mode_test_function, cir_test_procedure, &
                              completions_visible, completion_at_point_functions, completion_file_name_table, &
                              read_file_name_completion_ignore_case, minibuffer_completion_table, &
                              minibuffer_completion_test_function, minibuffer_completion_help
    use hooks_mod, only: completion_at_point_function_hook_type, completion_at_point_function_entry_type
    use search_mod, only: set_match_data, match_data
    use syntax_mod, only: skip_characters_backward
    use editing_functions_mod, only: character_after, preceding_character
    use character_mod, only: string_character_and_length
    use utility_functions_mod, only: substring, count_characters
    use environment_mod, only: getenv
    use file_input_output_mod, only: is_file_name_absolute
    use directory_editor_mod, only: completion_ignored_extensions
    use regular_expression_optimization_mod, only: regular_expression_optimization_character_set
    use interval_tree_mod, only: destroy_overlay
    use string_type_mod, only: string_type, append_string, prepend_string
    implicit none(type, external)
    private

    public :: command_interpreter_mode, initialize_command_interpreter, command_interpreter_check_process, &
              make_command_interpreter_in_buffer, make_command_interpreter, command_interpreter_execute, &
              command_interpreter_exec, command_interpreter_term_environment, is_command_interpreter_nonblank, &
              command_interpreter_output_filter, command_interpreter_carriage_motion, &
              command_interpreter_send_input, command_interpreter_simple_send, command_interpreter_send_string, &
              command_interpreter_send_region, command_interpreter_snapshot_last_prompt, &
              command_interpreter_get_old_input_default, command_interpreter_copy_old_input, command_interpreter_skip_prompt, &
              is_command_interpreter_after_pmark, &
              command_interpreter_line_beginning_position, command_interpreter_beginning_of_line, &
              command_interpreter_add_to_input_history, command_interpreter_previous_input, &
              command_interpreter_next_input, command_interpreter_previous_matching_input, &
              command_interpreter_next_matching_input, &
              command_interpreter_previous_matching_input_from_input, command_interpreter_next_matching_input_from_input, &
              command_interpreter_restore_input, &
              command_interpreter_delete_input, command_interpreter_delete_output, command_interpreter_write_output, &
              command_interpreter_append_output_to_file, &
              command_interpreter_show_output, command_interpreter_show_maximum_output, command_interpreter_clear_buffer, &
              command_interpreter_truncate_buffer, &
              command_interpreter_strip_control_m, command_interpreter_interrupt_subjob, comint_stop_subjob, &
              command_interpreter_quit_subjob, command_interpreter_cut_input, &
              command_interpreter_delchar_or_maybe_eof, command_interpreter_send_eof, command_interpreter_backward_matching_input, &
              command_interpreter_forward_matching_input, command_interpreter_next_prompt, command_interpreter_previous_prompt, &
              command_interpreter_update_fence, &
              command_interpreter_cut_whole_line, command_interpreter_cut_region, command_interpreter_get_next_from_history, &
              command_interpreter_accumulate, &
              command_interpreter_goto_process_mark, command_interpreter_beginning_of_line_or_process_mark, &
              command_interpreter_set_process_mark, command_interpreter_read_input_ring, &
              command_interpreter_write_input_ring, command_interpreter_postoutput_scroll_to_bottom, &
              command_interpreter_adjust_window_point, &
              command_interpreter_input_ring_item, command_interpreter_input_ring_length, command_interpreter_last_input_start, &
              command_interpreter_last_input_end, &
              command_interpreter_last_output_start, &
              is_command_interpreter_pty, command_interpreter_arguments, command_interpreter_delim_argument, &
              command_interpreter_substitute_in_file_name, &
              command_interpreter_previous_input_string, command_interpreter_within_quotes, command_interpreter_how_many_region, &
              command_interpreter_replace_by_expanded_history, command_interpreter_c_a_p_replace_by_expanded_history, &
              command_interpreter_replace_by_expanded_history_before_point, command_interpreter_magic_space, &
              command_interpreter_insert_previous_argument, &
              command_interpreter_history_incremental_search_backward, &
              command_interpreter_history_search_backward_regular_expression, &
              command_interpreter_history_incremental_search_setup, &
              command_interpreter_history_incremental_search_end, command_interpreter_goto_input, &
              command_interpreter_preinput_scroll_to_bottom, command_interpreter_suspend_subjob, &
              command_interpreter_continue_subjob, command_interpreter_complete_input_ring, command_interpreter_word, &
              command_interpreter_match_partial_filename, &
              command_interpreter_quote_filename, command_interpreter_unquote_argument, command_interpreter_requote_argument, &
              command_interpreter_completion_file_name_table, command_interpreter_file_name_completion_data, &
              command_interpreter_filename_completion, &
              command_interpreter_completion_at_point, command_interpreter_dynamic_complete_filename, &
              command_interpreter_replace_by_expanded_filename, &
              command_interpreter_dynamic_list_filename_completions, &
              command_interpreter_directory
    public :: command_interpreter_input_sender_procedure, command_interpreter_get_old_input_procedure, &
              command_interpreter_input_filter_procedure, &
              command_interpreter_password_procedure
    public :: command_interpreter_send_invisible, command_interpreter_read_noecho, command_interpreter_watch_for_password_prompt, &
              command_interpreter_password_prompt_regular_expression_default

    abstract interface
        !> A `command_interpreter_input_sender': send to PROCESS the input STRING.
        !> Not a port.
        subroutine command_interpreter_input_sender_procedure(process, string, status)
            import :: handle_type, status_type
            implicit none(type, external)
            type(handle_type), intent(in) :: process
            character(len=*), intent(in) :: string
            type(status_type), intent(out) :: status
        end subroutine command_interpreter_input_sender_procedure

        !> A `command_interpreter_get_old_input' function: the old text at point, as
        !> INPUT.  Not a port.
        subroutine command_interpreter_get_old_input_procedure(input, status)
            import :: status_type
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: input
            type(status_type), intent(out) :: status
        end subroutine command_interpreter_get_old_input_procedure

        !> A `command_interpreter_input_filter': whether INPUT may be saved on the input
        !> history.  Not a port.
        logical function command_interpreter_input_filter_procedure(input)
            implicit none(type, external)
            character(len=*), intent(in) :: input
        end function command_interpreter_input_filter_procedure
    end interface

    abstract interface
        !> A `command_interpreter_password_function': the password for PROMPT, FOUND
        !> false if normal password prompting should occur.  Not a port.
        recursive subroutine command_interpreter_password_procedure(prompt, password, found)
            implicit none(type, external)
            character(len=*), intent(in) :: prompt
            character(len=:), allocatable, intent(out) :: password
            logical, intent(out) :: found
        end subroutine command_interpreter_password_procedure
    end interface

    !> Values of `command_interpreter_move_point_for_output' besides .true. and .false.
    integer, parameter, public :: COMMAND_INTERPRETER_MOVE_POINT_FOR_OUTPUT_ALL = 2, &
                                  COMMAND_INTERPRETER_MOVE_POINT_FOR_OUTPUT_THIS = 3, &
                                  COMMAND_INTERPRETER_MOVE_POINT_FOR_OUTPUT_OTHERS = 4

    !> Values of `command_interpreter_move_point_for_matching_input'.
    integer, parameter, public :: COMMAND_INTERPRETER_MOVE_POINT_FOR_MATCHING_INPUT_AFTER_INPUT = 0, &
                                  COMMAND_INTERPRETER_MOVE_POINT_FOR_MATCHING_INPUT_END_OF_LINE = 1

    !> Regexp to recognize prompts in the inferior process.
    !> Defaults to "^", the null string at BEGINNING_OF_LINE.
    !>
    !> This variable is only used if the variable
    !> `command_interpreter_use_prompt_regular_expression' is true.
    !>
    !> Good choices:
    !>   Canonical Lisp: "^[^> \n]*>+:? *" (Lucid, franz, kcl, T, cscheme, oaklisp)
    !>   Lucid Common Lisp: "^\\(>\\|\\(->\\)+\\) *"
    !>   franz: "^\\(->\\|<[0-9]*>:\\) *"
    !>   kcl: "^>+ *"
    !>   shell: "^[^#$%>\n]*[#$%>] *"
    !>   T: "^>+ *"
    !>
    !> This is a good thing to set in mode hooks.  Port of comint.el
    !> comint-prompt-regexp.
    type(string_setting_type), target, public :: command_interpreter_prompt_regular_expression = string_setting_type( &
                                                 name="command_interpreter_prompt_regular_expression")

    !> If true, the comint prompt is read only.
    !> The read only region includes the newline before the prompt.
    !> This does not affect existing prompts.
    !> Certain derived modes may override this option.
    !>
    !> If you set this option to true, then the safe way to temporarily
    !> override the read-only-ness of comint prompts is to call
    !> `command_interpreter_cut_whole_line' or `command_interpreter_cut_region' with no
    !> narrowing in effect.  This way you will be certain that none of
    !> the remaining prompts will be accidentally messed up.  Port of
    !> comint.el comint-prompt-read-only.
    type(logical_setting_type), target, public :: command_interpreter_prompt_read_only = &
                                                  logical_setting_type(name="command_interpreter_prompt_read_only", value=.false.)

    !> If true, highlight input with `command_interpreter_highlight_input' face.
    !> Otherwise keep the original highlighting untouched.  Port of
    !> comint.el comint-highlight-input.
    type(logical_setting_type), target, public :: command_interpreter_highlight_input = &
                                                  logical_setting_type(name="command_interpreter_highlight_input", value=.true.)

    !> If true, don't add input matching the last on the input ring.
    !> This mirrors the optional behavior of bash.
    !>
    !> This variable is buffer-local.  Port of comint.el
    !> comint-input-ignoredups.
    type(logical_setting_type), target, public :: command_interpreter_input_ignoredups = &
                                                  logical_setting_type(name="command_interpreter_input_ignoredups", value=.false.)

    !> If allocated, name of the file to read/write input history.
    !> See also `command_interpreter_read_input_ring' and `command_interpreter_write_input_ring'.
    !> `command_interpreter_mode' makes this a buffer-local variable.  You probably want
    !> to set this in a mode hook, rather than customize the default
    !> value.  Port of comint.el comint-input-ring-file-name.
    type(string_setting_type), target, public :: command_interpreter_input_ring_file_name = &
                                                 string_setting_type(name="command_interpreter_input_ring_file_name", &
                                                                     permanent_local=.true.)

    !> If allocated, the program to use for pagination of program output.
    !> If unallocated, use the default pager.
    !>
    !> Some programs produce large amounts of output, and have provision
    !> for pagination of their output through a filter program, commonly
    !> known as a "pager".  The pager limits the amount of output produced
    !> and allows the user to interactively browse the output one page at
    !> a time.  Some programs paginate their output by default, by always
    !> starting a pager.  The program they use as the pager is specified
    !> by the environment variable PAGER; if that variable is not defined,
    !> they use some fixed default, such as "less".
    !>
    !> The interactive browsing aspects of pagination are not needed, and
    !> get in the way, when the output of the program is directed to an
    !> Fortmacs buffer, so in those cases pagination might need to be
    !> disabled.  Disabling pagination means that some programs will
    !> produce large amounts of output, but most such programs have other
    !> ways to limit their output, such as additional arguments or Fortmacs
    !> interfaces.  To disable pagination, this variable's value should be
    !> a string that names a program, such as "cat", which passes through
    !> all of the output without any filtering or delays.  Comint will then
    !> set the PAGER variable to name that program, when it invokes
    !> external programs.  Port of comint.el comint-pager.
    type(string_setting_type), target, public :: command_interpreter_pager = string_setting_type(name="command_interpreter_pager")

    !> The prefix to skip when parsing the input ring file.
    !> This is useful in Zsh when the extended_history option is on.  Port
    !> of comint.el comint-input-ring-file-prefix.
    type(string_setting_type), target, public :: command_interpreter_input_ring_file_prefix = &
                                                 string_setting_type(name="command_interpreter_input_ring_file_prefix")

    !> Controls whether interpreter output moves point to the end of the
    !> output.
    !> If .false., then output never moves point to the output.
    !>  (If the output occurs at point, it is inserted before point.)
    !> If .true. or COMMAND_INTERPRETER_MOVE_POINT_FOR_OUTPUT_ALL, move point in all windows
    !> showing the buffer.
    !> If COMMAND_INTERPRETER_MOVE_POINT_FOR_OUTPUT_THIS, move point only the selected window.
    !> If COMMAND_INTERPRETER_MOVE_POINT_FOR_OUTPUT_OTHERS, move point only in other windows, not in the
    !> selected window.
    !>
    !> The default is .false.
    !>
    !> See the variable `command_interpreter_scroll_show_maximum_output' and the
    !> function `command_interpreter_postoutput_scroll_to_bottom'.
    !> This variable is buffer-local in all Comint buffers.  Port of
    !> comint.el comint-move-point-for-output.
    type(choice_setting_type), target, public :: command_interpreter_move_point_for_output = &
                                                 choice_setting_type(name="command_interpreter_move_point_for_output", &
                                                                     permanent_local=.true.)

    !> Controls where to place point after matching input.
    !> \<command_interpreter_mode_map>This influences the commands
    !> \[command_interpreter_previous_matching_input_from_input] and
    !> \[command_interpreter_next_matching_input_from_input].
    !> If COMMAND_INTERPRETER_MOVE_POINT_FOR_MATCHING_INPUT_AFTER_INPUT, point will be positioned after the
    !> input typed by the user, but before the rest of the history entry
    !> that has been inserted.  If COMMAND_INTERPRETER_MOVE_POINT_FOR_MATCHING_INPUT_END_OF_LINE, point will
    !> be positioned at the end of the current logical (not visual) line
    !> after insertion.  Port of comint.el
    !> comint-move-point-for-matching-input.
    type(integer_setting_type), target, public :: command_interpreter_move_point_for_matching_input = &
                                                  integer_setting_type(name="command_interpreter_move_point_for_matching_input", &
                                                                value=COMMAND_INTERPRETER_MOVE_POINT_FOR_MATCHING_INPUT_AFTER_INPUT)

    !> Controls how to scroll due to interpreter output.
    !> This variable applies when point is at the end of the buffer
    !> (either because it was originally there, or because
    !> `command_interpreter_move_point_for_output' said to move it there)
    !> and output from the subprocess is inserted.
    !>
    !> True means scroll so that the window is full of text
    !> and point is on the last line.  A value of false
    !> means don't do anything special--scroll normally.
    !>
    !> See also the variable `command_interpreter_move_point_for_output' and the function
    !> `command_interpreter_postoutput_scroll_to_bottom'.
    !> This variable is buffer-local in all Comint buffers.  Port of
    !> comint.el comint-scroll-show-maximum-output.
    type(logical_setting_type), target, public :: command_interpreter_scroll_show_maximum_output = &
                                                  logical_setting_type(name="command_interpreter_scroll_show_maximum_output", &
                                                                       permanent_local=.true., &
                                                                       value=.true.)

    !> The maximum size in lines for Comint buffers.
    !> Comint buffers are truncated from the top to be no greater than this
    !> number, if the function `command_interpreter_truncate_buffer' is on
    !> `command_interpreter_output_filter_functions'.  Port of comint.el
    !> comint-buffer-maximum-size.
    type(integer_setting_type), target, public :: command_interpreter_buffer_maximum_size = &
                                                  integer_setting_type(name="command_interpreter_buffer_maximum_size", value=1024)

    !> Size of the input history ring in `command_interpreter_mode'.  Port of comint.el
    !> comint-input-ring-size.
    type(integer_setting_type), target, public :: command_interpreter_input_ring_size = &
                                                  integer_setting_type(name="command_interpreter_input_ring_size", value=500)

    !> Separator between commands in the history file.  Port of comint.el
    !> comint-input-ring-separator.
    type(string_setting_type), target, public :: command_interpreter_input_ring_separator = &
                                                 string_setting_type(name="command_interpreter_input_ring_separator")

    !> Regexp for history entries that should be ignored when Comint
    !> initializes.  Port of comint.el comint-input-history-ignore.
    type(string_setting_type), target, public :: command_interpreter_input_history_ignore = &
                                                 string_setting_type(name="command_interpreter_input_history_ignore")

    !> If true, assume that the subprocess echoes any input.
    !> If so, delete one copy of the input so that only one copy eventually
    !> appears in the buffer.
    !>
    !> This variable is buffer-local.  Port of comint.el
    !> comint-process-echoes.
    type(logical_setting_type), target, public :: command_interpreter_process_echoes = &
                                                  logical_setting_type(name="command_interpreter_process_echoes", value=.false.)

    !> True means go to the end of the line before sending input.
    !> See `command_interpreter_send_input'.  Port of comint.el comint-eol-on-send.
    type(logical_setting_type), target, public :: command_interpreter_end_of_line_on_send = &
                                                  logical_setting_type(name="command_interpreter_end_of_line_on_send", value=.true.)

    !> If true, use `command_interpreter_prompt_regular_expression' to recognize prompts.
    !> If false, then program output and user-input are given different
    !> `field' properties, which Fortmacs commands can use to distinguish them
    !> (in particular, common movement commands such as
    !> `beginning_of_line' respect field boundaries in a natural way).
    !> Port of comint.el comint-use-prompt-regexp.
    type(logical_setting_type), target, public :: command_interpreter_use_prompt_regular_expression = &
                                                  logical_setting_type(name="command_interpreter_use_prompt_regular_expression", &
                                                                       value=.false.)

    !> Values of `command_interpreter_input_autoexpand' besides .true. and .false.
    integer, parameter, public :: COMMAND_INTERPRETER_INPUT_AUTOEXPAND_INPUT = 2, COMMAND_INTERPRETER_INPUT_AUTOEXPAND_HISTORY = 3

    !> If true, expand input command history
    !> references on completion.  This mirrors the optional behavior of
    !> tcsh (its autoexpand and histlist).
    !>
    !> If the value is COMMAND_INTERPRETER_INPUT_AUTOEXPAND_INPUT, then the expansion is seen
    !> on input.  If the value is COMMAND_INTERPRETER_INPUT_AUTOEXPAND_HISTORY, then the
    !> expansion is only when inserting into the buffer's input ring.
    !> See also `command_interpreter_magic_space' and `completion_at_point'.
    !>
    !> This variable is buffer-local.  Port of comint.el
    !> comint-input-autoexpand.
    type(choice_setting_type), target, public :: command_interpreter_input_autoexpand = &
                                                 choice_setting_type(name="command_interpreter_input_autoexpand", &
                                                                     permanent_local=.true.)

    !> Values of `command_interpreter_scroll_to_bottom_on_input' besides .true. and .false.
    integer, parameter, public :: COMMAND_INTERPRETER_SCROLL_TO_BOTTOM_ON_INPUT_ALL = 2, &
                                  COMMAND_INTERPRETER_SCROLL_TO_BOTTOM_ON_INPUT_THIS = 3

    !> Controls whether input to interpreter causes window to scroll.
    !> If .false., then do not scroll.  If .true. or
    !> COMMAND_INTERPRETER_SCROLL_TO_BOTTOM_ON_INPUT_ALL, scroll all windows showing
    !> buffer.  If COMMAND_INTERPRETER_SCROLL_TO_BOTTOM_ON_INPUT_THIS, scroll only
    !> the selected window.
    !>
    !> The default is .false.
    !>
    !> See `command_interpreter_preinput_scroll_to_bottom'.  This variable is
    !> buffer-local.  Port of comint.el comint-scroll-to-bottom-on-input.
    type(choice_setting_type), target, public :: command_interpreter_scroll_to_bottom_on_input = &
                                                 choice_setting_type(name="command_interpreter_scroll_to_bottom_on_input", &
                                                                     permanent_local=.true.)

    !> If true, `command_interpreter_insert_previous_argument' counts args from the
    !> end.  If this variable is false, the default,
    !> `command_interpreter_insert_previous_argument' counts the arguments from the
    !> beginning; if true, it counts from the end instead.  This emulates
    !> the behavior of `ESC-NUM ESC-.' in both Bash and zsh: in Bash,
    !> `number' counts from the beginning (variable is false), while in
    !> zsh, it counts from the end.  Port of comint.el
    !> comint-insert-previous-argument-from-end.
    type(logical_setting_type), target, public :: command_interpreter_insert_previous_argument_from_end = &
                                                  logical_setting_type( &
                                                  name="command_interpreter_insert_previous_argument_from_end", &
                                                  value=.false.)

    !> Values of `command_interpreter_history_incremental_search' besides .true. and .false.
    integer, parameter, public :: COMMAND_INTERPRETER_HISTORY_INCREMENTAL_SEARCH_DO_WHAT_I_MEAN = 2

    !> If true, Isearch in input history only,
    !> not in comint buffer output.  If .true., usual
    !> Isearch keys like \`C-r' and \`C-M-r' in comint mode search in the
    !> input history.  If COMMAND_INTERPRETER_HISTORY_INCREMENTAL_SEARCH_DO_WHAT_I_MEAN, Isearch keys search
    !> in the input history only when initial point position is at the
    !> comint command line.  When starting Isearch from other parts of the
    !> comint buffer, they search in the comint buffer.  If .false.,
    !> Isearch operates on the whole comint
    !> buffer.  Port of comint.el comint-history-isearch.
    type(choice_setting_type), target, public :: command_interpreter_history_incremental_search = &
                                                 choice_setting_type(name="command_interpreter_history_incremental_search")

    !> Value to use for TERM when the system uses terminfo.
    !> If the system's terminfo database contains a definition for the
    !> "dumb-emacs-ansi" terminal (as all recent versions of terminfo do),
    !> set this to "dumb-emacs-ansi" and then some terminfo-aware programs
    !> will send colorized output when run under Comint.  Port of comint.el
    !> comint-terminfo-terminal.
    type(string_setting_type), target, public :: command_interpreter_terminfo_terminal = &
                                                 string_setting_type(name="command_interpreter_terminfo_terminal")

    !> If false, Comint will interpret `carriage control' characters in
    !> output.  See `command_interpreter_carriage_motion' for details.  Port of
    !> comint.el comint-inhibit-carriage-motion.
    type(logical_setting_type), target, public :: command_interpreter_inhibit_carriage_motion = &
                                                  logical_setting_type(name="command_interpreter_inhibit_carriage_motion", &
                                                                       value=.false.)

    !> The characters to recognize as separate arguments in input.
    !> Strings comprising a character in this list will separate the
    !> arguments surrounding them, and also be regarded as arguments in
    !> their own right (unlike whitespace).  See `command_interpreter_arguments'.
    !> Defaults to the empty string.
    !>
    !> For shells, a good value is "|&<>();".
    !>
    !> This is a good thing to set in mode hooks.  Port of comint.el
    !> comint-delimiter-argument-list (the characters as a string).
    type(string_setting_type), target, public :: command_interpreter_delimiter_arguments = &
                                                 string_setting_type(name="command_interpreter_delimiter_arguments")

    !> True directs the `command_interpreter_input_sender' function not to send a
    !> newline.  Port of comint.el comint-input-sender-no-newline.
    logical, public :: command_interpreter_input_sender_no_newline = .false.

    !> Hook run upon entry to `command_interpreter_mode'.
    !> This is run before the process is cranked up.  Port of comint.el
    !> comint-mode-hook.
    type(hook_type), target, public :: command_interpreter_mode_hook = hook_type(name="command_interpreter_mode_hook")

    !> Hook run each time a process is exec'd by `command_interpreter_exec'.
    !> This is called after the process is cranked up.  It is useful for
    !> things that must be done each time a process is executed in a
    !> Comint mode buffer (e.g., `set_process_query_on_exit_flag').  In
    !> contrast, `command_interpreter_mode_hook' is only executed once, when the buffer
    !> is created.  Port of comint.el comint-exec-hook.
    type(hook_type), target, public :: command_interpreter_exec_hook = hook_type(name="command_interpreter_exec_hook")

    !> Functions to call after output is inserted into the buffer.
    !> One possible function is `command_interpreter_postoutput_scroll_to_bottom'.
    !> These functions get one argument, a string containing the text as
    !> originally inserted.  Note that this might not be the same as the
    !> buffer contents between `command_interpreter_last_output_start' and the buffer's
    !> `process_mark', if other filter functions have already modified the
    !> buffer.
    !>
    !> See also `command_interpreter_preoutput_filter_functions'.
    !>
    !> You can use `add' to add functions to this hook either globally or
    !> locally.  Port of comint.el comint-output-filter-functions
    !> (ansi_color_mod adds `ansi_color_process_output').
    type(string_hook_type), target, public :: command_interpreter_output_filter_functions = &
                                              string_hook_type(name="command_interpreter_output_filter_functions", &
                                                               permanent_local=.true.)

    !> Hook of functions to call before inserting Comint output into the
    !> buffer.  Each function gets one argument, a string containing the
    !> text received from the subprocess.  It should return the string to
    !> insert, perhaps the same string that was received, or perhaps a
    !> modified or transformed string.
    !>
    !> The functions are called sequentially, and each one is given the
    !> string returned by the previous one.  The string returned by the
    !> last function is the text that is actually inserted in the
    !> redirection buffer.
    !>
    !> You can use `add' to add functions to this hook either globally or
    !> locally.  Port of comint.el comint-preoutput-filter-functions.
    type(string_filter_hook_type), target, public :: command_interpreter_preoutput_filter_functions = &
                                                     string_filter_hook_type( &
                                                     name="command_interpreter_preoutput_filter_functions", &
                                                     permanent_local=.true.)

    !> Abnormal hook run before input is sent to the process.
    !> These functions get one argument, a string containing the text to
    !> send.  Port of comint.el comint-input-filter-functions.
    type(string_hook_type), target, public :: command_interpreter_input_filter_functions = &
                                              string_hook_type(name="command_interpreter_input_filter_functions", &
                                                               permanent_local=.true.)

    !> Keymap for Comint mode.  Port of comint.el comint-mode-map (the
    !> keys of ported commands).
    type(handle_type), public :: command_interpreter_mode_map

    !> Keymap to repeat comint key sequences.  Used in `repeat_mode'.
    !> Port of comint.el comint-repeat-map.
    type(handle_type), public :: command_interpreter_repeat_map

    !> Face to use to highlight user input: the face `command_interpreter_highlight_input'
    !> (the variable of that name is the setting).  Port of
    !> comint.el:comint-highlight-input (the face).
    type(named_face_type), target, public :: command_interpreter_highlight_input_face

    !> Face to use to highlight prompts.  Port of
    !> comint.el:comint-highlight-prompt.
    type(named_face_type), target, public :: command_interpreter_highlight_prompt

    !> A Comint buffer's own state: upstream's buffer-local variables
    !> One buffer's own `command_interpreter_get_old_input'.
    type :: command_interpreter_get_old_input_entry_type
        procedure(command_interpreter_get_old_input_procedure), pointer, nopass :: procedure => null()
    end type command_interpreter_get_old_input_entry_type

    !> The type of `command_interpreter_get_old_input': the global value `procedure' (set with `=>')
    !> and the buffers' own values (`set_local').
    type, extends(buffer_local_setting_type), public :: command_interpreter_get_old_input_setting_type
        procedure(command_interpreter_get_old_input_procedure), pointer, nopass :: procedure => null()
        type(command_interpreter_get_old_input_entry_type), allocatable :: locals(:)
    contains
        !> Give the current buffer its own value (null when absent).
        procedure :: set_local => set_command_interpreter_get_old_input_local
        !> The current buffer's value: its own, else the global one.
        procedure :: get_current => get_command_interpreter_get_old_input_current
        procedure :: append_local_entry => append_command_interpreter_get_old_input_entry
        procedure :: clear_local_entry => clear_command_interpreter_get_old_input_entry
    end type command_interpreter_get_old_input_setting_type

    !> One buffer's own `command_interpreter_input_sender'.
    type :: command_interpreter_input_sender_entry_type
        procedure(command_interpreter_input_sender_procedure), pointer, nopass :: procedure => null()
    end type command_interpreter_input_sender_entry_type

    !> The type of `command_interpreter_input_sender': the global value `procedure' (set with `=>')
    !> and the buffers' own values (`set_local').
    type, extends(buffer_local_setting_type), public :: command_interpreter_input_sender_setting_type
        procedure(command_interpreter_input_sender_procedure), pointer, nopass :: procedure => null()
        type(command_interpreter_input_sender_entry_type), allocatable :: locals(:)
    contains
        !> Give the current buffer its own value (null when absent).
        procedure :: set_local => set_command_interpreter_input_sender_local
        !> The current buffer's value: its own, else the global one.
        procedure :: get_current => get_command_interpreter_input_sender_current
        procedure :: append_local_entry => append_command_interpreter_input_sender_entry
        procedure :: clear_local_entry => clear_command_interpreter_input_sender_entry
    end type command_interpreter_input_sender_setting_type

    !> One buffer's own `command_interpreter_input_filter'.
    type :: command_interpreter_input_filter_entry_type
        procedure(command_interpreter_input_filter_procedure), pointer, nopass :: procedure => null()
    end type command_interpreter_input_filter_entry_type

    !> The type of `command_interpreter_input_filter': the global value `procedure' (set with `=>')
    !> and the buffers' own values (`set_local').
    type, extends(buffer_local_setting_type), public :: command_interpreter_input_filter_setting_type
        procedure(command_interpreter_input_filter_procedure), pointer, nopass :: procedure => null()
        type(command_interpreter_input_filter_entry_type), allocatable :: locals(:)
    contains
        !> Give the current buffer its own value (null when absent).
        procedure :: set_local => set_command_interpreter_input_filter_local
        !> The current buffer's value: its own, else the global one.
        procedure :: get_current => get_command_interpreter_input_filter_current
        procedure :: append_local_entry => append_command_interpreter_input_filter_entry
        procedure :: clear_local_entry => clear_command_interpreter_input_filter_entry
    end type command_interpreter_input_filter_setting_type

    !> One buffer's own `command_interpreter_password_function'.
    type :: command_interpreter_password_entry_type
        procedure(command_interpreter_password_procedure), pointer, nopass :: procedure => null()
    end type command_interpreter_password_entry_type

    !> The type of `command_interpreter_password_function': the global value `procedure' (set with `=>')
    !> and the buffers' own values (`set_local').
    type, extends(buffer_local_setting_type), public :: command_interpreter_password_setting_type
        procedure(command_interpreter_password_procedure), pointer, nopass :: procedure => null()
        type(command_interpreter_password_entry_type), allocatable :: locals(:)
    contains
        !> Give the current buffer its own value (null when absent).
        procedure :: set_local => set_command_interpreter_password_local
        !> The current buffer's value: its own, else the global one.
        procedure :: get_current => get_command_interpreter_password_current
        procedure :: append_local_entry => append_command_interpreter_password_entry
        procedure :: clear_local_entry => clear_command_interpreter_password_entry
    end type command_interpreter_password_setting_type

    !> One buffer's own `command_interpreter_unquote_function'.
    type :: command_interpreter_unquote_entry_type
        procedure(completion_unquote_procedure), pointer, nopass :: procedure => null()
    end type command_interpreter_unquote_entry_type

    !> The type of `command_interpreter_unquote_function': the global value `procedure' (set with `=>')
    !> and the buffers' own values (`set_local').
    type, extends(buffer_local_setting_type), public :: command_interpreter_unquote_setting_type
        procedure(completion_unquote_procedure), pointer, nopass :: procedure => null()
        type(command_interpreter_unquote_entry_type), allocatable :: locals(:)
    contains
        !> Give the current buffer its own value (null when absent).
        procedure :: set_local => set_command_interpreter_unquote_local
        !> The current buffer's value: its own, else the global one.
        procedure :: get_current => get_command_interpreter_unquote_current
        procedure :: append_local_entry => append_command_interpreter_unquote_entry
        procedure :: clear_local_entry => clear_command_interpreter_unquote_entry
    end type command_interpreter_unquote_setting_type

    !> One buffer's own `command_interpreter_requote_function'.
    type :: command_interpreter_requote_entry_type
        procedure(completion_requote_procedure), pointer, nopass :: procedure => null()
    end type command_interpreter_requote_entry_type

    !> The type of `command_interpreter_requote_function': the global value `procedure' (set with `=>')
    !> and the buffers' own values (`set_local').
    type, extends(buffer_local_setting_type), public :: command_interpreter_requote_setting_type
        procedure(completion_requote_procedure), pointer, nopass :: procedure => null()
        type(command_interpreter_requote_entry_type), allocatable :: locals(:)
    contains
        !> Give the current buffer its own value (null when absent).
        procedure :: set_local => set_command_interpreter_requote_local
        !> The current buffer's value: its own, else the global one.
        procedure :: get_current => get_command_interpreter_requote_current
        procedure :: append_local_entry => append_command_interpreter_requote_entry
        procedure :: clear_local_entry => clear_command_interpreter_requote_entry
    end type command_interpreter_requote_setting_type

    !> One buffer's own `command_interpreter_completion_fignore'.
    type :: command_interpreter_completion_fignore_entry_type
        character(len=8), allocatable :: suffixes(:)
    end type command_interpreter_completion_fignore_entry_type

    !> The type of `command_interpreter_completion_fignore': the global
    !> value `suffixes' (set with `=') and the buffers' own values
    !> (`set_local').
    type, extends(buffer_local_setting_type), public :: command_interpreter_completion_fignore_setting_type
        character(len=8), allocatable :: suffixes(:)
        type(command_interpreter_completion_fignore_entry_type), allocatable :: locals(:)
    contains
        !> Give the current buffer its own value, SUFFIXES.
        procedure :: set_local => set_command_interpreter_completion_fignore_local
        !> The current buffer's value: its own, else the global one.
        procedure :: get_current => get_command_interpreter_completion_fignore_current
        procedure :: append_local_entry => append_command_interpreter_completion_fignore_entry
        procedure :: clear_local_entry => clear_command_interpreter_completion_fignore_entry
        procedure, private :: assign_command_interpreter_completion_fignore
        !> Set the global value.
        generic :: assignment(=) => assign_command_interpreter_completion_fignore
    end type command_interpreter_completion_fignore_setting_type

    !> Function that returns old text in Comint mode.
    !> This function is called when return is typed while the point is in old
    !> text.  It returns the text to be submitted as process input.  The
    !> default is `command_interpreter_get_old_input_default', which either grabs the
    !> current input field or grabs the current line and strips off leading
    !> text matching `command_interpreter_prompt_regular_expression', depending on the value of
    !> `command_interpreter_use_prompt_regular_expression'.  Port of comint.el
    !> comint-get-old-input (`%procedure' is the global value, `%set_local'
    !> gives a buffer its own).
    type(command_interpreter_get_old_input_setting_type), public :: command_interpreter_get_old_input

    !> Function to actually send to PROCESS the STRING submitted by user.
    !> Usually this is just `command_interpreter_simple_send', but if your mode needs to
    !> massage the input string, put a different function here.
    !> `command_interpreter_simple_send' just sends the string plus a newline.
    !> (If `command_interpreter_input_sender_no_newline' is true, it omits the newline.)
    !> This is called from the user command `command_interpreter_send_input'.  Port of
    !> comint.el comint-input-sender (`%procedure' is the global value,
    !> `%set_local' gives a buffer its own).
    type(command_interpreter_input_sender_setting_type), public :: command_interpreter_input_sender

    !> Test for filtering additions to input history.
    !> Takes one argument, the input.  If true, the input may be saved on the input
    !> history list.  Default is to save anything that isn't all whitespace.
    !> Port of comint.el comint-input-filter (`%procedure' is the global
    !> value, `%set_local' gives a buffer its own).
    type(command_interpreter_input_filter_setting_type), public :: command_interpreter_input_filter

    !> Abnormal hook run when prompted for a password.
    !> This function gets one argument, a string containing the prompt.
    !> It may return a string containing the password (FOUND .true.), or
    !> FOUND .false. if normal password prompting should occur.  Port of
    !> comint.el comint-password-function (always buffer-local:
    !> `%set_local'; null for none).
    type(command_interpreter_password_setting_type), public :: command_interpreter_password_function

    !> Function to use for completion of quoted data.
    !> See `completion_table_with_quoting' and `command_interpreter_requote_function'.
    !> Port of comint.el comint-unquote-function (`%procedure' is the
    !> global value, `%set_local' gives a buffer its own).
    type(command_interpreter_unquote_setting_type), public :: command_interpreter_unquote_function

    !> Function to use for completion of quoted data.
    !> See `completion_table_with_quoting' and `command_interpreter_unquote_function'.
    !> Port of comint.el comint-requote-function (`%procedure' is the
    !> global value, `%set_local' gives a buffer its own).
    type(command_interpreter_requote_setting_type), public :: command_interpreter_requote_function

    !> comint-input-ring, comint-input-ring-index,
    !> comint-save-input-ring-index, comint-ptyp (permanent), and
    !> comint-last-input-start, comint-last-input-end,
    !> comint-last-output-start, comint-accum-marker, comint-last-prompt,
    !> comint-stored-incomplete-input,
    !> comint-matching-input-from-input-string.
    type :: command_interpreter_buffer_type
        integer(int64) :: serial = 0
        type(handle_type) :: buffer
        type(ring_type) :: input_ring
        logical :: has_ring = .false.
        !> -1 for none.
        integer :: input_ring_index = -1, save_input_ring_index = -1
        logical :: ptyp = .true.
        !> The variables comint-mode sets (killed with the local variables).
        logical :: local = .false.
        type(handle_type) :: last_input_start, last_input_end, last_output_start, accum_marker
        logical :: has_last_prompt = .false.
        type(handle_type) :: last_prompt_start, last_prompt_end
        character(len=:), allocatable :: stored_incomplete_input
        character(len=:), allocatable :: matching_input_from_input_string
        !> comint-insert-previous-argument-last-start-pos and
        !> comint-insert-previous-argument-last-index.
        type(handle_type) :: insert_previous_argument_last_start_position
        integer :: insert_previous_argument_last_index = 0
        logical :: has_insert_previous_argument_last_index = .false.
        !> comint-history-isearch-message-overlay.
        type(handle_type) :: history_incremental_search_message_overlay
    end type command_interpreter_buffer_type

    !> Regexp matching prompts for passwords in the inferior process.
    !> This is used by `command_interpreter_watch_for_password_prompt'.  Port of
    !> comint.el comint-password-prompt-regexp (made by
    !> `command_interpreter_password_prompt_regular_expression_default' when Comint is set up).
    character(len=:), allocatable, public :: command_interpreter_password_prompt_regular_expression

    !> The maximum amount of text to examine when matching password
    !> prompts.  This is used by `command_interpreter_watch_for_password_prompt' to
    !> reduce the amount of time spent searching for password prompts.
    !> Port of comint.el comint-password-prompt-max-length.
    integer, public :: command_interpreter_password_prompt_maximum_length = 256

    ! comint--prompt-recursion-depth.
    integer :: command_interpreter_prompt_recursion_depth = 0

    ! A password prompt waiting for the timer `command_interpreter_watch_for_password_prompt'
    ! starts (the arguments of its lambda).
    type :: pending_prompt_type
        type(handle_type) :: buffer
        character(len=:), allocatable :: prompt
    end type pending_prompt_type

    type(pending_prompt_type), allocatable :: pending_prompts(:)

    !> True means to force searching in input history.
    !> If false, respect the option `command_interpreter_history_incremental_search'.  Port of
    !> comint.el comint--force-history-isearch.
    logical :: command_interpreter_force_history_incremental_search = .false.

    ! What comint-replace-by-expanded-history-before-point throws to
    ! DRY-RUN: nothing, `message', `history' or `expand'.
    integer, parameter :: DRY_RUN_NONE = 0, DRY_RUN_MESSAGE = 1, DRY_RUN_HISTORY = 2, DRY_RUN_EXPAND = 3

    type(command_interpreter_buffer_type), allocatable :: command_interpreter_buffers(:)

    !> comint--prompt-rear-nonsticky: text properties set on the prompt
    !> that are not to leak past it.
    character(len=*), parameter :: PROMPT_REAR_NONSTICKY(5) = [character(len=31) :: "field", &
                                                                                   "inhibit_line_move_field_capture", "read_only", &
                                                                                          "font_lock_face", "insert_in_front_hooks"]

    !> If true, automatically list possibilities on partial completion.
    !> This mirrors the optional behavior of tcsh.  Port of comint.el
    !> comint-completion-autolist.
    type(logical_setting_type), target, public :: command_interpreter_completion_autolist = &
                                                  logical_setting_type(name="command_interpreter_completion_autolist", &
                                                                       value=.false.)

    !> The string FILESUFFIX added to file names on unambiguous or exact
    !> completion (" " by default; the empty string adds none).
    !> This mirrors the optional behavior of tcsh.  Port of comint.el
    !> comint-completion-addsuffix (.true. is " ", .false. is "", and the obsolete
    !> two values, DIRSUFFIX and FILESUFFIX, give FILESUFFIX).
    type(string_setting_type), target, public :: command_interpreter_completion_addsuffix = &
                                                 string_setting_type(name="command_interpreter_completion_addsuffix")

    !> If true, use shortest completion if characters cannot be added.
    !> This mirrors the optional behavior of tcsh.
    !>
    !> A true value is useful if `command_interpreter_completion_autolist' is true too.
    !> Port of comint.el comint-completion-recexact.
    type(logical_setting_type), target, public :: command_interpreter_completion_recexact = &
                                                  logical_setting_type(name="command_interpreter_completion_recexact", &
                                                                       value=.false.)

    !> List of suffixes to be disregarded during file completion.
    !> This mirrors the optional behavior of bash and tcsh.
    !>
    !> Note that this applies to `command_interpreter_dynamic_complete_filename' only.
    !> Port of comint.el comint-completion-fignore (at most as many
    !> suffixes as `completion_ignored_extensions' has entries; a Comint
    !> buffer gets its own with `%set_local').
    type(command_interpreter_completion_fignore_setting_type), public :: command_interpreter_completion_fignore

    !> Prefix prepended to absolute file names taken from process input.
    !> This is used by Comint's and shell's completion functions, and by
    !> shell's directory tracking functions.  Port of comint.el
    !> comint-file-name-prefix.
    type(string_setting_type), target, public :: command_interpreter_file_name_prefix = string_setting_type( &
                                                 name="command_interpreter_file_name_prefix")

    !> String of characters valid in a file name.
    !> Note that all non-ASCII characters are considered valid in a file
    !> name regardless of what this variable says.
    !>
    !> This is a good thing to set in mode hooks.  Port of comint.el
    !> comint-file-name-chars.
    type(string_setting_type), target, public :: command_interpreter_file_name_characters = string_setting_type( &
                                                 name="command_interpreter_file_name_characters")

    !> The characters to quote with `\' when in a file name.
    !>
    !> This is a good thing to set in mode hooks.  Port of comint.el
    !> comint-file-name-quote-list (a string of the characters).
    type(string_setting_type), target, public :: command_interpreter_file_name_quote_characters = &
                                                 string_setting_type(name="command_interpreter_file_name_quote_characters")

    !> List of functions called to perform completion.
    !> Works like `completion_at_point_functions'.
    !> See also `completion_at_point'.
    !>
    !> This is a good thing to set in mode hooks.  Port of comint.el
    !> comint-dynamic-complete-functions.
    type(completion_at_point_function_hook_type), target, public :: command_interpreter_dynamic_complete_functions = &
                                                                    completion_at_point_function_hook_type( &
                                                                    name="command_interpreter_dynamic_complete_functions")

contains

    ! ------------------------------------------------------------------
    ! The buffers' own state
    ! ------------------------------------------------------------------

    !> The entry of buffer slot BI (0 for none).  Not a port.
    integer function entry_of(bi) result(k)
        integer(int64), intent(in) :: bi

        k = 0
        if (.not. allocated(command_interpreter_buffers) .or. bi <= 0) return
        k = findloc(command_interpreter_buffers%serial, buffers(bi)%serial, dim=1)
    end function entry_of

    !> Find or make (empty) the entry of the current buffer, its index K.
    !> Entries of dead buffers are dropped then.  Not a port.
    subroutine make_current_entry(k)
        integer, intent(out) :: k
        type(command_interpreter_buffer_type), allocatable :: kept(:)
        integer :: i, n

        k = entry_of(current)
        if (k /= 0) return
        if (.not. allocated(command_interpreter_buffers)) allocate (command_interpreter_buffers(0))
        n = 0
        allocate (kept(size(command_interpreter_buffers) + 1))

        do i = 1, size(command_interpreter_buffers)
            if (is_buffer_live(command_interpreter_buffers(i)%buffer)) then
                n = n + 1
                call move_entry(command_interpreter_buffers(i), kept(n))
            else
                call clear_local(command_interpreter_buffers(i))
            end if
        end do

        n = n + 1
        kept(n)%serial = buffers(current)%serial
        kept(n)%buffer = current_buffer()
        call move_alloc(kept, command_interpreter_buffers)
        command_interpreter_buffers = command_interpreter_buffers(1:n)
        k = n
    end subroutine make_current_entry

    ! Not a port.
    subroutine move_entry(from, to)
        type(command_interpreter_buffer_type), intent(inout) :: from
        type(command_interpreter_buffer_type), intent(out) :: to

        to = from
    end subroutine move_entry

    !> Free the markers of entry E and forget its local variables.  Not a port.
    subroutine clear_local(e)
        type(command_interpreter_buffer_type), intent(inout) :: e
        type(status_type) :: st

        if (.not. e%local) return
        call destroy_marker(e%last_input_start, st)
        call destroy_marker(e%last_input_end, st)
        call destroy_marker(e%last_output_start, st)
        call destroy_marker(e%accum_marker, st)
        call forget_last_prompt(e)
        if (allocated(e%stored_incomplete_input)) deallocate (e%stored_incomplete_input)
        e%matching_input_from_input_string = ""

        if (e%insert_previous_argument_last_start_position%index /= 0) then
            call destroy_marker(e%insert_previous_argument_last_start_position, st)
        end if

        e%insert_previous_argument_last_start_position = null_handle()
        e%has_insert_previous_argument_last_index = .false.
        e%history_incremental_search_message_overlay = null_handle()
        e%local = .false.
    end subroutine clear_local

    ! Not a port.
    subroutine forget_last_prompt(e)
        type(command_interpreter_buffer_type), intent(inout) :: e
        type(status_type) :: st

        if (.not. e%has_last_prompt) return
        call destroy_marker(e%last_prompt_start, st)
        call destroy_marker(e%last_prompt_end, st)
        e%has_last_prompt = .false.
    end subroutine forget_last_prompt

    !> Forget the current buffer's local Comint variables (not the
    !> permanent ones), as `clear_all_local_variables' does.  Not a port.
    subroutine clear_command_interpreter_locals()
        integer :: k

        k = entry_of(current)
        if (k /= 0) call clear_local(command_interpreter_buffers(k))
        call command_interpreter_get_old_input%clear_local()
        call command_interpreter_input_sender%clear_local()
        call command_interpreter_input_filter%clear_local()
        call command_interpreter_password_function%clear_local()
        call command_interpreter_completion_fignore%clear_local()
        call command_interpreter_unquote_function%clear_local()
        call command_interpreter_requote_function%clear_local()
    end subroutine clear_command_interpreter_locals

    !> Find or make the current buffer's entry with its local variables
    !> (made as comint-mode makes them if the buffer has none), its index
    !> K.  Not a port.
    subroutine make_local_entry(k)
        integer, intent(out) :: k

        call make_current_entry(k)
        if (.not. command_interpreter_buffers(k)%local) call make_locals(k)
    end subroutine make_local_entry

    !> Make the local variables of entry K, as comint-mode does.  Not a port.
    subroutine make_locals(k)
        integer, intent(in) :: k
        procedure(command_interpreter_get_old_input_procedure), pointer :: get_old_input
        procedure(command_interpreter_input_sender_procedure), pointer :: input_sender
        procedure(command_interpreter_input_filter_procedure), pointer :: input_filter
        type(status_type) :: st

        associate (e => command_interpreter_buffers(k))
            call copy_marker_at(point_minimum(), e%last_input_start, .false., st)
            call copy_marker_at(point_minimum(), e%last_input_end, .false., st)
            call make_marker(e%last_output_start)
            call make_marker(e%accum_marker)
            e%has_last_prompt = .false.
            e%matching_input_from_input_string = ""
            e%local = .true.
        end associate

        ! (make-local-variable: the global values, unless set already.)
        get_old_input => command_interpreter_get_old_input%get_current()
        if (.not. command_interpreter_get_old_input%is_local()) call command_interpreter_get_old_input%set_local(get_old_input)
        input_sender => command_interpreter_input_sender%get_current()
        if (.not. command_interpreter_input_sender%is_local()) call command_interpreter_input_sender%set_local(input_sender)
        input_filter => command_interpreter_input_filter%get_current()
        if (.not. command_interpreter_input_filter%is_local()) call command_interpreter_input_filter%set_local(input_filter)

        if (.not. command_interpreter_completion_fignore%is_local()) then
            call command_interpreter_completion_fignore%set_local(command_interpreter_completion_fignore%get_current())
        end if
    end subroutine make_locals

    !> Whether the current buffer has a local input ring (comint-input-ring).
    !> Not a port.
    logical function has_input_ring()
        integer :: k

        has_input_ring = .false.
        k = entry_of(current)
        if (k /= 0) has_input_ring = command_interpreter_buffers(k)%has_ring
    end function has_input_ring

    !> The number of items of the current buffer's input ring.  Not a port.
    integer function command_interpreter_input_ring_length()
        integer :: k

        command_interpreter_input_ring_length = 0
        k = entry_of(current)
        if (k == 0) return
        if (command_interpreter_buffers(k)%has_ring) command_interpreter_input_ring_length = command_interpreter_buffers( &
                                                                                             k)%input_ring%count()
    end function command_interpreter_input_ring_length

    !> Item INDEX (0 the newest) of the current buffer's input ring ("" if
    !> it has none).  Not a port.
    function command_interpreter_input_ring_item(index) result(item)
        integer, intent(in) :: index
        character(len=:), allocatable :: item
        type(status_type) :: st
        integer :: k

        item = ""
        k = entry_of(current)
        if (k == 0) return
        if (command_interpreter_buffers(k)%has_ring) call command_interpreter_buffers(k)%input_ring%reference(index, item, st)
    end function command_interpreter_input_ring_item

    !> Position of a marker of the current buffer's state (0 if unset).
    !> Not a port.
    integer(int64) function marked_position(m) result(position)
        type(handle_type), intent(in) :: m
        type(status_type) :: st

        position = 0
        if (m%index == 0) return
        call marker_position(m, position, st)
        if (.not. st%ok()) position = 0
    end function marked_position

    !> The position of comint-last-input-start in the current buffer.
    !> Not a port.
    integer(int64) function command_interpreter_last_input_start()
        integer :: k

        command_interpreter_last_input_start = 0
        k = entry_of(current)
        if (k /= 0) command_interpreter_last_input_start = marked_position(command_interpreter_buffers(k)%last_input_start)
    end function command_interpreter_last_input_start

    !> The position of comint-last-input-end in the current buffer.
    !> Not a port.
    integer(int64) function command_interpreter_last_input_end()
        integer :: k

        command_interpreter_last_input_end = 0
        k = entry_of(current)
        if (k /= 0) command_interpreter_last_input_end = marked_position(command_interpreter_buffers(k)%last_input_end)
    end function command_interpreter_last_input_end

    !> The position of comint-last-output-start in the current buffer.
    !> Not a port.
    integer(int64) function command_interpreter_last_output_start()
        integer :: k

        command_interpreter_last_output_start = 0
        k = entry_of(current)
        if (k /= 0) command_interpreter_last_output_start = marked_position(command_interpreter_buffers(k)%last_output_start)
    end function command_interpreter_last_output_start

    !> True if communications via pty; false if by pipe.  Buffer local.
    !> This is to work around a bug in Emacs process signaling.  Port of
    !> comint.el comint-ptyp.
    logical function is_command_interpreter_pty()
        integer :: k

        is_command_interpreter_pty = .true.
        k = entry_of(current)
        if (k /= 0) is_command_interpreter_pty = command_interpreter_buffers(k)%ptyp
    end function is_command_interpreter_pty

    !> Give the current buffer its own `command_interpreter_get_old_input', LOCAL_PROCEDURE (null
    !> when absent).  Not a port.
    subroutine set_command_interpreter_get_old_input_local(self, local_procedure)
        class(command_interpreter_get_old_input_setting_type), intent(inout) :: self
        procedure(command_interpreter_get_old_input_procedure), optional :: local_procedure
        integer :: k

        call self%make_local_entry(k)
        if (k == 0) return
        self%locals(k)%procedure => null()
        if (present(local_procedure)) self%locals(k)%procedure => local_procedure
    end subroutine set_command_interpreter_get_old_input_local

    !> The current buffer's `command_interpreter_get_old_input': its own value, else the global one.
    !> Not a port.
    function get_command_interpreter_get_old_input_current(self) result(current_procedure)
        class(command_interpreter_get_old_input_setting_type), intent(in) :: self
        procedure(command_interpreter_get_old_input_procedure), pointer :: current_procedure
        integer :: k

        current_procedure => self%procedure
        k = self%get_local_index()
        if (k > 0) current_procedure => self%locals(k)%procedure
    end function get_command_interpreter_get_old_input_current

    ! Grow `locals' by one entry.  Not a port.
    subroutine append_command_interpreter_get_old_input_entry(self)
        class(command_interpreter_get_old_input_setting_type), intent(inout) :: self
        type(command_interpreter_get_old_input_entry_type) :: entry

        if (.not. allocated(self%locals)) allocate (self%locals(0))
        self%locals = [self%locals, entry]
    end subroutine append_command_interpreter_get_old_input_entry

    ! Reset entry K of `locals'.  Not a port.
    subroutine clear_command_interpreter_get_old_input_entry(self, k)
        class(command_interpreter_get_old_input_setting_type), intent(inout) :: self
        integer, intent(in) :: k

        self%locals(k)%procedure => null()
    end subroutine clear_command_interpreter_get_old_input_entry

    !> Give the current buffer its own `command_interpreter_input_sender', LOCAL_PROCEDURE (null
    !> when absent).  Not a port.
    subroutine set_command_interpreter_input_sender_local(self, local_procedure)
        class(command_interpreter_input_sender_setting_type), intent(inout) :: self
        procedure(command_interpreter_input_sender_procedure), optional :: local_procedure
        integer :: k

        call self%make_local_entry(k)
        if (k == 0) return
        self%locals(k)%procedure => null()
        if (present(local_procedure)) self%locals(k)%procedure => local_procedure
    end subroutine set_command_interpreter_input_sender_local

    !> The current buffer's `command_interpreter_input_sender': its own value, else the global one.
    !> Not a port.
    function get_command_interpreter_input_sender_current(self) result(current_procedure)
        class(command_interpreter_input_sender_setting_type), intent(in) :: self
        procedure(command_interpreter_input_sender_procedure), pointer :: current_procedure
        integer :: k

        current_procedure => self%procedure
        k = self%get_local_index()
        if (k > 0) current_procedure => self%locals(k)%procedure
    end function get_command_interpreter_input_sender_current

    ! Grow `locals' by one entry.  Not a port.
    subroutine append_command_interpreter_input_sender_entry(self)
        class(command_interpreter_input_sender_setting_type), intent(inout) :: self
        type(command_interpreter_input_sender_entry_type) :: entry

        if (.not. allocated(self%locals)) allocate (self%locals(0))
        self%locals = [self%locals, entry]
    end subroutine append_command_interpreter_input_sender_entry

    ! Reset entry K of `locals'.  Not a port.
    subroutine clear_command_interpreter_input_sender_entry(self, k)
        class(command_interpreter_input_sender_setting_type), intent(inout) :: self
        integer, intent(in) :: k

        self%locals(k)%procedure => null()
    end subroutine clear_command_interpreter_input_sender_entry

    !> Give the current buffer its own `command_interpreter_input_filter', LOCAL_PROCEDURE (null
    !> when absent).  Not a port.
    subroutine set_command_interpreter_input_filter_local(self, local_procedure)
        class(command_interpreter_input_filter_setting_type), intent(inout) :: self
        procedure(command_interpreter_input_filter_procedure), optional :: local_procedure
        integer :: k

        call self%make_local_entry(k)
        if (k == 0) return
        self%locals(k)%procedure => null()
        if (present(local_procedure)) self%locals(k)%procedure => local_procedure
    end subroutine set_command_interpreter_input_filter_local

    !> The current buffer's `command_interpreter_input_filter': its own value, else the global one.
    !> Not a port.
    function get_command_interpreter_input_filter_current(self) result(current_procedure)
        class(command_interpreter_input_filter_setting_type), intent(in) :: self
        procedure(command_interpreter_input_filter_procedure), pointer :: current_procedure
        integer :: k

        current_procedure => self%procedure
        k = self%get_local_index()
        if (k > 0) current_procedure => self%locals(k)%procedure
    end function get_command_interpreter_input_filter_current

    ! Grow `locals' by one entry.  Not a port.
    subroutine append_command_interpreter_input_filter_entry(self)
        class(command_interpreter_input_filter_setting_type), intent(inout) :: self
        type(command_interpreter_input_filter_entry_type) :: entry

        if (.not. allocated(self%locals)) allocate (self%locals(0))
        self%locals = [self%locals, entry]
    end subroutine append_command_interpreter_input_filter_entry

    ! Reset entry K of `locals'.  Not a port.
    subroutine clear_command_interpreter_input_filter_entry(self, k)
        class(command_interpreter_input_filter_setting_type), intent(inout) :: self
        integer, intent(in) :: k

        self%locals(k)%procedure => null()
    end subroutine clear_command_interpreter_input_filter_entry

    !> Give the current buffer its own `command_interpreter_password_function', LOCAL_PROCEDURE (null
    !> when absent).  Not a port.
    subroutine set_command_interpreter_password_local(self, local_procedure)
        class(command_interpreter_password_setting_type), intent(inout) :: self
        procedure(command_interpreter_password_procedure), optional :: local_procedure
        integer :: k

        call self%make_local_entry(k)
        if (k == 0) return
        self%locals(k)%procedure => null()
        if (present(local_procedure)) self%locals(k)%procedure => local_procedure
    end subroutine set_command_interpreter_password_local

    !> The current buffer's `command_interpreter_password_function': its own value, else the global one.
    !> Not a port.
    function get_command_interpreter_password_current(self) result(current_procedure)
        class(command_interpreter_password_setting_type), intent(in) :: self
        procedure(command_interpreter_password_procedure), pointer :: current_procedure
        integer :: k

        current_procedure => self%procedure
        k = self%get_local_index()
        if (k > 0) current_procedure => self%locals(k)%procedure
    end function get_command_interpreter_password_current

    ! Grow `locals' by one entry.  Not a port.
    subroutine append_command_interpreter_password_entry(self)
        class(command_interpreter_password_setting_type), intent(inout) :: self
        type(command_interpreter_password_entry_type) :: entry

        if (.not. allocated(self%locals)) allocate (self%locals(0))
        self%locals = [self%locals, entry]
    end subroutine append_command_interpreter_password_entry

    ! Reset entry K of `locals'.  Not a port.
    subroutine clear_command_interpreter_password_entry(self, k)
        class(command_interpreter_password_setting_type), intent(inout) :: self
        integer, intent(in) :: k

        self%locals(k)%procedure => null()
    end subroutine clear_command_interpreter_password_entry

    !> Give the current buffer its own `command_interpreter_unquote_function', LOCAL_PROCEDURE (null
    !> when absent).  Not a port.
    subroutine set_command_interpreter_unquote_local(self, local_procedure)
        class(command_interpreter_unquote_setting_type), intent(inout) :: self
        procedure(completion_unquote_procedure), optional :: local_procedure
        integer :: k

        call self%make_local_entry(k)
        if (k == 0) return
        self%locals(k)%procedure => null()
        if (present(local_procedure)) self%locals(k)%procedure => local_procedure
    end subroutine set_command_interpreter_unquote_local

    !> The current buffer's `command_interpreter_unquote_function': its own value, else the global one.
    !> Not a port.
    function get_command_interpreter_unquote_current(self) result(current_procedure)
        class(command_interpreter_unquote_setting_type), intent(in) :: self
        procedure(completion_unquote_procedure), pointer :: current_procedure
        integer :: k

        current_procedure => self%procedure
        k = self%get_local_index()
        if (k > 0) current_procedure => self%locals(k)%procedure
    end function get_command_interpreter_unquote_current

    ! Grow `locals' by one entry.  Not a port.
    subroutine append_command_interpreter_unquote_entry(self)
        class(command_interpreter_unquote_setting_type), intent(inout) :: self
        type(command_interpreter_unquote_entry_type) :: entry

        if (.not. allocated(self%locals)) allocate (self%locals(0))
        self%locals = [self%locals, entry]
    end subroutine append_command_interpreter_unquote_entry

    ! Reset entry K of `locals'.  Not a port.
    subroutine clear_command_interpreter_unquote_entry(self, k)
        class(command_interpreter_unquote_setting_type), intent(inout) :: self
        integer, intent(in) :: k

        self%locals(k)%procedure => null()
    end subroutine clear_command_interpreter_unquote_entry

    !> Give the current buffer its own `command_interpreter_requote_function', LOCAL_PROCEDURE (null
    !> when absent).  Not a port.
    subroutine set_command_interpreter_requote_local(self, local_procedure)
        class(command_interpreter_requote_setting_type), intent(inout) :: self
        procedure(completion_requote_procedure), optional :: local_procedure
        integer :: k

        call self%make_local_entry(k)
        if (k == 0) return
        self%locals(k)%procedure => null()
        if (present(local_procedure)) self%locals(k)%procedure => local_procedure
    end subroutine set_command_interpreter_requote_local

    !> The current buffer's `command_interpreter_requote_function': its own value, else the global one.
    !> Not a port.
    function get_command_interpreter_requote_current(self) result(current_procedure)
        class(command_interpreter_requote_setting_type), intent(in) :: self
        procedure(completion_requote_procedure), pointer :: current_procedure
        integer :: k

        current_procedure => self%procedure
        k = self%get_local_index()
        if (k > 0) current_procedure => self%locals(k)%procedure
    end function get_command_interpreter_requote_current

    ! Grow `locals' by one entry.  Not a port.
    subroutine append_command_interpreter_requote_entry(self)
        class(command_interpreter_requote_setting_type), intent(inout) :: self
        type(command_interpreter_requote_entry_type) :: entry

        if (.not. allocated(self%locals)) allocate (self%locals(0))
        self%locals = [self%locals, entry]
    end subroutine append_command_interpreter_requote_entry

    ! Reset entry K of `locals'.  Not a port.
    subroutine clear_command_interpreter_requote_entry(self, k)
        class(command_interpreter_requote_setting_type), intent(inout) :: self
        integer, intent(in) :: k

        self%locals(k)%procedure => null()
    end subroutine clear_command_interpreter_requote_entry

    ! Set the global value of `command_interpreter_completion_fignore' to
    ! SUFFIXES.  Not a port.
    subroutine assign_command_interpreter_completion_fignore(self, suffixes)
        class(command_interpreter_completion_fignore_setting_type), intent(inout) :: self
        character(len=*), intent(in) :: suffixes(:)
        integer :: i

        if (allocated(self%suffixes)) deallocate (self%suffixes)
        allocate (self%suffixes(size(suffixes)))

        do i = 1, size(suffixes)
            self%suffixes(i) = suffixes(i)
        end do
    end subroutine assign_command_interpreter_completion_fignore

    !> Give the current buffer its own `command_interpreter_completion_fignore',
    !> SUFFIXES.  Not a port.
    subroutine set_command_interpreter_completion_fignore_local(self, suffixes)
        class(command_interpreter_completion_fignore_setting_type), intent(inout) :: self
        character(len=*), intent(in) :: suffixes(:)
        integer :: k, i

        call self%make_local_entry(k)
        if (k == 0) return
        if (allocated(self%locals(k)%suffixes)) deallocate (self%locals(k)%suffixes)
        allocate (self%locals(k)%suffixes(size(suffixes)))

        do i = 1, size(suffixes)
            self%locals(k)%suffixes(i) = suffixes(i)
        end do
    end subroutine set_command_interpreter_completion_fignore_local

    !> The current buffer's `command_interpreter_completion_fignore': its
    !> own value, else the global one (empty when unset).  Not a port.
    function get_command_interpreter_completion_fignore_current(self) result(suffixes)
        class(command_interpreter_completion_fignore_setting_type), intent(in) :: self
        character(len=8), allocatable :: suffixes(:)
        integer :: k

        k = self%get_local_index()

        if (k > 0) then
            if (allocated(self%locals(k)%suffixes)) suffixes = self%locals(k)%suffixes
        else if (allocated(self%suffixes)) then
            suffixes = self%suffixes
        end if

        if (.not. allocated(suffixes)) allocate (suffixes(0))
    end function get_command_interpreter_completion_fignore_current

    ! Grow `locals' by one entry.  Not a port.
    subroutine append_command_interpreter_completion_fignore_entry(self)
        class(command_interpreter_completion_fignore_setting_type), intent(inout) :: self
        type(command_interpreter_completion_fignore_entry_type) :: entry

        if (.not. allocated(self%locals)) allocate (self%locals(0))
        self%locals = [self%locals, entry]
    end subroutine append_command_interpreter_completion_fignore_entry

    ! Reset entry K of `locals'.  Not a port.
    subroutine clear_command_interpreter_completion_fignore_entry(self, k)
        class(command_interpreter_completion_fignore_setting_type), intent(inout) :: self
        integer, intent(in) :: k

        if (allocated(self%locals(k)%suffixes)) deallocate (self%locals(k)%suffixes)
    end subroutine clear_command_interpreter_completion_fignore_entry

    ! ------------------------------------------------------------------
    ! Small helpers
    ! ------------------------------------------------------------------

    !> Bind `inhibit_read_only' to true, keeping its value in SAVED.
    !> Not a port.
    subroutine inhibit_read_only_begin(saved)
        type(value_type), intent(out) :: saved
        type(status_type) :: st

        call symbol_value(value_type(name="inhibit_read_only"), saved, st)
        call set(value_type(name="inhibit_read_only"), value_type(.true.), st)
    end subroutine inhibit_read_only_begin

    !> End the binding of `inhibit_read_only_begin'.  Not a port.
    subroutine inhibit_read_only_end(saved)
        type(value_type), intent(in) :: saved
        type(status_type) :: st

        call set(value_type(name="inhibit_read_only"), saved, st)
    end subroutine inhibit_read_only_end

    !> The current buffer's process, or an error: "Current buffer has no
    !> process".  Not a port.
    subroutine current_process(process, status)
        type(handle_type), intent(out) :: process
        type(status_type), intent(out) :: status

        status = status_type()
        process = get_buffer_process(current_buffer())
        if (process%index == 0) call set_error(status, USER_ERROR_CODE, "Current buffer has no process")
    end subroutine current_process

    !> The position of the process mark of PROCESS (0 if it points nowhere).
    !> Not a port.
    integer(int64) function pmark_position(process)
        type(handle_type), intent(in) :: process

        pmark_position = marked_position(process_mark(process))
    end function pmark_position

    !> The text properties (`rear_nonsticky' with `command_interpreter__prompt_rear_nonsticky').
    !> Not a port.
    function rear_nonsticky_plist() result(v)
        type(value_type) :: v

        v = list(value_type(name="rear_nonsticky"), list(value_type(name=trim(PROMPT_REAR_NONSTICKY(1))), &
                                                         value_type(name=trim(PROMPT_REAR_NONSTICKY(2))), &
                                                         value_type(name=trim(PROMPT_REAR_NONSTICKY(3))), &
                                                         value_type(name=trim(PROMPT_REAR_NONSTICKY(4))), &
                                                         value_type(name=trim(PROMPT_REAR_NONSTICKY(5)))))
    end function rear_nonsticky_plist

    !> Whether the name V is named NAME.  Not a port.
    logical function symbol_is(v, name)
        type(value_type), intent(in) :: v
        character(len=*), intent(in) :: name

        symbol_is = .false.
        if ((vtag(v) /= V_NAME .and. vtag(v) /= V_NULL)) return
        if (.not. allocated(v%a)) return
        symbol_is = symbols(xsymbol(v))%name == name
    end function symbol_is

    !> The `field' property of the character at POSITION (get-char-property).
    !> Not a port.
    function field_at(position) result(v)
        integer(int64), intent(in) :: position
        type(value_type) :: v
        type(status_type) :: st

        call get_text_property(position, value_type(name="field"), v, st)
    end function field_at

    ! ------------------------------------------------------------------
    ! The mode
    ! ------------------------------------------------------------------

    !> Major mode for interacting with an inferior interpreter.
    !> Interpreter name is same as buffer name, sans the asterisks.
    !> Return at end of buffer sends line as input.
    !> Return not at end copies rest of line to end and sends it.
    !> Setting variable `command_interpreter_end_of_line_on_send' means jump to the end of the
    !> line before submitting new input.
    !>
    !> This mode is customized to create major modes such as Inferior Lisp
    !> mode, Shell mode, etc.  This can be done by setting the hooks
    !> `command_interpreter_input_filter_functions', `command_interpreter_input_filter',
    !> `command_interpreter_input_sender' and `command_interpreter_get_old_input' to appropriate
    !> functions, and the variable `command_interpreter_prompt_regular_expression' to the
    !> appropriate regular expression.
    !>
    !> The mode maintains an input history of size `command_interpreter_input_ring_size'.
    !> You can access this with the commands \[command_interpreter_next_input] and
    !> \[command_interpreter_previous_input].
    !> Addition is controlled by the variable `command_interpreter_input_ignoredups'.
    !>
    !> Input to, and output from, the subprocess can cause the window to
    !> scroll to the end of the buffer.  See variables
    !> `command_interpreter_output_filter_functions', `command_interpreter_preoutput_filter_functions'
    !> and `command_interpreter_move_point_for_output'.
    !>
    !> Entry to this mode runs the hooks on `command_interpreter_mode_hook'.  Port of
    !> comint.el:comint-mode.
    recursive subroutine command_interpreter_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. associated(major_modes%command_interpreter_mode%execute)) call initialize_command_interpreter()
        call major_modes%command_interpreter_mode%begin(saved, status)
        if (.not. status%ok()) return
        call command_interpreter_mode_body()
        call major_modes%command_interpreter_mode%end(saved, status)
    end subroutine command_interpreter_mode

    !> The body of comint-mode (also run by the modes derived from it,
    !> through their parent).  Port of comint.el:comint-mode (its body).
    subroutine command_interpreter_mode_body()
        type(mode_line_part_type) :: parts(2)
        integer :: k

        ! (":%s"): a colon, then the status of the process.
        parts(1)%text = ":"
        parts(1)%face = ""
        parts(2)%text = ""
        parts(2)%part = MODE_LINE_BUFFER_PROCESS_STATUS
        parts(2)%face = ""
        call set_mode_line_process(parts)
        call make_current_entry(k)
        call make_locals(k)
        call command_interpreter_prompt_regular_expression%make_local()
        call command_interpreter_delimiter_arguments%make_local()
        call command_interpreter_input_ring_size%make_local()
        call command_interpreter_input_ring_file_name%make_local()
        call command_interpreter_input_autoexpand%make_local()
        call command_interpreter_input_ignoredups%make_local()
        call command_interpreter_end_of_line_on_send%make_local()
        call command_interpreter_scroll_to_bottom_on_input%make_local()
        call command_interpreter_move_point_for_output%make_local()
        call command_interpreter_scroll_show_maximum_output%make_local()
        call command_interpreter_process_echoes%make_local()
        call make_current_entry(k)

        if (.not. command_interpreter_buffers(k)%has_ring) then
            call command_interpreter_buffers(k)%input_ring%make(int(command_interpreter_input_ring_size%value))
            command_interpreter_buffers(k)%has_ring = .true.
        end if

        call pre_command_hook%append(command_interpreter_preinput_scroll_to_bottom, local=.true.)
        call font_lock_defaults%set_local(keywords_only=.true.)
        call command_interpreter_file_name_characters%make_local()
        call command_interpreter_file_name_quote_characters%make_local()
        ! dir tracking on remote files
        call command_interpreter_file_name_prefix%set_local("")
        call incremental_search_mode_hook%prepend(command_interpreter_history_incremental_search_setup, local=.true.)
        call completion_at_point_functions%prepend(command_interpreter_completion_at_point, local=.true.)
    end subroutine command_interpreter_mode_body

    !> Return true if there is a living process associated with BUFFER.
    !> Living means the status is `run' (`open' and `stop' are not
    !> ported).  Port of comint.el:comint-check-proc.
    logical function command_interpreter_check_process(buffer)
        type(handle_type), intent(in) :: buffer
        type(handle_type) :: process

        command_interpreter_check_process = .false.
        process = get_buffer_process(buffer)
        if (process%index /= 0) command_interpreter_check_process = process_status(process) == "run"
    end function command_interpreter_check_process

    !> Make a Comint process NAME in BUFFER, running PROGRAM.
    !> If BUFFER_NAME is absent, it defaults to NAME surrounded by `*'s.
    !> If there is a running process in BUFFER, it is not restarted.
    !>
    !> Optional fourth arg STARTFILE is the name of a file, whose
    !> contents are sent to the process as its initial input.
    !>
    !> SWITCHES are arguments to PROGRAM.
    !>
    !> RESULT is the (possibly newly created) process buffer.  Port of
    !> comint.el:make-comint-in-buffer (PROGRAM a string).
    recursive subroutine make_command_interpreter_in_buffer(name, program, result, status, buffer_name, startfile, switches)
        character(len=*), intent(in) :: name, program
        type(handle_type), intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: buffer_name, startfile
        type(string_type), intent(in), optional :: switches(:)
        type(handle_type) :: old
        type(status_type) :: st
        type(string_type), allocatable :: arguments(:)

        if (present(buffer_name)) then
            call get_or_make_buffer(buffer_name, result, status)
        else
            call get_or_make_buffer("*"//name//"*", result, status)
        end if

        if (.not. status%ok()) return
        ! If no process, or nuked process, crank up a new one and put
        ! buffer in comint mode.  Otherwise, leave buffer and existing
        ! process alone.
        if (command_interpreter_check_process(result)) return
        old = current_buffer()
        call set_buffer(result, status)
        if (.not. status%ok()) return
        if (.not. derived_from_command_interpreter()) call command_interpreter_mode(status)
        if (is_buffer_live(old)) call set_buffer(old, st)
        if (.not. status%ok()) return

        if (present(switches)) then
            arguments = switches
        else
            allocate (arguments(0))
        end if

        call command_interpreter_exec(result, name, program, arguments, status, startfile)
    end subroutine make_command_interpreter_in_buffer

    !> Whether the current buffer's major mode is derived from Comint mode.
    !> Not a port.
    logical function derived_from_command_interpreter()
        use derived_mod, only: is_derived_mode

        derived_from_command_interpreter = is_derived_mode("command_interpreter_mode")
    end function derived_from_command_interpreter

    !> Make a Comint process NAME in a buffer, running PROGRAM.
    !> The name of the buffer is made by surrounding NAME with `*'s.
    !> If there is already a running process in that buffer, it is not
    !> restarted.  Optional third arg STARTFILE is the name of a file,
    !> whose contents are sent to the process as its initial input.
    !>
    !> SWITCHES are arguments to PROGRAM.
    !>
    !> RESULT is the (possibly newly created) process buffer.  Port of
    !> comint.el:make-comint.
    recursive subroutine make_command_interpreter(name, program, result, status, startfile, switches)
        character(len=*), intent(in) :: name, program
        type(handle_type), intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: startfile
        type(string_type), intent(in), optional :: switches(:)

        call make_command_interpreter_in_buffer(name, program, result, status, startfile=startfile, switches=switches)
    end subroutine make_command_interpreter

    !> Run PROGRAM in a Comint buffer and switch to that buffer.
    !>
    !> If SWITCHES are supplied, they are passed to PROGRAM.
    !>
    !> The buffer name is made by surrounding the file name of PROGRAM with
    !> `*'s.
    !>
    !> See `make_command_interpreter' and `command_interpreter_exec'.  Port of comint.el:comint-run
    !> (the hooks `comint-NAME-hook' are not applicable: hooks are named
    !> objects).
    recursive subroutine command_interpreter_execute(program, status, switches)
        use file_input_output_mod, only: file_name_nondirectory
        use window_mod, only: switch_to_buffer
        character(len=*), intent(in) :: program
        type(status_type), intent(out) :: status
        type(string_type), intent(in), optional :: switches(:)
        type(handle_type) :: buffer
        character(len=:), allocatable :: name

        name = file_name_nondirectory(program)
        call make_command_interpreter(name, program, buffer, status, switches=switches)
        if (.not. status%ok()) return
        call switch_to_buffer(buffers(buffer%index)%name, status)
    end subroutine command_interpreter_execute

    !> The command: read PROGRAM and run it.  Port of the command of
    !> comint.el:comint-run.
    !>
    !> Run PROGRAM in a Comint buffer and switch to that buffer.
    recursive subroutine execute_command_interpreter_execute(status)
        character(len=:), allocatable :: program
        type(status_type), intent(out) :: status

        call read_string("Run program: ", program, status)
        if (.not. status%ok()) return

        if (current_prefix_argument%kind == PREFIX_RAW) then
            call set_error(status, GENERIC_ERROR_CODE, "Reading switches for command_interpreter_execute is not supported yet")
            return
        end if

        call command_interpreter_execute(program, status)
    end subroutine execute_command_interpreter_execute

    !> Start up a process named NAME in buffer BUFFER for Comint modes.
    !> Runs the given COMMAND with SWITCHES, and initial input from
    !> STARTFILE.
    !>
    !> This function blasts any old process running in the buffer, and
    !> does not set the buffer mode.  You can use this to cheaply run a
    !> series of processes in the same Comint buffer.  The hook
    !> `command_interpreter_exec_hook' is run after each exec.  ENVIRONMENT, if
    !> present, are "NAME=VALUE" entries put after Comint's own in the
    !> process's environment (as binding `process_environment' around the
    !> call).  Port of comint.el:comint-exec (COMMAND a program).
    recursive subroutine command_interpreter_exec(buffer, name, command, switches, status, startfile, environment)
        type(handle_type), intent(in) :: buffer
        character(len=*), intent(in) :: name, command
        type(string_type), intent(in) :: switches(:)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: startfile
        type(string_type), intent(in), optional :: environment(:)
        type(handle_type) :: old, process
        type(status_type) :: st
        character(len=:), allocatable :: text
        integer :: k

        old = current_buffer()
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        ! Blast any old process.
        process = get_buffer_process(buffer)
        if (process%index /= 0) call delete_process(process, st)
        ! Crank up a new process.
        call command_interpreter_exec_1(name, buffer, command, switches, process, status, environment)

        if (.not. status%ok()) then
            if (is_buffer_live(old)) call set_buffer(old, st)
            return
        end if

        call set_process_filter(process, st, command_interpreter_output_filter)
        ! t if pty, nil if pipe.
        call make_current_entry(k)
        command_interpreter_buffers(k)%ptyp = process_connection_type%value
        ! Jump to the end, and set the process mark.
        call goto_character(point_maximum())
        call set_marker(process_mark(process), point(), st)

        ! Feed it the startfile.
        if (present(startfile)) then
            call temporary_buffer_file_contents(startfile, text, status)
            if (.not. status%ok()) return
            call command_interpreter_send_string(process, text, status)
            if (.not. status%ok()) return
        end if

        call command_interpreter_exec_hook%execute(status)
        if (is_buffer_live(old)) call set_buffer(old, st)
    end subroutine command_interpreter_exec

    !> The contents of FILE (with-temp-buffer, insert-file-contents,
    !> buffer-string).  Not a port.
    subroutine temporary_buffer_file_contents(file, text, status)
        use buffer_mod, only: close_buffer, generate_new_buffer_name
        use editing_functions_mod, only: buffer_string
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: text
        type(status_type), intent(out) :: status
        type(handle_type) :: old, temporary
        type(status_type) :: st
        integer(int64) :: inserted

        text = ""
        old = current_buffer()
        call get_or_make_buffer(generate_new_buffer_name(" *temp*"), temporary, status)
        if (.not. status%ok()) return
        call set_buffer(temporary, st)
        call insert_file_contents(file, .false., inserted, status)
        if (status%ok()) text = buffer_string()
        call set_buffer(old, st)
        call close_buffer(temporary, st)
    end subroutine temporary_buffer_file_contents

    !> Crank up the process for comint-exec in the appropriate environment.
    !> Port of comint.el:comint-exec-1.
    recursive subroutine command_interpreter_exec_1(name, buffer, command, switches, process, status, environment)
        character(len=*), intent(in) :: name, command
        type(handle_type), intent(in) :: buffer
        type(string_type), intent(in) :: switches(:)
        type(handle_type), intent(out) :: process
        type(status_type), intent(out) :: status
        type(string_type), intent(in), optional :: environment(:)
        type(string_type), allocatable :: environment_variables(:), argv(:)
        character(len=:), allocatable :: saved_directory
        logical :: had_directory
        integer :: k

        call command_interpreter_term_environment(environment_variables)
        call append_string(environment_variables, "INSIDE_EMACS="//upstream_version//",comint")
        if (allocated(command_interpreter_pager%value)) call append_string(environment_variables, &
                                                                           "PAGER="//command_interpreter_pager%value)

        if (present(environment)) then
            do k = 1, size(environment)
                call append_string(environment_variables, environment(k)%text)
            end do
        end if

        had_directory = allocated(buffers(current)%directory)
        if (had_directory) saved_directory = buffers(current)%directory

        if (had_directory) then
            if (.not. is_file_accessible_directory(saved_directory)) buffers(current)%directory = "/"
        else
            buffers(current)%directory = "/"
        end if

        allocate (argv(size(switches) + 1))
        argv(1)%text = command

        do k = 1, size(switches)
            argv(k + 1)%text = switches(k)%text
        end do

        call make_process(name, argv, process, status, buffer=buffer, environment=environment_variables)

        if (had_directory) then
            buffers(current)%directory = saved_directory
        else
            deallocate (buffers(current)%directory)
        end if
        ! (Coding systems are not ported.)
    end subroutine command_interpreter_exec_1

    !> Return an environment variable list for terminal configuration.
    !> Port of comint.el:comint-term-environment (a system using
    !> terminfo).
    subroutine command_interpreter_term_environment(environment)
        type(string_type), allocatable, intent(out) :: environment(:)
        character(len=16) :: columns
        integer :: width

        width = 80
        if (selected_frame%selected_window%index /= 0) width = window_text_columns(windows(selected_frame%selected_window%index))
        write (columns, "(i0)") width
        call append_string(environment, "TERM="//command_interpreter_terminfo_terminal%value, "TERMCAP=", "COLUMNS="//trim(columns))
    end subroutine command_interpreter_term_environment

    !> Return true if STRING contains non-whitespace syntax.  Port of
    !> comint.el:comint-nonblank-p.
    logical function is_command_interpreter_nonblank(string)
        character(len=*), intent(in) :: string
        type(status_type) :: st

        is_command_interpreter_nonblank = string_match("\`\s *\'", string, st, inhibit_modify=.true.) == 0
    end function is_command_interpreter_nonblank

    ! ------------------------------------------------------------------
    ! The input history
    ! ------------------------------------------------------------------

    !> Set the buffer's `command_interpreter_input_ring' from a history file.
    !> The name of the file is given by the variable
    !> `command_interpreter_input_ring_file_name'.
    !> The history ring is of size `command_interpreter_input_ring_size', regardless of
    !> file size.  If `command_interpreter_input_ring_file_name' is unallocated this
    !> function does nothing.
    !>
    !> If the optional argument SILENT is true, we say nothing about a
    !> failure to read the history file.
    !>
    !> This function is useful for major mode commands and mode hooks.
    !>
    !> The commands stored in the history file are separated by the
    !> `command_interpreter_input_ring_separator', and entries that match
    !> `command_interpreter_input_history_ignore' are ignored.  The most recent command
    !> comes last.
    !>
    !> See also `command_interpreter_input_ignoredups' and `command_interpreter_write_input_ring'.
    !> Port of comint.el:comint-read-input-ring.
    subroutine command_interpreter_read_input_ring(silent)
        use buffer_mod, only: close_buffer, generate_new_buffer_name
        logical, intent(in), optional :: silent
        type(ring_type) :: ring
        type(handle_type) :: old, temporary
        type(status_type) :: st
        character(len=:), allocatable :: file, separator, prefix, ignore, history, last
        integer(int64) :: inserted, found, start, end
        integer :: count, ring_size, ring_maximum_size, k
        logical :: quiet, ignoredups, has_prefix, keep

        quiet = .false.
        if (present(silent)) quiet = silent
        if (.not. allocated(command_interpreter_input_ring_file_name%value)) return
        if (len(command_interpreter_input_ring_file_name%value) == 0) return
        file = command_interpreter_input_ring_file_name%value

        if (.not. is_file_readable(file)) then
            if (.not. quiet) call message("Cannot read history file "//file)
            return
        end if

        count = 0
        ! Some users set HISTSIZE or `comint-input-ring-size' to huge
        ! numbers.  Don't allocate a huge ring right away; there might not
        ! be that much history.
        ring_size = int(min(1500_int64, command_interpreter_input_ring_size%value))
        call ring%make(ring_size)
        ! Use possibly buffer-local values of these variables.
        ring_maximum_size = int(command_interpreter_input_ring_size%value)
        separator = command_interpreter_input_ring_separator%value
        has_prefix = allocated(command_interpreter_input_ring_file_prefix%value)
        if (has_prefix) prefix = command_interpreter_input_ring_file_prefix%value
        ignore = command_interpreter_input_history_ignore%value
        ignoredups = command_interpreter_input_ignoredups%value
        old = current_buffer()
        call get_or_make_buffer(generate_new_buffer_name(" *temp*"), temporary, st)
        call set_buffer(temporary, st)
        call insert_file_contents(file, .false., inserted, st)
        call goto_character(point_maximum())

        do while (count < ring_maximum_size)
            call regular_expression_search_backward(separator, found, st, noerror=SEARCH_NOERROR)
            if (found == 0) exit
            end = match_beginning(0, st)
            call regular_expression_search_backward(separator, found, st, noerror=SEARCH_NOERROR)

            if (found /= 0) then
                call goto_character(match_end(0, st))
            else
                call goto_character(point_minimum())
            end if

            if (has_prefix) then
                ! Skip zsh extended_history stamps.
                if (looking_at(prefix, st)) call goto_character(match_end(0, st))
            end if

            start = point()
            call buffer_substring(start, end, history, st)
            keep = string_match(ignore, history, st) == 0

            if (keep .and. ignoredups .and. .not. ring%empty_p()) then
                call ring%reference(ring%count() - 1, last, st)
                keep = .not. (last == history .and. len(last) == len(history))
            end if

            if (keep) then
                if (count == ring_size) then
                    call ring%extend(min(ring_maximum_size - ring_size, ring_size))
                    ring_size = ring%size()
                end if

                call ring%insert_at_beginning(history)
                count = count + 1
            end if
        end do

        call set_buffer(old, st)
        call close_buffer(temporary, st)
        call make_current_entry(k)
        command_interpreter_buffers(k)%input_ring = ring
        command_interpreter_buffers(k)%has_ring = .true.
        command_interpreter_buffers(k)%input_ring_index = -1
    end subroutine command_interpreter_read_input_ring

    !> Writes the buffer's `command_interpreter_input_ring' to a history file.
    !> The name of the file is given by the variable
    !> `command_interpreter_input_ring_file_name'.  The original contents of the file
    !> are lost if `command_interpreter_input_ring' is not empty.  If
    !> `command_interpreter_input_ring_file_name' is unallocated this function does
    !> nothing.
    !>
    !> Useful within process sentinels.
    !>
    !> See also `command_interpreter_read_input_ring'.  Port of
    !> comint.el:comint-write-input-ring.
    subroutine command_interpreter_write_input_ring()
        use buffer_mod, only: close_buffer, generate_new_buffer_name
        type(handle_type) :: old, temporary
        type(status_type) :: st
        type(ring_type) :: ring
        character(len=:), allocatable :: file, separator, item, message_text
        integer :: k, index

        k = entry_of(current)
        if (.not. allocated(command_interpreter_input_ring_file_name%value)) return
        if (len(command_interpreter_input_ring_file_name%value) == 0) return
        if (k == 0) return
        if (.not. command_interpreter_buffers(k)%has_ring) return
        if (command_interpreter_buffers(k)%input_ring%empty_p()) return
        file = command_interpreter_input_ring_file_name%value

        if (.not. is_file_writable(file)) then
            call message("Cannot write history file "//file)
            return
        end if

        ring = command_interpreter_buffers(k)%input_ring
        separator = command_interpreter_input_ring_separator%value
        index = ring%count()
        ! Write it all out into a buffer first.  Much faster, but messier,
        ! than writing it one line at a time.
        old = current_buffer()
        call get_or_make_buffer(generate_new_buffer_name(" *temp*"), temporary, st)
        call set_buffer(temporary, st)

        do while (index > 0)
            index = index - 1
            call ring%reference(index, item, st)
            call insert(item//separator, st)
        end do

        call write_region(file, .false., st, message=message_text)
        call set_buffer(old, st)
        call close_buffer(temporary, st)
    end subroutine command_interpreter_write_input_ring

    !> Read a regexp with PROMPT: REGULAR_EXPRESSION, and the prefix argument as N.
    !> Port of comint.el:comint-regexp-arg.
    subroutine command_interpreter_regular_expression_argument(prompt, regular_expression, n, status)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: regular_expression
        integer, intent(out) :: n
        type(status_type), intent(out) :: status
        type(value_type) :: history
        type(command_type), pointer :: saved_last
        type(value_type), allocatable :: elements(:)
        type(status_type) :: st

        ! Don't clobber this.
        saved_last => last_command
        call read_from_minibuffer(prompt, regular_expression, status, history="minibuffer_history_search_history")
        last_command => saved_last
        if (.not. status%ok()) return

        ! If the user didn't enter anything, nothing is added to m-h-s-h.
        ! Use the previous search regexp, if there is one.
        if (len(regular_expression) == 0) then
            call symbol_value(value_type(name="minibuffer_history_search_history"), history, st)

            if (st%ok() .and. vtag(history) == V_ARRAY) then
                call list_elements(history, elements)

                if (size(elements) > 0) then
                    block
                        use value_mod, only: xstring, value_type
                        if (vtag(elements(1)) == V_STRING) regular_expression = xstring(elements(1))
                    end block
                end if
            end if
        end if

        n = int(prefix_numeric_value(current_prefix_argument))
    end subroutine command_interpreter_regular_expression_argument

    !> First make sure there is a ring and that we are after the process
    !> mark; ARGUMENT is the count (1 for 0, which restarts from the
    !> beginning).  Port of comint.el:comint-search-arg.
    subroutine command_interpreter_search_argument(argument, n, status)
        integer, intent(in) :: argument
        integer, intent(out) :: n
        type(status_type), intent(out) :: status
        integer :: k

        status = status_type()
        n = argument

        if (.not. is_command_interpreter_after_pmark()) then
            call set_error(status, USER_ERROR_CODE, "Not at command line")
            return
        end if

        k = entry_of(current)

        if (k == 0) then
            call set_error(status, USER_ERROR_CODE, "Empty input ring")
            return
        end if

        if (.not. command_interpreter_buffers(k)%has_ring) then
            call set_error(status, USER_ERROR_CODE, "Empty input ring")
        else if (command_interpreter_buffers(k)%input_ring%empty_p()) then
            call set_error(status, USER_ERROR_CODE, "Empty input ring")
        else if (argument == 0) then
            ! arg of zero resets search from beginning, and uses arg of 1.
            command_interpreter_buffers(k)%input_ring_index = -1
            n = 1
        end if
    end subroutine command_interpreter_search_argument

    !> Restore unfinished input.  Port of comint.el:comint-restore-input.
    subroutine command_interpreter_restore_input(status)
        type(status_type), intent(out) :: status
        integer :: k

        status = status_type()
        k = entry_of(current)
        if (k == 0) return
        if (command_interpreter_buffers(k)%input_ring_index < 0) return
        call command_interpreter_delete_input(status)
        if (.not. status%ok()) return

        if (allocated(command_interpreter_buffers(k)%stored_incomplete_input)) then
            if (len(command_interpreter_buffers(k)%stored_incomplete_input) > 0) then
                call insert(command_interpreter_buffers(k)%stored_incomplete_input, status)
                call message("Input restored")
            end if
        end if

        command_interpreter_buffers(k)%input_ring_index = -1
    end subroutine command_interpreter_restore_input

    !> Index to start a directional search, starting at
    !> `command_interpreter_input_ring_index'.  Port of comint.el:comint-search-start.
    integer function command_interpreter_search_start(k, argument) result(index)
        integer, intent(in) :: k, argument

        associate (e => command_interpreter_buffers(k))
            if (e%input_ring_index >= 0) then
                ! If a search is running, offset by 1 in direction of arg.
                index = modulo(e%input_ring_index + merge(1, -1, argument > 0), e%input_ring%count())
            else if (argument >= 0) then
                ! For a new search, start from beginning or end, as
                ! appropriate.
                index = 0
            else
                index = e%input_ring%count() - 1
            end if
        end associate
    end function command_interpreter_search_start

    !> Cycle backwards through input history, saving input.  Port of
    !> comint.el:comint-previous-input.
    subroutine command_interpreter_previous_input(argument, status)
        integer, intent(in) :: argument
        type(status_type), intent(out) :: status
        integer :: k
        logical :: leaving

        k = entry_of(current)
        leaving = .false.

        if (k /= 0) then
            associate (e => command_interpreter_buffers(k))
                if (e%input_ring_index >= 0 .and. allocated(e%stored_incomplete_input) .and. e%has_ring) then
                    ! Leaving the "end" of the ring.
                    leaving = (argument < 0 .and. e%input_ring_index == 0) .or. &
                              (argument > 0 .and. e%input_ring_index == e%input_ring%count() - 1)
                end if
            end associate
        end if

        if (leaving) then
            call command_interpreter_restore_input(status)
        else
            call command_interpreter_previous_matching_input(".", argument, status)
        end if
    end subroutine command_interpreter_previous_input

    !> Cycle forwards through input history.  Port of
    !> comint.el:comint-next-input.
    subroutine command_interpreter_next_input(argument, status)
        integer, intent(in) :: argument
        type(status_type), intent(out) :: status

        call command_interpreter_previous_input(-argument, status)
    end subroutine command_interpreter_next_input

    !> Return the index matching REGULAR_EXPRESSION ARGUMENT places along the input ring
    !> (-1 for none).  Moves relative to START, or
    !> `command_interpreter_input_ring_index'.  Port of
    !> comint.el:comint-previous-matching-input-string-position.
    subroutine command_interpreter_previous_matching_input_string_position(regular_expression, argument_in, position, status, start)
        character(len=*), intent(in) :: regular_expression
        integer, intent(in) :: argument_in
        integer, intent(out) :: position
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: start
        character(len=:), allocatable :: item
        integer :: k, length, motion, n, previous, argument
        logical :: tried_each_ring_item
        type(status_type) :: st

        position = -1
        status = status_type()
        k = entry_of(current)

        if (k == 0) then
            call set_error(status, USER_ERROR_CODE, "No history")
            return
        end if

        if (.not. command_interpreter_buffers(k)%has_ring) then
            call set_error(status, USER_ERROR_CODE, "No history")
            return
        end if

        if (command_interpreter_buffers(k)%input_ring%empty_p()) then
            call set_error(status, USER_ERROR_CODE, "No history")
            return
        end if

        argument = argument_in
        length = command_interpreter_buffers(k)%input_ring%count()
        motion = merge(1, -1, argument > 0)

        if (present(start)) then
            n = modulo(start - motion, length)
        else
            n = modulo(command_interpreter_search_start(k, argument) - motion, length)
        end if

        tried_each_ring_item = .false.

        ! Do the whole search as many times as the argument says.
        do while (argument /= 0 .and. .not. tried_each_ring_item)
            ! Step once.
            previous = n
            n = modulo(n + motion, length)

            ! If we haven't reached a match, step some more.
            do while (n < length .and. .not. tried_each_ring_item)
                call command_interpreter_buffers(k)%input_ring%reference(n, item, st)
                if (string_match(regular_expression, item, status) /= 0) exit
                if (.not. status%ok()) return
                n = modulo(n + motion, length)
                ! If we have gone all the way around in this search.
                tried_each_ring_item = n == previous
            end do

            if (argument > 0) then
                argument = argument - 1
            else
                argument = argument + 1
            end if
        end do

        ! Now that we know which ring element to use, if we found it,
        ! return that.
        call command_interpreter_buffers(k)%input_ring%reference(n, item, st)
        if (string_match(regular_expression, item, status) /= 0) position = n
    end subroutine command_interpreter_previous_matching_input_string_position

    !> Delete all input between accumulation or process mark and point.
    !> Port of comint.el:comint-delete-input.
    subroutine command_interpreter_delete_input(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: process
        integer(int64) :: from
        integer :: k

        status = status_type()
        from = 0
        k = entry_of(current)

        if (k /= 0) then
            if (command_interpreter_buffers(k)%local) from = marked_position(command_interpreter_buffers(k)%accum_marker)
        end if

        if (from == 0) then
            call current_process(process, status)
            if (.not. status%ok()) return
            from = pmark_position(process)
        end if

        ! Can't use kill-region as it sets this-command.
        call delete_region(from, point_maximum(), status)
    end subroutine command_interpreter_delete_input

    !> Search backwards through input history for match for REGULAR_EXPRESSION.
    !> (Previous history elements are earlier commands.)
    !> With prefix argument N, search for Nth previous match.
    !> If N is negative, find the next or Nth next match.  Port of
    !> comint.el:comint-previous-matching-input.
    subroutine command_interpreter_previous_matching_input(regular_expression, n_in, status, restore)
        character(len=*), intent(in) :: regular_expression
        integer, intent(in) :: n_in
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: restore
        character(len=:), allocatable :: item, old
        character(len=16) :: number
        integer :: n, position, k
        logical :: wrap

        call command_interpreter_search_argument(n_in, n, status)
        if (.not. status%ok()) return
        call command_interpreter_previous_matching_input_string_position(regular_expression, n, position, status)
        if (.not. status%ok()) return

        ! Has a match been found?
        if (position < 0) then
            call set_error(status, USER_ERROR_CODE, "Not found")
            return
        end if

        k = entry_of(current)
        wrap = .false.

        if (command_interpreter_buffers(k)%input_ring_index >= 0 .and. present(restore)) then
            if (restore) wrap = (n < 0 .and. command_interpreter_buffers(k)%input_ring_index < position) .or. &
                                (n > 0 .and. command_interpreter_buffers(k)%input_ring_index > position)
        end if

        if (wrap) then
            ! We have a wrap; restore contents.
            call command_interpreter_restore_input(status)
            return
        end if

        ! If leaving the edit line, save partial input.
        if (command_interpreter_buffers(k)%input_ring_index < 0) then
            call execute_get_old_input(old, status)
            if (.not. status%ok()) return
            k = entry_of(current)
            command_interpreter_buffers(k)%stored_incomplete_input = old
        end if

        command_interpreter_buffers(k)%input_ring_index = position
        write (number, "(i0)") position + 1
        call message("History item: "//trim(number))
        call command_interpreter_delete_input(status)
        if (.not. status%ok()) return
        call command_interpreter_input_ring_reference(position, item)
        call insert(item, status)
    end subroutine command_interpreter_previous_matching_input

    !> Item POSITION of the current buffer's input ring.  Not a port.
    subroutine command_interpreter_input_ring_reference(position, item)
        integer, intent(in) :: position
        character(len=:), allocatable, intent(out) :: item
        type(status_type) :: st
        integer :: k

        item = ""
        k = entry_of(current)
        if (k /= 0) call command_interpreter_buffers(k)%input_ring%reference(position, item, st)
    end subroutine command_interpreter_input_ring_reference

    !> Execute the current buffer's `command_interpreter_get_old_input'.  Not a port.
    subroutine execute_get_old_input(input, status)
        character(len=:), allocatable, intent(out) :: input
        type(status_type), intent(out) :: status
        procedure(command_interpreter_get_old_input_procedure), pointer :: get_old_input
        integer :: k

        call make_local_entry(k)
        get_old_input => command_interpreter_get_old_input%get_current()
        call get_old_input(input, status)
    end subroutine execute_get_old_input

    !> Send STRING to PROCESS with the current buffer's
    !> `command_interpreter_input_sender'.  Not a port.
    subroutine execute_input_sender(process, string, status)
        type(handle_type), intent(in) :: process
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        procedure(command_interpreter_input_sender_procedure), pointer :: input_sender

        input_sender => command_interpreter_input_sender%get_current()
        call input_sender(process, string, status)
    end subroutine execute_input_sender

    !> Search forwards through input history for match for REGULAR_EXPRESSION.
    !> (Later history elements are more recent commands.)
    !> With prefix argument N, search for Nth following match.
    !> If N is negative, find the previous or Nth previous match.  Port of
    !> comint.el:comint-next-matching-input.
    subroutine command_interpreter_next_matching_input(regular_expression, n, status)
        character(len=*), intent(in) :: regular_expression
        integer, intent(in) :: n
        type(status_type), intent(out) :: status

        call command_interpreter_previous_matching_input(regular_expression, -n, status)
    end subroutine command_interpreter_next_matching_input

    !> Search backwards through input history for match for current input.
    !> (Previous history elements are earlier commands.)
    !> With prefix argument N, search for Nth previous match.
    !> If N is negative, search forwards for the -Nth following match.
    !> Port of comint.el:comint-previous-matching-input-from-input.
    subroutine command_interpreter_previous_matching_input_from_input(n, status)
        integer, intent(in) :: n
        type(status_type), intent(out) :: status
        type(handle_type) :: process
        integer(int64) :: original_point, from
        integer :: k
        character(len=:), allocatable :: text

        original_point = point()
        call make_local_entry(k)

        if (.not. (associated(last_command, commands%command_interpreter_previous_matching_input_from_input) .or. &
                   associated(last_command, commands%command_interpreter_next_matching_input_from_input))) then
            ! Starting a new search.
            from = marked_position(command_interpreter_buffers(k)%accum_marker)

            if (from == 0) then
                call current_process(process, status)
                if (.not. status%ok()) return
                from = pmark_position(process)
            end if

            call buffer_substring(from, point(), text, status)
            if (.not. status%ok()) return
            command_interpreter_buffers(k)%matching_input_from_input_string = text
            command_interpreter_buffers(k)%input_ring_index = -1
        end if

        call command_interpreter_previous_matching_input("^"//regular_expression_quote(command_interpreter_buffers( &
                                                                                       k)%matching_input_from_input_string), &
                                                         n, status, .true.)
        if (.not. status%ok()) return
        if (command_interpreter_move_point_for_matching_input%value &
            == COMMAND_INTERPRETER_MOVE_POINT_FOR_MATCHING_INPUT_AFTER_INPUT) call goto_character( &
            original_point)
    end subroutine command_interpreter_previous_matching_input_from_input

    !> Search forwards through input history for match for current input.
    !> (Following history elements are more recent commands.)
    !> With prefix argument N, search for Nth following match.
    !> If N is negative, search backwards for the -Nth previous match.
    !> Port of comint.el:comint-next-matching-input-from-input.
    subroutine command_interpreter_next_matching_input_from_input(n, status)
        integer, intent(in) :: n
        type(status_type), intent(out) :: status

        call command_interpreter_previous_matching_input_from_input(-n, status)
    end subroutine command_interpreter_next_matching_input_from_input

    !> Add COMMAND to the input history.
    !> Ignore duplicates if `command_interpreter_input_ignoredups' is true.  Port of
    !> comint.el:comint-add-to-input-history.
    subroutine command_interpreter_add_to_input_history(command)
        character(len=*), intent(in) :: command
        character(len=:), allocatable :: last
        type(status_type) :: st
        integer :: k, size
        logical :: add
        procedure(command_interpreter_input_filter_procedure), pointer :: input_filter

        call make_local_entry(k)

        if (.not. command_interpreter_buffers(k)%has_ring) then
            call command_interpreter_buffers(k)%input_ring%make(int(command_interpreter_input_ring_size%value))
            command_interpreter_buffers(k)%has_ring = .true.
        end if

        input_filter => command_interpreter_input_filter%get_current()
        add = input_filter(command)

        if (add .and. command_interpreter_input_ignoredups%value .and. .not. command_interpreter_buffers(k)%input_ring%empty_p()) &
            then
            call command_interpreter_buffers(k)%input_ring%reference(0, last, st)
            add = .not. (last == command .and. len(last) == len(command))
        end if

        if (.not. add) return
        ! If `comint-input-ring' is full, maybe grow it.
        size = command_interpreter_buffers(k)%input_ring%size()

        if (size == command_interpreter_buffers(k)%input_ring%count() .and. size < command_interpreter_input_ring_size%value) then
            call command_interpreter_buffers(k)%input_ring%extend(int(min(int(size, int64), &
                                                                          command_interpreter_input_ring_size%value - size)))
        end if

        call command_interpreter_buffers(k)%input_ring%insert(command)
    end subroutine command_interpreter_add_to_input_history

    !> Return an array of arguments from ARGUMENT, backwards.
    !> Break it up at the delimiters in `command_interpreter_delimiter_arguments'.
    !> Characters marked LITERAL (e.g., backslash-escaped characters) are
    !> not considered to be delimiters.  Port of comint.el:comint-delim-arg
    !> (the `literal' text property of the string as an array).
    subroutine command_interpreter_delim_argument(argument, literal, arguments)
        character(len=*), intent(in) :: argument
        logical, intent(in) :: literal(:)
        type(string_type), allocatable, intent(out) :: arguments(:)
        character(len=:), allocatable :: delims
        integer :: position, start, n, k
        character :: ch

        delims = ""
        if (allocated(command_interpreter_delimiter_arguments%value)) delims = command_interpreter_delimiter_arguments%value

        if (len(delims) == 0) then
            call append_string(arguments, argument)
            return
        end if

        allocate (arguments(0))
        n = len(argument)
        position = 1

        do while (position <= n)
            ch = argument(position:position)
            start = position

            if (index(delims, ch) > 0 .and. .not. literal(position)) then
                k = verify(argument(position:n), ch)
                position = merge(n + 1, position + k - 1, k == 0)
            else
                do while (position <= n)
                    if (index(delims, argument(position:position)) > 0 .and. .not. literal(position)) exit
                    position = position + 1
                end do
            end if

            call prepend_string(arguments, argument(start:position - 1))
        end do
    end subroutine command_interpreter_delim_argument

    !> Return from STRING the NTH to MTH arguments.
    !> NTH and/or MTH can be absent, which means the last argument.
    !> NTH and MTH can be negative to count from the end; -1 means
    !> the last argument.
    !> Returned arguments are separated by single spaces.  We assume
    !> whitespace separates arguments, except within quotes and except
    !> for a space or tab that immediately follows a backslash.  Also, a
    !> run of one or more of a single character in
    !> `command_interpreter_delimiter_arguments' is a separate argument.
    !> Argument 0 is the command name.  Port of comint.el:comint-arguments.
    function command_interpreter_arguments(string, nth, mth) result(res)
        character(len=*), intent(in) :: string
        integer, intent(in), optional :: nth, mth
        character(len=:), allocatable :: res
        ! The first line handles ordinary characters and
        ! backslash-sequences.  The second matches "-quoted strings.  The
        ! third matches '-quoted strings.  The fourth matches `-quoted
        ! strings.  This seems to fit the syntax of BASH 2.0.
        character(len=*), parameter :: ARGPART = "[^ "//achar(10)//achar(9)//"""'`\]\|\(\\.\)\|"// &
                                       "\(""\([^""\]\|\\.\)*""\|'[^']*'\|`[^`]*`\)"
        type(string_type), allocatable :: arguments(:), parts(:)
        logical, allocatable :: literal(:)
        type(status_type) :: st
        integer(int64) :: index, mb, me, b1, e1, q
        integer :: position, count, beginning, n, m, k
        logical :: quotes, has_beginning

        allocate (arguments(0))
        allocate (literal(len(string)))
        literal = .false.
        position = 1
        count = 0
        has_beginning = .false.
        beginning = 0
        quotes = .false.

        ! Build a list of all the args until we have as many as we want.
        do
            if (present(mth)) then
                if (mth >= 0 .and. count > mth) exit
            end if

            index = string_match(ARGPART, string, st, start=int(position, int64))
            if (index == 0) exit
            mb = match_beginning(0, st)
            me = match_end(0, st)
            ! Apply the `literal' text property to backslash-escaped
            ! characters, so that `comint-delim-arg' won't break them up.
            b1 = match_beginning(1, st)
            e1 = match_end(1, st)
            if (b1 > 0) literal(b1:e1 - 1) = .true.
            q = match_beginning(2, st)

            if (has_beginning .and. position == mb) then
                ! It's contiguous, part of the same arg.
                position = int(me)
                quotes = quotes .or. q > 0
            else
                ! It's a new separate arg.
                if (has_beginning) call prepend_current_argument()
                count = size(arguments)
                quotes = q > 0
                beginning = int(mb)
                has_beginning = .true.
                position = int(me)
            end if
        end do

        if (has_beginning) call prepend_current_argument()
        count = size(arguments)

        if (.not. present(nth)) then
            n = count - 1
        else if (nth >= 0) then
            n = nth
        else
            n = count + nth
        end if

        if (.not. present(mth)) then
            m = 0
        else if (mth >= 0) then
            m = count - mth - 1
        else
            m = -mth - 1
        end if

        ! (mapconcat #'identity (nthcdr n (nreverse (nthcdr m args))) " "):
        ! ARGUMENTS is backwards, the J'th argument (from 0) being
        ! ARGUMENTS(COUNT - J).
        res = ""

        do k = max(n, 0), count - 1 - m
            if (k > max(n, 0)) res = res//" "
            res = res//arguments(count - k)%text
        end do

    contains

        subroutine prepend_current_argument()
            character(len=:), allocatable :: piece

            piece = string(beginning:position - 1)

            if (quotes) then
                call prepend_string(arguments, piece)
            else
                call command_interpreter_delim_argument(piece, literal(beginning:position - 1), parts)
                arguments = [parts, arguments]
            end if
        end subroutine prepend_current_argument
    end function command_interpreter_arguments

    !> Return FILENAME with environment variables substituted.
    !> Supports additional environment variable syntax of the command
    !> interpreter.  Port of comint.el:comint-substitute-in-file-name.
    function command_interpreter_substitute_in_file_name(filename) result(name)
        use file_input_output_mod, only: substitute_in_file_name
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: name

        name = substitute_in_file_name(filename)
    end function command_interpreter_substitute_in_file_name

    ! ------------------------------------------------------------------
    ! Sending input
    ! ------------------------------------------------------------------

    !> Send input to process.
    !> After the process output mark, sends all text from the process mark
    !> to point as input to the process.  Before the process output mark,
    !> calls value of variable `command_interpreter_get_old_input' to retrieve old
    !> input, copies it to the process mark, and sends it.
    !>
    !> This command also sends and inserts a final newline, unless
    !> NO_NEWLINE is true.
    !>
    !> The functions of `command_interpreter_input_filter_functions' are called on the
    !> input before sending it.  The input is entered into the input
    !> history ring, if the value of variable `command_interpreter_input_filter' returns
    !> true when called on the input.
    !>
    !> If variable `command_interpreter_end_of_line_on_send' is true, then point is moved to
    !> the end of line before sending the input.
    !>
    !> After the input has been sent, if `command_interpreter_process_echoes' is true,
    !> then `command_interpreter_send_input' waits to see if the process outputs a
    !> string matching the input, and if so, deletes that part of the
    !> output.  If ARTIFICIAL is true, it inhibits such deletion.  Callers
    !> sending input not from the user should use ARTIFICIAL = true.
    !>
    !> The values of `command_interpreter_get_old_input', `command_interpreter_input_filter_functions',
    !> and `command_interpreter_input_filter' are chosen according to the command
    !> interpreter running in the buffer.  Port of
    !> comint.el:comint-send-input (input history expansion is Deferred).
    recursive subroutine command_interpreter_send_input(status, no_newline, artificial, sender, run_filters)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_newline, artificial
        !> Send with SENDER instead of `command_interpreter_input_sender' (the binding
        !> comint-skip-input makes).
        procedure(command_interpreter_input_sender_procedure), optional :: sender
        !> False to run no `command_interpreter_input_filter_functions' (the binding
        !> comint-skip-input makes).
        logical, intent(in), optional :: run_filters
        type(handle_type) :: process, pmark
        type(value_type) :: saved, properties
        type(silent_modifications_type) :: silent
        type(status_type) :: st
        character(len=:), allocatable :: intxt, input, history, copy
        integer(int64) :: pm, beginning, end, echo_length, lie, lis, start
        logical :: nonl, art, changed, got, filters, expanded
        integer :: k

        nonl = .false.
        if (present(no_newline)) nonl = no_newline
        art = .false.
        if (present(artificial)) art = artificial
        filters = .true.
        if (present(run_filters)) filters = run_filters
        ! Note that the input string does not include its terminal newline.
        call current_process(process, status)
        if (.not. status%ok()) return
        call make_local_entry(k)
        call widen()
        pmark = process_mark(process)
        pm = pmark_position(process)

        if (point() >= pm) then
            if (command_interpreter_end_of_line_on_send%value) then
                if (command_interpreter_use_prompt_regular_expression%value) then
                    call end_of_line()
                else
                    call goto_character(field_end(point(), .false.))
                end if
            end if

            call buffer_substring(pm, point(), intxt, status)
            if (.not. status%ok()) return
        else
            call execute_get_old_input(copy, status)
            if (.not. status%ok()) return
            call goto_character(pm)
            call insert(copy, status)
            if (.not. status%ok()) return
            intxt = copy
        end if

        if (command_interpreter_input_autoexpand%choice /= COMMAND_INTERPRETER_INPUT_AUTOEXPAND_INPUT) then
            ! Just whatever's already there.
            input = intxt
        else
            ! Expand and leave it visible in buffer.
            expanded = command_interpreter_replace_by_expanded_history(st, silent=.true., start=pm)
            call buffer_substring(pm, point(), input, st)
        end if

        if (command_interpreter_input_autoexpand%choice /= COMMAND_INTERPRETER_INPUT_AUTOEXPAND_HISTORY) then
            history = input
        else
            ! This is messy 'cos ultimately the original functions used do
            ! insertion, rather than return strings.  We have to expand,
            ! then insert back.
            expanded = command_interpreter_replace_by_expanded_history(st, silent=.true., start=pm)
            call buffer_substring(pm, point(), copy, st)
            start = point()
            call insert(input, st)
            call delete_region(pm, start, st)
            history = copy
        end if

        ! Delete and reinsert input.  This seems like a no-op, except for
        ! the resulting entries in the undo list: undoing this insertion
        ! will delete the region, moving the process mark back to its
        ! original position.
        call inhibit_read_only_begin(saved)
        call delete_region(pm, point(), st)
        call insert(input, st)
        call inhibit_read_only_end(saved)
        if (.not. nonl) call insert(achar(10), status)
        call command_interpreter_add_to_input_history(history)

        if (filters) then
            if (nonl) then
                call command_interpreter_input_filter_functions%execute(input, status)
            else
                call command_interpreter_input_filter_functions%execute(input//achar(10), status)
            end if

            if (.not. status%ok()) return
        end if

        call make_local_entry(k)
        beginning = marked_position(pmark)

        if (nonl) then
            end = point()
        else
            end = point() - 1
        end if

        call silent_modifications_begin(silent)

        if (end > beginning) then
            if (command_interpreter_highlight_input%value) then
                properties = list(value_type(name="font_lock_face"), value_type(name="command_interpreter_highlight_input"), &
                                  value_type(name="command_interpreter_fontify_input_inhibit_fontification"), value_type(.true.), &
                                  value_type(name="front_sticky"), value_type(.true.))
                call add_text_properties(beginning, end, properties, changed, st)
            end if

            if (.not. command_interpreter_use_prompt_regular_expression%value) then
                ! Give old user input a field property of `input', to
                ! distinguish it from both process output and unsent input.
                ! The terminating newline is put into a special `boundary'
                ! field to make cursor movement between input and output
                ! fields smoother.
                properties = list(value_type(name="mouse_face"), value_type(name="highlight"), value_type(name="help_echo"), &
                                  value_type(text="mouse-2: insert after prompt as new input"))
                call add_text_properties(beginning, end, properties, changed, st)
            end if
        end if

        if (.not. (nonl .or. command_interpreter_use_prompt_regular_expression%value)) then
            ! Cover the terminating newline.
            properties = rear_nonsticky_plist()
            call add_text_properties(end, end + 1, properties, changed, st)
            properties = list(value_type(name="field"), value_type(name="boundary"), &
                              value_type(name="inhibit_line_move_field_capture"), value_type(.true.))
            call add_text_properties(end, end + 1, properties, changed, st)
        end if

        call silent_modifications_end(silent)
        call command_interpreter_snapshot_last_prompt()
        call make_local_entry(k)
        command_interpreter_buffers(k)%save_input_ring_index = command_interpreter_buffers(k)%input_ring_index
        command_interpreter_buffers(k)%input_ring_index = -1
        ! Update the markers before we send the input in case we get
        ! output amidst sending the input.
        call set_marker(command_interpreter_buffers(k)%last_input_start, pm, st)
        call set_marker(command_interpreter_buffers(k)%last_input_end, point(), st)
        call set_marker(pmark, point(), st)
        ! Clear the "accumulation" marker.
        call detach_marker(command_interpreter_buffers(k)%accum_marker, st)
        command_interpreter_input_sender_no_newline = nonl

        if (present(sender)) then
            call sender(process, input, status)
        else
            call execute_input_sender(process, input, status)
        end if

        command_interpreter_input_sender_no_newline = .false.
        if (.not. status%ok()) return
        ! Optionally delete echoed input (after checking it).
        call make_local_entry(k)

        if (command_interpreter_process_echoes%value .and. .not. art) then
            lis = marked_position(command_interpreter_buffers(k)%last_input_start)
            lie = marked_position(command_interpreter_buffers(k)%last_input_end)
            echo_length = lie - lis

            ! Wait for all input to be echoed.
            do while (lie + echo_length > point_maximum())
                call accept_process_output(got, st, process)
                if (.not. got) exit
                if (.not. same_text(lis, point_maximum() - echo_length, lie, point_maximum())) exit
            end do

            if (lie + echo_length <= point_maximum()) then
                if (same_text(lis, lie, lie, lie + echo_length)) then
                    ! Certain parts of the text to be deleted may have been
                    ! mistaken for prompts.  We have to prevent problems
                    ! when `command_interpreter_prompt_read_only' is .true..
                    call inhibit_read_only_begin(saved)
                    call delete_region(lie, lie + echo_length, st)

                    if (command_interpreter_prompt_read_only%value) then
                        block
                            integer(int64) :: original_point
                            original_point = point()
                            call goto_character(lie)
                            call command_interpreter_update_fence()
                            call goto_character(original_point)
                        end block
                    end if

                    call inhibit_read_only_end(saved)
                end if
            end if
        end if

        ! This used to call comint-output-filter-functions, but that
        ! scrolled the buffer in undesirable ways.
        call make_local_entry(k)
        call set_marker(command_interpreter_buffers(k)%last_output_start, marked_position(pmark), st)
        call command_interpreter_output_filter_functions%execute("", status)

    contains

        !> Whether the text from B1 to E1 is the text from B2 to E2
        !> (compare-buffer-substrings gives 0).
        logical function same_text(b1, e1, b2, e2)
            integer(int64), intent(in) :: b1, e1, b2, e2
            character(len=:), allocatable :: t1, t2
            type(status_type) :: s

            call buffer_substring(max(b1, point_minimum()), min(e1, point_maximum()), t1, s)
            call buffer_substring(max(b2, point_minimum()), min(e2, point_maximum()), t2, s)
            same_text = t1 == t2 .and. len(t1) == len(t2)
        end function same_text
    end subroutine command_interpreter_send_input

    !> The command: send input to process.  Port of the command of
    !> comint.el:comint-send-input.
    !>
    !> Send input to process.
    recursive subroutine execute_command_interpreter_send_input(status)
        type(status_type), intent(out) :: status

        call command_interpreter_send_input(status)
    end subroutine execute_command_interpreter_send_input

    !> Snapshot the current `command_interpreter_last_prompt'.
    !> Freezes the `font_lock_face' text property in place.  Port of
    !> comint.el:comint-snapshot-last-prompt.
    subroutine command_interpreter_snapshot_last_prompt()
        type(silent_modifications_type) :: silent
        type(status_type) :: st
        integer :: k

        k = entry_of(current)
        if (k == 0) return
        if (.not. command_interpreter_buffers(k)%has_last_prompt) return
        call silent_modifications_begin(silent)
        call font_lock_append_text_property(marked_position(command_interpreter_buffers(k)%last_prompt_start), &
                                            marked_position(command_interpreter_buffers(k)%last_prompt_end), &
                                            value_type(name="font_lock_face"), &
                                            value_type(name="command_interpreter_highlight_prompt"), st)
        call silent_modifications_end(silent)
        ! Reset comint-last-prompt so later on comint-output-filter does
        ! not remove the font-lock-face text property of the previous
        ! (this) prompt.
        call forget_last_prompt(command_interpreter_buffers(k))
    end subroutine command_interpreter_snapshot_last_prompt

    !> Default function for sending to PROCESS input STRING.
    !> This just sends STRING plus a newline.  To override this, set the
    !> hook `command_interpreter_input_sender'.  Port of comint.el:comint-simple-send.
    recursive subroutine command_interpreter_simple_send(process, string, status)
        type(handle_type), intent(in) :: process
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status

        if (command_interpreter_input_sender_no_newline) then
            call command_interpreter_send_string(process, string, status)
        else
            ! Sending as two separate strings does not work on Windows, so
            ! concat the \n before sending.
            call command_interpreter_send_string(process, string//achar(10), status)
        end if

        if (.not. status%ok()) return
        if (command_interpreter_input_sender_no_newline .and. len(string) > 0) call process_send_eof(status)
    end subroutine command_interpreter_simple_send

    !> The `command_interpreter_input_sender' comint-skip-input binds: send nothing.
    !> Not a port.
    subroutine ignore_input(process, string, status)
        type(handle_type), intent(in) :: process
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status

        ! (Nothing is sent: PROCESS and STRING are not used.)
        status = status_type()
        if (process%index < 0 .or. len(string) < 0) return
    end subroutine ignore_input

    !> Like `process_send_string', but also does extra bookkeeping for
    !> Comint mode.  Port of comint.el:comint-send-string.
    recursive subroutine command_interpreter_send_string(process, string, status)
        type(handle_type), intent(in) :: process
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status

        call snapshot_in_buffer_of(process)
        call process_send_string(string, status, process)
    end subroutine command_interpreter_send_string

    !> Like `process_send_region', but also does extra bookkeeping for
    !> Comint mode.  Port of comint.el:comint-send-region.
    subroutine command_interpreter_send_region(process, start, end, status)
        type(handle_type), intent(in) :: process
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status

        call snapshot_in_buffer_of(process)
        call process_send_region(start, end, status, process)
    end subroutine command_interpreter_send_region

    !> comint-snapshot-last-prompt in the buffer of PROCESS.  Not a port.
    subroutine snapshot_in_buffer_of(process)
        type(handle_type), intent(in) :: process
        type(handle_type) :: old, buffer
        type(status_type) :: st

        buffer = process_buffer(process)
        if (.not. is_buffer_live(buffer)) return
        old = current_buffer()
        call set_buffer(buffer, st)
        call command_interpreter_snapshot_last_prompt()
        call set_buffer(old, st)
    end subroutine snapshot_in_buffer_of

    ! ------------------------------------------------------------------
    ! Output
    ! ------------------------------------------------------------------

    !> Interpret carriage control characters in the region from START to
    !> END.  Translate carriage return/linefeed sequences to linefeeds.
    !> Make single carriage returns delete to the beginning of the line.
    !> Make backspaces delete the previous character.  Port of
    !> comint.el:comint-carriage-motion.
    recursive subroutine command_interpreter_carriage_motion(start, end)
        integer(int64), intent(in) :: start, end
        type(value_type) :: v, inhibit
        type(handle_type) :: delete_end, saved_point
        type(status_type) :: st
        integer(int64) :: original_point, lbeg, de, moved
        logical :: changed
        integer(int32) :: ch
        logical :: has_delete_end

        call copy_marker_at(point(), saved_point, .false., st)
        call goto_character(start)
        call inhibit_read_only_begin(inhibit)
        lbeg = line_beginning_position()
        has_delete_end = .false.

        ! If the preceding text is marked as "must-overwrite", record it in
        ! delete-end.
        if (start > point_minimum()) then
            call get_text_property(start - 1, value_type(name="command_interpreter_must_overwrite"), v, st)

            if (allocated(v%a)) then
                call copy_marker_at(point(), delete_end, .false., st)
                has_delete_end = .true.
                call remove_text_properties(lbeg, start, list(value_type(name="command_interpreter_must_overwrite"), &
                                                              value_type()), changed, st)
            end if
        end if

        call narrow_to_region(lbeg, end, st)

        ! Handle BS, LF, and CR specially.
        do
            moved = skip_characters_forward("^"//achar(8)//achar(10)//achar(13), st)
            if (is_end_of_buffer()) exit
            ch = following_character()

            if (ch == 8) then
                ! CH = BS
                call delete_character(1_int64, st)
                if (point() > lbeg) call delete_character(-1_int64, st)
            else if (ch == 10) then
                ! CH = LF
                if (has_delete_end) then
                    call marker_position(delete_end, de, st)
                    if (de < point()) call delete_region(lbeg, de, st)
                    call destroy_marker(delete_end, st)
                    has_delete_end = .false.
                end if

                call forward_character(st, 1_int64)
                lbeg = point()
            else
                ! CH = CR
                call delete_character(1_int64, st)

                if (has_delete_end) then
                    call marker_position(delete_end, de, st)

                    if (de < point()) then
                        call set_marker(delete_end, point(), st)
                    end if
                else
                    call copy_marker_at(point(), delete_end, .false., st)
                    has_delete_end = .true.
                end if
            end if
        end do

        if (has_delete_end) then
            call marker_position(delete_end, de, st)

            if (de < point()) then
                ! As there's a text after the last CR, make the current
                ! line contain only that text.
                call delete_region(lbeg, de, st)
            else
                ! Remember that the process output ends with CR, and thus
                ! the next output should overwrite the current line.
                call put_text_property(lbeg, de, value_type(name="command_interpreter_must_overwrite"), value_type(.true.), st)
            end if

            call destroy_marker(delete_end, st)
        end if

        call widen()
        ! (save-excursion restores point.)
        call marker_position(saved_point, original_point, st)
        call goto_character(original_point)
        call destroy_marker(saved_point, st)
        call inhibit_read_only_end(inhibit)
    end subroutine command_interpreter_carriage_motion

    !> The filter of Comint processes: insert STRING (after the
    !> `command_interpreter_preoutput_filter_functions') at the process mark, with
    !> carriage motion, run `command_interpreter_output_filter_functions', mark the text
    !> as output and the last line as the prompt.  Port of
    !> comint.el:comint-output-filter.
    recursive subroutine command_interpreter_output_filter(process, string, status)
        type(handle_type), intent(in) :: process
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        type(handle_type) :: old_process_buffer, saved_point, rbeg, rend, pmark
        type(value_type) :: inhibit
        type(silent_modifications_type) :: silent
        type(status_type) :: st
        character(len=:), allocatable :: text
        integer(int64) :: start, prompt_start, b, e, shortage
        logical :: narrowed, changed
        type(value_type) :: v
        integer :: k

        status = status_type()
        old_process_buffer = process_buffer(process)
        ! First check for closed buffer or no input.
        if (.not. is_buffer_live(old_process_buffer)) return
        call set_buffer(old_process_buffer, status)
        if (.not. status%ok()) return
        call make_local_entry(k)
        ! Run preoutput filters.
        call command_interpreter_preoutput_filter_functions%execute(string, text, status)
        if (.not. status%ok()) return
        ! Insert STRING.
        call inhibit_read_only_begin(inhibit)
        ! The point should float after any insertion we do.
        call copy_marker_at(point(), saved_point, .true., st)
        ! We temporarily remove any buffer narrowing, in case the process
        ! mark is outside of the restriction.
        narrowed = buffers(current)%beginning_of_accessible_portion > 1 .or. buffers(current)%end_of_accessible_portion < buffers( &
                   current)%text%z
        call copy_marker_at(point_minimum(), rbeg, .false., st)
        call copy_marker_at(point_maximum(), rend, .true., st)
        call widen()
        pmark = process_mark(process)
        call goto_character(marked_position(pmark))
        call make_local_entry(k)
        call set_marker(command_interpreter_buffers(k)%last_output_start, point(), st)
        start = point()
        ! insert-before-markers is a bad thing.  (The text is marked as
        ! output once it is inserted, the property being on the string
        ! upstream.)
        call insert(text, status)

        if (status%ok()) then
            call put_text_property(start, point(), value_type(name="field"), value_type(name="output"), st)
            ! Advance process-mark.
            call set_marker(pmark, point(), st)

            if (.not. command_interpreter_inhibit_carriage_motion%value) then
                ! Interpret any carriage motion characters (newline,
                ! backspace).
                call command_interpreter_carriage_motion(marked_position(command_interpreter_buffers(k)%last_output_start), point())
            end if

            ! Run these hooks with point where the user had it.
            call goto_character(marked_position(saved_point))
            call command_interpreter_output_filter_functions%execute(text, status)
            call set_marker(saved_point, point(), st)
            ! In case a filter moved it.
            call goto_character(marked_position(pmark))
            call make_local_entry(k)

            if (.not. command_interpreter_use_prompt_regular_expression%value) then
                call command_interpreter_mark_as_output(marked_position(command_interpreter_buffers(k)%last_output_start), point())
            end if

            ! Highlight the prompt, where we define `prompt' to mean the
            ! most recent output that doesn't end with a newline.
            b = point()
            shortage = forward_line(0_int64)
            prompt_start = point()
            call goto_character(b)

            if (command_interpreter_prompt_read_only%value) then
                call silent_modifications_begin(silent)

                if (point_minimum() /= prompt_start) then
                    call get_text_property(prompt_start - 1, value_type(name="read_only"), v, st)
                    if (.not. allocated(v%a)) call put_text_property(prompt_start - 1, prompt_start, value_type(name="read_only"), &
                                                                     value_type(name="fence"), st)
                end if

                call add_text_properties(prompt_start, point(), list(value_type(name="read_only"), value_type(.true.), &
                                                                     value_type(name="front_sticky"), &
                                                                     list(value_type(name="read_only"))), changed, st)
                call silent_modifications_end(silent)
            end if

            call make_local_entry(k)

            if (command_interpreter_buffers(k)%has_last_prompt) then
                ! There might be some keywords here waiting for
                ! fontification, so no `with-silent-modifications'.
                call font_lock_remove_face_from_text_property(marked_position(command_interpreter_buffers(k)%last_prompt_start), &
                                                              marked_position(command_interpreter_buffers(k)%last_prompt_end), &
                                                              value_type(name="font_lock_face"), &
                                                              value_type(name="command_interpreter_highlight_prompt"), st)
                call forget_last_prompt(command_interpreter_buffers(k))
            end if

            call copy_marker_at(prompt_start, command_interpreter_buffers(k)%last_prompt_start, .false., st)
            call copy_marker_at(point(), command_interpreter_buffers(k)%last_prompt_end, .false., st)
            command_interpreter_buffers(k)%has_last_prompt = .true.
            e = point()
            call font_lock_append_text_property(prompt_start, e, value_type(name="font_lock_face"), &
                                                value_type(name="command_interpreter_highlight_prompt"), st)
            call add_text_properties(prompt_start, e, rear_nonsticky_plist(), changed, st)
        end if

        ! save-restriction.
        if (narrowed) then
            call narrow_to_region(marked_position(rbeg), marked_position(rend), st)
        end if

        call destroy_marker(rbeg, st)
        call destroy_marker(rend, st)
        call goto_character(marked_position(saved_point))
        call destroy_marker(saved_point, st)
        call inhibit_read_only_end(inhibit)
    end subroutine command_interpreter_output_filter

    !> Mark the text from BEGINNING to END as process output.  Port of
    !> comint.el:comint--mark-as-output.
    subroutine command_interpreter_mark_as_output(beginning, end)
        integer(int64), intent(in) :: beginning, end
        type(silent_modifications_type) :: silent
        type(status_type) :: st
        logical :: changed

        call silent_modifications_begin(silent)
        call add_text_properties(beginning, end, rear_nonsticky_plist(), changed, st)
        call add_text_properties(beginning, end, list(value_type(name="front_sticky"), &
                                                      list(value_type(name="field"), &
                                                           value_type(name="inhibit_line_move_field_capture")), &
                                                      value_type(name="field"), value_type(name="output"), &
                                                 value_type(name="inhibit_line_move_field_capture"), value_type(.true.)), changed, &
                                 st)
        ! Text inserted by a user in the middle of process output should be
        ! marked as output.  (Text property hooks are not ported: the
        ! property is set as upstream sets it.)
        call add_text_properties(beginning, end, list(value_type(name="insert_in_front_hooks"), &
                                                      list(value_type(name="command_interpreter__mark_as_output"), &
                                                           value_type(name="command_interpreter_mark_pasted_as_output"))), &
                                 changed, st)
        call silent_modifications_end(silent)
    end subroutine command_interpreter_mark_as_output

    !> Go to the end of buffer in some or all windows showing it.
    !> Do not scroll if the current line is the last line in the buffer.
    !> Depends on the value of `command_interpreter_move_point_for_output' and
    !> `command_interpreter_scroll_show_maximum_output'.
    !>
    !> This function should be in the hook `command_interpreter_output_filter_functions'.
    !> Port of comint.el:comint-postoutput-scroll-to-bottom.
    recursive subroutine command_interpreter_postoutput_scroll_to_bottom(string, status)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        type(handle_type) :: current, process, selected
        type(handle_type), allocatable :: leaves(:)
        type(status_type) :: st
        integer :: i

        status = status_type()
        ! (STRING is not used.)
        if (len(string) < 0) return
        current = current_buffer()
        process = get_buffer_process(current)
        if (process%index == 0) return
        call window_leaves(selected_frame%root_window, leaves)
        selected = selected_frame%selected_window

        do i = 1, size(leaves)
            if (.not. same_handle(windows(leaves(i)%index)%buffer, current)) cycle
            call command_interpreter_adjust_window_point(leaves(i), process)

            ! Optionally scroll to the bottom of the window.
            if (command_interpreter_scroll_show_maximum_output%value) then
                if (window_point_of(leaves(i)) == point_maximum()) then
                    call select_window(leaves(i), st, norecord=.true.)
                    call recenter(prefix_argument_type(PREFIX_NUMBER, -1_int64), .false., st)
                    call select_window(selected, st, norecord=.true.)
                end if
            end if

            call set_buffer(current, st)
        end do
    end subroutine command_interpreter_postoutput_scroll_to_bottom

    !> The point of window W: point, for the selected window showing the
    !> current buffer.  Not a port.
    integer(int64) function window_point_of(w) result(position)
        type(handle_type), intent(in) :: w

        if (same_handle(w, selected_frame%selected_window) .and. same_handle(windows(w%index)%buffer, current_buffer())) then
            position = point()
        else
            position = window_point(windows(w%index))
        end if
    end function window_point_of

    !> Move point in WINDOW based on Comint settings.
    !> For point adjustment use the process-mark of PROCESS.  Port of
    !> comint.el:comint-adjust-window-point.
    subroutine command_interpreter_adjust_window_point(window, process)
        type(handle_type), intent(in) :: window, process
        type(status_type) :: st
        integer(int64) :: wp, pm, los
        logical :: move
        integer :: k

        wp = window_point_of(window)
        pm = pmark_position(process)
        if (.not. (wp < pm)) return

        select case (command_interpreter_move_point_for_output%choice)
        case (COMMAND_INTERPRETER_MOVE_POINT_FOR_OUTPUT_THIS)
            ! Maybe user wants point to jump to end.
            move = same_handle(window, selected_frame%selected_window)
        case (COMMAND_INTERPRETER_MOVE_POINT_FOR_OUTPUT_OTHERS)
            move = .not. same_handle(window, selected_frame%selected_window)
        case default
            ! .true. or COMMAND_INTERPRETER_MOVE_POINT_FOR_OUTPUT_ALL: all windows.
            move = .not. command_interpreter_move_point_for_output%is_off()
        end select

        if (.not. move) then
            ! If point was at the end, keep it at end.
            k = entry_of(current)
            los = 0
            if (k /= 0) los = marked_position(command_interpreter_buffers(k)%last_output_start)
            move = los /= 0 .and. wp >= los
        end if

        if (.not. move) return

        if (same_handle(window, selected_frame%selected_window) .and. &
            same_handle(windows(window%index)%buffer, current_buffer())) then
            call goto_character(pm)
        else
            call set_window_point(windows(window%index), pm, st)
        end if
    end subroutine command_interpreter_adjust_window_point

    !> Truncate the buffer to `command_interpreter_buffer_maximum_size'.
    !> This function could be on `command_interpreter_output_filter_functions' or bound
    !> to a key (STRING is not used).  Port of comint.el:comint-truncate-buffer.
    recursive subroutine command_interpreter_truncate_buffer(string, status)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status

        status = status_type()
        if (len(string) < 0) return
        call truncate_to(command_interpreter_buffer_maximum_size%value, status)
    end subroutine command_interpreter_truncate_buffer

    !> The command.  Truncate the buffer to `command_interpreter_buffer_maximum_size'.
    recursive subroutine execute_command_interpreter_truncate_buffer(status)
        type(status_type), intent(out) :: status

        call command_interpreter_truncate_buffer("", status)
    end subroutine execute_command_interpreter_truncate_buffer

    !> comint-truncate-buffer with comint-buffer-maximum-size MAXIMUM.
    !> Port of comint.el:comint-truncate-buffer (its body).
    subroutine truncate_to(maximum, status)
        integer(int64), intent(in) :: maximum
        type(status_type), intent(out) :: status
        type(handle_type) :: process
        type(value_type) :: inhibit
        integer(int64) :: original_point, shortage

        call current_process(process, status)
        if (.not. status%ok()) return
        original_point = point()
        call goto_character(pmark_position(process))
        shortage = forward_line(-maximum)
        call beginning_of_line()
        call inhibit_read_only_begin(inhibit)
        call delete_region(point_minimum(), point(), status)
        call inhibit_read_only_end(inhibit)
        call goto_character(min(original_point, point_maximum()))
    end subroutine truncate_to

    !> Strip trailing `^M' characters from the current output group.
    !> This function could be on `command_interpreter_output_filter_functions' or bound
    !> to a key (STRING is not used).  Port of comint.el:comint-strip-ctrl-m.
    subroutine command_interpreter_strip_control_m(string, status)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status

        status = status_type()
        if (len(string) < 0) return
        call strip_control_m(status, .false.)
    end subroutine command_interpreter_strip_control_m

    !> comint-strip-ctrl-m, called interactively if INTERACTIVE.
    !> Port of comint.el:comint-strip-ctrl-m (its body).
    subroutine strip_control_m(status, interactive)
        use search_mod, only: regular_expression_search_forward, replace_match
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: interactive
        type(handle_type) :: process
        type(status_type) :: st
        integer(int64) :: original_point, found, pm, from
        logical :: inter
        integer :: k

        status = status_type()
        inter = .false.
        if (present(interactive)) inter = interactive
        process = get_buffer_process(current_buffer())

        if (process%index == 0) then
            ! This function may be used in `comint-output-filter-functions',
            ! and in that case, if there's no process, then we should do
            ! nothing.  If interactive, report an error.
            if (inter) call set_error(status, GENERIC_ERROR_CODE, "No process in the current buffer")
            return
        end if

        pm = pmark_position(process)
        original_point = point()
        k = entry_of(current)
        from = 0

        if (k /= 0) then
            if (inter) then
                from = marked_position(command_interpreter_buffers(k)%last_input_end)
            else
                from = marked_position(command_interpreter_buffers(k)%last_output_start)
            end if
        end if

        if (from > 0) call goto_character(from)

        do
            call regular_expression_search_forward(achar(13)//"+$", found, st, bound=pm, noerror=SEARCH_NOERROR)
            if (found == 0) exit
            call replace_match("", st, fixedcase=.true., literal=.true.)
        end do

        call goto_character(original_point)
    end subroutine strip_control_m

    !> The command.  Strip trailing `^M' characters from the current
    !> output group.
    subroutine execute_command_interpreter_strip_control_m(status)
        type(status_type), intent(out) :: status

        call strip_control_m(status, .true.)
    end subroutine execute_command_interpreter_strip_control_m

    !> Put the end of the buffer at the bottom of the window.  Port of
    !> comint.el:comint-show-maximum-output.
    subroutine command_interpreter_show_maximum_output(status)
        type(status_type), intent(out) :: status

        call goto_character(point_maximum())
        call recenter(prefix_argument_type(PREFIX_NUMBER, -1_int64), .false., status)
    end subroutine command_interpreter_show_maximum_output

    ! ------------------------------------------------------------------
    ! Old input
    ! ------------------------------------------------------------------

    !> Default for `command_interpreter_get_old_input'.
    !> If `command_interpreter_use_prompt_regular_expression' is false, then either return the
    !> current input field, if point is on an input field, or the current
    !> line, if point is on an output field.  If `command_interpreter_use_prompt_regular_expression'
    !> is true, then return the current line with any initial string
    !> matching the regexp `command_interpreter_prompt_regular_expression' removed.  Port of
    !> comint.el:comint-get-old-input-default.
    recursive subroutine command_interpreter_get_old_input_default(input, status)
        character(len=:), allocatable, intent(out) :: input
        type(status_type), intent(out) :: status
        type(value_type) :: field_property
        integer(int64) :: bof, start, end

        status = status_type()
        field_property = value_type()

        if (.not. command_interpreter_use_prompt_regular_expression%value) then
            bof = field_beginning(point(), .false.)
            field_property = field_at(bof)

            ! Make sure we're in an input rather than output field.
            if (.not. allocated(field_property%a)) then
                input = field_string_no_properties(bof)
                return
            end if
        end if

        call command_interpreter_beginning_of_line(.false.)
        start = point()

        if (command_interpreter_use_prompt_regular_expression%value .or. symbol_is(field_property, "output")) then
            end = line_end_position()
        else
            end = field_end(point(), .false.)
        end if

        call buffer_substring(start, end, input, status)
    end subroutine command_interpreter_get_old_input_default

    !> Insert after prompt old input at point as new input to be edited.
    !> Calls `command_interpreter_get_old_input' to get old input.  Port of
    !> comint.el:comint-copy-old-input.
    subroutine command_interpreter_copy_old_input(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: process
        character(len=:), allocatable :: input

        call execute_get_old_input(input, status)
        if (.not. status%ok()) return
        call current_process(process, status)
        if (.not. status%ok()) return
        call goto_character(pmark_position(process))
        call insert(input, status)
    end subroutine command_interpreter_copy_old_input

    !> Skip past the text matching regexp `command_interpreter_prompt_regular_expression'.
    !> If this takes us past the end of the current line, don't skip at
    !> all.  Port of comint.el:comint-skip-prompt.
    subroutine command_interpreter_skip_prompt()
        type(status_type) :: st
        integer(int64) :: e

        if (looking_at(command_interpreter_prompt_regular_expression%value, st)) then
            e = match_end(0, st)
            if (e <= line_end_position()) call goto_character(e)
        end if
    end subroutine command_interpreter_skip_prompt

    !> Return true if point is after the process output marker.  Port of
    !> comint.el:comint-after-pmark-p.
    logical function is_command_interpreter_after_pmark()
        type(handle_type) :: process

        is_command_interpreter_after_pmark = .false.
        process = get_buffer_process(current_buffer())
        if (process%index /= 0) is_command_interpreter_after_pmark = pmark_position(process) <= point()
    end function is_command_interpreter_after_pmark

    !> Return the buffer position of the beginning of the line, after any
    !> prompt.  If `command_interpreter_use_prompt_regular_expression' is true, then the prompt
    !> skip is done by skipping text matching the regular expression
    !> `command_interpreter_prompt_regular_expression', a buffer local variable.  Port of
    !> comint.el:comint-line-beginning-position.
    integer(int64) function command_interpreter_line_beginning_position() result(position)
        integer(int64) :: original_point, line_beginning

        if (command_interpreter_use_prompt_regular_expression%value) then
            ! Use comint-prompt-regexp.
            original_point = point()
            call beginning_of_line()
            call command_interpreter_skip_prompt()
            position = point()
            call goto_character(original_point)
        else
            ! Use input fields.  Note that, unlike the behavior of
            ! `line-beginning-position' inside a field, this function will
            ! return the position of the end of a prompt, even if the point
            ! is already inside the prompt.  In order to do this, it assumes
            ! that if there are two fields on a line, then the first one is
            ! the prompt, and the second one is an input field, and is
            ! front-sticky (as input fields should be).
            if (symbol_is(field_at_position(point()), "output")) then
                line_beginning = line_beginning_position()
            else
                line_beginning = field_beginning(point(), .false.)
            end if

            position = constrain_to_field(line_beginning, line_end_position(), .false., .false.)
        end if
    end function command_interpreter_line_beginning_position

    !> The field at POSITION, as editfns.c:Ffield_at_pos gives it (the field
    !> of the text before POSITION unless the field after it is front-sticky).
    !> Not a port.
    function field_at_position(position) result(v)
        use editing_functions_mod, only: get_position_property
        integer(int64), intent(in) :: position
        type(value_type) :: v

        v = get_position_property(position, value_type(name="field"))
    end function field_at_position

    !> Go to the beginning of line, then skip past the prompt, if any.
    !> If prefix argument is given (\[universal_argument]) the prompt is
    !> not skipped.  If `command_interpreter_use_prompt_regular_expression' is true, then the prompt
    !> skip is done by skipping text matching the regular expression
    !> `command_interpreter_prompt_regular_expression', a buffer local variable.  Port of
    !> comint.el:comint-bol.
    subroutine command_interpreter_beginning_of_line(argument)
        logical, intent(in) :: argument
        integer(int64) :: shortage

        if (argument) then
            ! Unlike `beginning-of-line', forward-line ignores field
            ! boundaries.
            shortage = forward_line(0_int64)
        else
            call goto_character(command_interpreter_line_beginning_position())
        end if
    end subroutine command_interpreter_beginning_of_line

    ! ------------------------------------------------------------------
    ! Output commands
    ! ------------------------------------------------------------------

    !> Delete all output from interpreter since last input.
    !> If KILL (interactively, the prefix), save the cut text in the
    !> clipboard history.
    !>
    !> This command does not delete the prompt.  Port of
    !> comint.el:comint-delete-output.
    recursive subroutine command_interpreter_delete_output(status, kill)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: kill
        type(handle_type) :: process
        type(value_type) :: inhibit
        type(status_type) :: st
        character(len=:), allocatable :: replacement, prompt
        integer(int64) :: original_point, pm, lie, shortage
        integer :: k

        call current_process(process, status)
        if (.not. status%ok()) return
        call make_local_entry(k)
        call inhibit_read_only_begin(inhibit)
        original_point = point()
        call goto_character(pmark_position(process))
        shortage = forward_line(0_int64)
        pm = point()
        lie = marked_position(command_interpreter_buffers(k)%last_input_end)

        if (present(kill)) then
            if (kill) call copy_region_quietly(lie, pm, st)
        end if

        call delete_region(lie, pm, st)
        pm = lie
        call goto_character(pmark_position(process))
        call buffer_substring(pm, point(), prompt, st)
        replacement = "*** output flushed ***"//achar(10)//prompt
        call delete_region(pm, point(), st)

        if (original_point <= point_maximum()) then
            call goto_character(original_point)
        else
            call goto_character(point_maximum())
        end if

        call inhibit_read_only_end(inhibit)
        ! Output message and put back prompt.
        call command_interpreter_output_filter(process, replacement, status)
    end subroutine command_interpreter_delete_output

    !> The command: with a prefix argument, kill the output.
    !>
    !> Delete all output from interpreter since last input.
    recursive subroutine execute_command_interpreter_delete_output(status)
        type(status_type), intent(out) :: status

        call command_interpreter_delete_output(status, current_prefix_argument%kind /= PREFIX_NONE)
    end subroutine execute_command_interpreter_delete_output

    !> Write output from interpreter since last input to FILENAME.
    !> Any prompt at the end of the output is not written.
    !>
    !> If the optional argument APPEND (the prefix argument when
    !> interactive) is true, the output is appended to the file instead
    !> (Deferred: write_region has no APPEND yet, an error says so).  Port of
    !> comint.el:comint-write-output (MUSTBENEW is Deferred).
    subroutine command_interpreter_write_output(filename, status, append)
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: append
        type(handle_type) :: process
        integer(int64) :: original_point, shortage, e
        character(len=:), allocatable :: message_text
        integer :: k

        if (present(append)) then
            if (append) then
                call set_error(status, GENERIC_ERROR_CODE, "Appending output to a file is not supported yet")
                return
            end if
        end if

        call current_process(process, status)
        if (.not. status%ok()) return
        call make_local_entry(k)
        original_point = point()
        call goto_character(pmark_position(process))
        shortage = forward_line(0_int64)
        e = point()
        call goto_character(original_point)
        call write_region(filename, .false., status, start=marked_position(command_interpreter_buffers(k)%last_input_end), end=e, &
                          message=message_text)
        if (status%ok() .and. allocated(message_text)) call message(message_text)
    end subroutine command_interpreter_write_output

    !> The command: read the file name.
    !>
    !> Write output from interpreter since last input to FILENAME.
    subroutine execute_command_interpreter_write_output(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        logical :: append

        append = current_prefix_argument%kind /= PREFIX_NONE

        if (append) then
            call read_file_name("Append output to file: ", file, status)
        else
            call read_file_name("Write output to file: ", file, status)
        end if

        if (.not. status%ok()) return
        call command_interpreter_write_output(file, status, append)
    end subroutine execute_command_interpreter_write_output

    !> Append output from interpreter since last input to FILENAME.
    !> Any prompt at the end of the output is not written.  Port of
    !> comint.el:comint-append-output-to-file.
    subroutine command_interpreter_append_output_to_file(filename, status)
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status

        call command_interpreter_write_output(filename, status, .true.)
    end subroutine command_interpreter_append_output_to_file

    !> Display start of this batch of interpreter output at top of window.
    !> Sets mark to the value of point when this command is run.  Port of
    !> comint.el:comint-show-output.
    subroutine command_interpreter_show_output(status)
        type(status_type), intent(out) :: status
        type(status_type) :: st
        integer(int64) :: position
        integer :: k

        status = status_type()
        call push_mark()
        position = 0
        k = entry_of(current)
        if (k /= 0) position = marked_position(command_interpreter_buffers(k)%last_input_end)
        if (position == 0) position = point_maximum()

        if (command_interpreter_use_prompt_regular_expression%value) then
            call goto_character(position)
            call beginning_of_line(0_int64)
            call set_window_start(windows(selected_frame%selected_window%index), point(), st)
            call command_interpreter_skip_prompt()
        else
            call goto_character(field_beginning(position, .false.))
            call set_window_start(windows(selected_frame%selected_window%index), point(), st)
        end if
    end subroutine command_interpreter_show_output

    !> Clear the comint buffer.  Port of comint.el:comint-clear-buffer.
    subroutine command_interpreter_clear_buffer(status)
        type(status_type), intent(out) :: status

        call truncate_to(0_int64, status)
    end subroutine command_interpreter_clear_buffer

    !> Interrupt the current subjob.  Port of
    !> comint.el:comint-interrupt-subjob.
    recursive subroutine command_interpreter_interrupt_subjob(status)
        type(status_type), intent(out) :: status

        call command_interpreter_skip_input(status)
        if (.not. status%ok()) return
        call interrupt_process(status, current_group=is_command_interpreter_pty())
    end subroutine command_interpreter_interrupt_subjob

    !> Send kill signal to the current subjob.  Port of
    !> comint.el:comint-kill-subjob.
    recursive subroutine comint_stop_subjob(status)
        type(status_type), intent(out) :: status

        call command_interpreter_skip_input(status)
        if (.not. status%ok()) return
        call stop_process(status, current_group=is_command_interpreter_pty())
    end subroutine comint_stop_subjob

    !> Send quit signal to the current subjob.  Port of
    !> comint.el:comint-quit-subjob.
    recursive subroutine command_interpreter_quit_subjob(status)
        type(status_type), intent(out) :: status

        call command_interpreter_skip_input(status)
        if (.not. status%ok()) return
        call quit_process(status, current_group=is_command_interpreter_pty())
    end subroutine command_interpreter_quit_subjob

    !> Skip all pending input, from last stuff output by interpreter to
    !> point.  This means mark it as if it had been sent as input, without
    !> sending it.  The command keys used to trigger the command that
    !> called this function are inserted into the buffer.  Port of
    !> comint.el:comint-skip-input.
    recursive subroutine command_interpreter_skip_input(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: process, marker
        type(value_type) :: inhibit
        type(status_type) :: st
        integer(int64) :: position
        character(len=:), allocatable :: keys

        call command_interpreter_send_input(status, .true., .true., ignore_input, .false.)
        if (.not. status%ok()) return
        call end_of_line()
        position = point()
        call current_process(process, status)
        if (.not. status%ok()) return
        marker = process_mark(process)
        call inhibit_read_only_begin(inhibit)
        keys = ""
        if (allocated(this_command_keys)) keys = key_description(this_command_keys)
        call insert("  "//keys, st)
        if (marked_position(marker) == position) call set_marker(marker, point(), st)
        call inhibit_read_only_end(inhibit)
    end subroutine command_interpreter_skip_input

    !> Cut all text from last stuff output by interpreter to point.  Port of
    !> comint.el:comint-kill-input.
    subroutine command_interpreter_cut_input(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: process
        integer(int64) :: pm

        call current_process(process, status)
        if (.not. status%ok()) return
        pm = pmark_position(process)
        if (point() > pm) call cut_region(pm, point(), status)
    end subroutine command_interpreter_cut_input

    !> Behave like the global binding or send an EOF to subprocess.
    !> Sends an EOF only if point is at the end of the buffer and there is
    !> no input.  Port of comint.el:comint-delchar-or-maybe-eof.
    recursive subroutine command_interpreter_delchar_or_maybe_eof(status)
        use keymap_mod, only: is_keymap
        use value_mod, only: make_keymap_reference, value_type
        type(status_type), intent(out) :: status
        type(handle_type) :: process
        type(value_type) :: command, remap
        logical :: eof

        process = get_buffer_process(current_buffer())
        eof = .false.
        if (is_end_of_buffer() .and. process%index /= 0) eof = point() == pmark_position(process)

        if (eof) then
            call command_interpreter_send_eof(status)
        else
            call lookup_key(make_keymap_reference(current_global_map()), this_command_keys, command, status)
            if (.not. status%ok()) return
            call command_remapping(command, remap)
            if (allocated(remap%a)) command = remap
            call execute_interactively(command, status)
        end if
    end subroutine command_interpreter_delchar_or_maybe_eof

    !> Send an EOF to the current buffer's process.  Port of
    !> comint.el:comint-send-eof.
    recursive subroutine command_interpreter_send_eof(status)
        type(status_type), intent(out) :: status

        call command_interpreter_send_input(status, .true., .true.)
        if (.not. status%ok()) return
        call process_send_eof(status)
    end subroutine command_interpreter_send_eof

    !> Search backward through buffer for input fields that match REGULAR_EXPRESSION.
    !> If `command_interpreter_use_prompt_regular_expression' is true, then input fields are
    !> identified by lines that match `command_interpreter_prompt_regular_expression'.
    !>
    !> With prefix argument N, search for Nth previous match.
    !> If N is negative, find the next or Nth next match.  Port of
    !> comint.el:comint-backward-matching-input.
    subroutine command_interpreter_backward_matching_input(regular_expression, n_in, status)
        character(len=*), intent(in) :: regular_expression
        integer, intent(in) :: n_in
        type(status_type), intent(out) :: status
        integer(int64) :: original_point, found, position
        integer :: n, directory
        type(value_type) :: f

        status = status_type()
        n = n_in
        original_point = point()

        if (command_interpreter_use_prompt_regular_expression%value) then
            ! Use comint-prompt-regexp.
            if (n > 0) then
                call end_of_line(0_int64)
            else
                call end_of_line(1_int64)
            end if

            call regular_expression_search_backward(command_interpreter_prompt_regular_expression%value//".*"//regular_expression, &
                                                    found, &
                                                    status, noerror=SEARCH_NOERROR, &
                                                    count=int(n, int64))
            position = point()
            call goto_character(original_point)

            if (found == 0) then
                call message("Not found")
                call ding(status=status)
            else
                call goto_character(position)
                call command_interpreter_beginning_of_line(.false.)
            end if
        else
            ! Use input fields.
            directory = merge(-1, 1, n < 0)

            do while (n /= 0)
                call regular_expression_search_backward(regular_expression, found, status, noerror=SEARCH_NOERROR, &
                                                        count=int(directory, int64))

                if (found == 0) then
                    call goto_character(original_point)
                    call set_error(status, USER_ERROR_CODE, "Not found")
                    return
                end if

                f = field_at(point())
                if (.not. allocated(f%a)) n = n - directory
            end do

            position = field_beginning(point(), .false.)
            call goto_character(position)
        end if
    end subroutine command_interpreter_backward_matching_input

    !> Search forward through buffer for input fields that match REGULAR_EXPRESSION.
    !> If `command_interpreter_use_prompt_regular_expression' is true, then input fields are
    !> identified by lines that match `command_interpreter_prompt_regular_expression'.
    !>
    !> With prefix argument N, search for Nth following match.
    !> If N is negative, find the previous or Nth previous match.  Port of
    !> comint.el:comint-forward-matching-input.
    subroutine command_interpreter_forward_matching_input(regular_expression, n, status)
        character(len=*), intent(in) :: regular_expression
        integer, intent(in) :: n
        type(status_type), intent(out) :: status

        call command_interpreter_backward_matching_input(regular_expression, -n, status)
    end subroutine command_interpreter_forward_matching_input

    !> Move to end of Nth next prompt in the buffer.
    !> If `command_interpreter_use_prompt_regular_expression' is false, then this means the
    !> beginning of the Nth next `input' field, otherwise, it means the Nth
    !> occurrence of text matching `command_interpreter_prompt_regular_expression'.  Port of
    !> comint.el:comint-next-prompt.
    subroutine command_interpreter_next_prompt(n_in)
        integer, intent(in) :: n_in
        character(len=:), allocatable :: saved
        integer(int64) :: position, input_position, previous_position, left
        logical :: has_saved, has_input
        integer :: n

        n = n_in

        if (command_interpreter_use_prompt_regular_expression%value) then
            ! Use comint-prompt-regexp.
            has_saved = allocated(paragraph_start%value)
            if (has_saved) saved = paragraph_start%value
            paragraph_start%value = command_interpreter_prompt_regular_expression%value

            if (n > 0) then
                call end_of_line(1_int64)
            else
                call end_of_line(0_int64)
            end if

            left = forward_paragraph(int(n, int64))
            call command_interpreter_skip_prompt()

            if (has_saved) then
                paragraph_start%value = saved
            else
                deallocate (paragraph_start%value)
            end if
        else
            ! Use input fields.
            position = point()
            has_input = .false.
            input_position = 0

            do while (n /= 0)
                previous_position = position

                if (n > 0) then
                    position = next_single_character_property_change(position, value_type(name="field"))
                else
                    position = previous_single_character_property_change(position, value_type(name="field"))
                end if

                if (position == previous_position) then
                    ! Ran off the end of the buffer.
                    if (n > 0) then
                        ! There's always an input field at the end of the
                        ! buffer, but it has a `field' property of nil.
                        input_position = point_maximum()
                        has_input = .true.
                    end if

                    ! Stop iterating.
                    n = 0
                else if (vtag(field_at(position)) == V_NULL) then
                    if (n < 0) then
                        n = n + 1
                    else
                        n = n - 1
                    end if

                    input_position = position
                    has_input = .true.
                end if
            end do

            if (has_input) call goto_character(input_position)
        end if
    end subroutine command_interpreter_next_prompt

    !> Move to end of Nth previous prompt in the buffer.
    !> If `command_interpreter_use_prompt_regular_expression' is false, then this means the
    !> beginning of the Nth previous `input' field, otherwise, it means the
    !> Nth occurrence of text matching `command_interpreter_prompt_regular_expression'.  Port of
    !> comint.el:comint-previous-prompt.
    subroutine command_interpreter_previous_prompt(n)
        integer, intent(in) :: n

        call command_interpreter_next_prompt(-n)
    end subroutine command_interpreter_previous_prompt

    !> Update read-only status of newline before point.
    !> The `fence' read-only property is used to indicate that a newline
    !> is read-only for no other reason than to "fence off" a following
    !> front-sticky read-only region.  This is used to implement comint
    !> read-only prompts.  If the text after a newline changes, the
    !> read-only status of that newline may need updating.  That is what
    !> this function does.
    !>
    !> This function does nothing if point is not at the beginning of a
    !> line, or is at the beginning of the accessible portion of the
    !> buffer.  Otherwise, if the character after point has a front-sticky
    !> read-only property, then the preceding newline is given a read-only
    !> property of `fence', unless it already is read-only.  If the
    !> character after point does not have a front-sticky read-only
    !> property, any read-only property of `fence' on the preceding newline
    !> is removed.  Port of comint.el:comint-update-fence.
    subroutine command_interpreter_update_fence()
        type(silent_modifications_type) :: silent
        type(status_type) :: st
        type(value_type) :: lst, ro, previous_ro
        integer(int64) :: point_value
        logical :: sticky, changed

        point_value = point()
        if (.not. is_beginning_of_line() .or. is_beginning_of_buffer()) return
        call get_text_property(point_value, value_type(name="front_sticky"), lst, st)
        call get_text_property(point_value, value_type(name="read_only"), ro, st)
        call silent_modifications_begin(silent)
        sticky = .false.
        if (allocated(ro%a)) sticky = front_sticky_read_only(lst)
        call get_text_property(point_value - 1, value_type(name="read_only"), previous_ro, st)

        if (sticky) then
            if (.not. allocated(previous_ro%a)) call put_text_property(point_value - 1, point_value, value_type(name="read_only"), &
                                                                       value_type(name="fence"), st)
        else if (symbol_is(previous_ro, "fence")) then
            call remove_list_of_text_properties(point_value - 1, point_value, list(value_type(name="read_only")), changed, st)
        end if

        call silent_modifications_end(silent)
    end subroutine command_interpreter_update_fence

    !> Whether a front-sticky value LST covers `read_only': (if (listp lst)
    !> (memq 'read-only lst) t).  Not a port.
    logical function front_sticky_read_only(lst) result(yes)
        type(value_type), intent(in) :: lst
        type(value_type), allocatable :: elements(:)
        integer :: i

        if (.not. allocated(lst%a)) then
            yes = .false.
        else if (vtag(lst) == V_ARRAY) then
            yes = .false.
            call list_elements(lst, elements)

            do i = 1, size(elements)
                if (symbol_is(elements(i), "read_only")) yes = .true.
            end do
        else
            yes = .true.
        end if
    end function front_sticky_read_only

    !> Cut current line, ignoring read-only and field properties.
    !> With prefix arg COUNT, cut that many lines starting from the
    !> current line.  If COUNT is negative, cut backward.  Also cut the
    !> preceding newline, instead of the trailing one.
    !> If COUNT is zero, cut current line but exclude the trailing
    !> newline.  The read-only status of newlines is updated with
    !> `command_interpreter_update_fence', if necessary.  Port of
    !> comint.el:comint-kill-whole-line.
    subroutine command_interpreter_cut_whole_line(count, status)
        integer(int64), intent(in) :: count
        type(status_type), intent(out) :: status
        type(value_type) :: inhibit
        logical :: saved

        call inhibit_read_only_begin(inhibit)
        saved = inhibit_field_text_motion
        inhibit_field_text_motion = .true.
        call cut_whole_line(count, status)
        if (count >= 0) call command_interpreter_update_fence()
        inhibit_field_text_motion = saved
        call inhibit_read_only_end(inhibit)
    end subroutine command_interpreter_cut_whole_line

    !> Like `cut_region', but ignore read-only properties, if safe.
    !> This command assumes that the buffer contains read-only "prompts"
    !> which are regions with front-sticky read-only properties at the
    !> beginning of a line, with the preceding newline being read-only to
    !> protect the prompt.  This is true of the comint prompts if
    !> `command_interpreter_prompt_read_only' is true.  This command will not delete the
    !> region if this would create mutilated or out of place prompts.  That
    !> is, if any part of a prompt is deleted, the entire prompt must be
    !> deleted and all remaining prompts should stay at the beginning of a
    !> line.  If this is not the case, this command just calls
    !> `cut_region' with all read-only properties intact.  The read-only
    !> status of newlines is updated using `command_interpreter_update_fence', if
    !> necessary.  Port of comint.el:comint-kill-region.
    subroutine command_interpreter_cut_region(beginning, end, status)
        integer(int64), intent(in) :: beginning, end
        type(status_type), intent(out) :: status
        type(value_type) :: inhibit, lst, ro
        type(status_type) :: st
        integer(int64) :: original_point, true_beginning, true_end
        logical :: beginning_is_beginning_of_line, end_is_beginning_of_line, beginning_bad, end_bad

        original_point = point()
        true_beginning = min(beginning, end)
        true_end = max(beginning, end)
        call goto_character(true_beginning)
        beginning_is_beginning_of_line = is_beginning_of_line()
        call get_text_property(true_beginning, value_type(name="front_sticky"), lst, st)
        call get_text_property(true_beginning, value_type(name="read_only"), ro, st)
        beginning_bad = .false.
        if (allocated(ro%a)) beginning_bad = front_sticky_read_only(lst)
        call goto_character(true_end)
        end_is_beginning_of_line = is_beginning_of_line()
        call get_text_property(true_end, value_type(name="front_sticky"), lst, st)
        call get_text_property(true_end, value_type(name="read_only"), ro, st)
        end_bad = .false.
        if (allocated(ro%a)) end_bad = front_sticky_read_only(lst)
        call goto_character(original_point)

        if ((.not. beginning_is_beginning_of_line .and. (beginning_bad .or. end_bad)) .or. (.not. end_is_beginning_of_line &
                                                                                            .and. end_bad)) then
            call cut_region(beginning, end, status)
        else
            call inhibit_read_only_begin(inhibit)
            call cut_region(beginning, end, status)
            original_point = point()
            call command_interpreter_update_fence()
            call goto_character(original_point)
            call inhibit_read_only_end(inhibit)
        end if
    end subroutine command_interpreter_cut_region

    !> After fetching a line from input history, this fetches the following
    !> line.  In other words, this recalls the input line after the line
    !> you recalled last.  You can use this to repeat a sequence of input
    !> lines.  Port of comint.el:comint-get-next-from-history.
    subroutine command_interpreter_get_next_from_history(status)
        type(status_type), intent(out) :: status
        integer :: k

        status = status_type()
        k = entry_of(current)

        if (k /= 0) then
            if (command_interpreter_buffers(k)%save_input_ring_index >= 0) then
                command_interpreter_buffers(k)%input_ring_index = command_interpreter_buffers(k)%save_input_ring_index + 1
                call command_interpreter_next_input(1, status)
                return
            end if
        end if

        call message("No previous history command")
    end subroutine command_interpreter_get_next_from_history

    !> Accumulate a line to send as input along with more lines.
    !> This inserts a newline so that you can enter more text to be sent
    !> along with this line.  Use \[command_interpreter_send_input] to send all the
    !> accumulated input, at once.  The entire accumulated text becomes one
    !> item in the input history when you send it.  Port of
    !> comint.el:comint-accumulate.
    subroutine command_interpreter_accumulate(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: process
        type(value_type) :: inhibit
        type(status_type) :: st
        character(len=:), allocatable :: text
        integer(int64) :: pm, am
        integer :: k

        status = status_type()
        call make_local_entry(k)
        process = get_buffer_process(current_buffer())

        if (process%index /= 0) then
            pm = pmark_position(process)
            am = marked_position(command_interpreter_buffers(k)%accum_marker)

            if (am == 0) then
                call set_marker(command_interpreter_buffers(k)%accum_marker, pm, st)
                am = pm
            end if

            if (point() >= am .and. am >= pm) then
                ! Delete and reinsert input.  This seems like a no-op,
                ! except for the resulting entries in the undo list:
                ! undoing this insertion will delete the region, moving the
                ! accumulation marker back to its original position.
                call buffer_substring(am, point(), text, st)
                call inhibit_read_only_begin(inhibit)
                call delete_region(am, point(), st)
                call insert(text, st)
                call inhibit_read_only_end(inhibit)
            end if
        end if

        call insert(achar(10), status)
        if (.not. status%ok()) return
        call make_local_entry(k)
        call set_marker(command_interpreter_buffers(k)%accum_marker, point(), st)

        if (command_interpreter_buffers(k)%input_ring_index >= 0) then
            command_interpreter_buffers(k)%save_input_ring_index = command_interpreter_buffers(k)%input_ring_index - 1
        end if
    end subroutine command_interpreter_accumulate

    !> Move point to the process mark.
    !> The process mark separates output, and input already sent, from
    !> input that has not yet been sent.  Port of
    !> comint.el:comint-goto-process-mark.
    subroutine command_interpreter_goto_process_mark(status, interactive)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: interactive
        type(handle_type) :: process

        call current_process(process, status)
        if (.not. status%ok()) return
        call goto_character(pmark_position(process))

        if (present(interactive)) then
            if (interactive) call message("Point is now at the process mark")
        end if
    end subroutine command_interpreter_goto_process_mark

    !> The command.  Move point to the process mark.
    subroutine execute_command_interpreter_goto_process_mark(status)
        type(status_type), intent(out) :: status

        call command_interpreter_goto_process_mark(status, .true.)
    end subroutine execute_command_interpreter_goto_process_mark

    !> Move point to beginning of line (after prompt) or to the process
    !> mark.  The first time you use this command, it moves to the
    !> beginning of the line (but after the prompt, if any).  If you repeat
    !> it again immediately, it moves point to the process mark.
    !>
    !> The process mark separates the process output, along with input
    !> already sent, from input that has not yet been sent.  Ordinarily,
    !> the process mark is at the beginning of the current input line; but
    !> if you have used \[command_interpreter_accumulate] to send multiple lines at once,
    !> the process mark is at the beginning of the accumulated input.  Port of
    !> comint.el:comint-bol-or-process-mark.
    subroutine command_interpreter_beginning_of_line_or_process_mark(status)
        type(status_type), intent(out) :: status

        status = status_type()

        if (.not. associated(last_command, commands%command_interpreter_beginning_of_line_or_process_mark)) then
            call command_interpreter_beginning_of_line(.false.)
        else
            call command_interpreter_goto_process_mark(status)
        end if
    end subroutine command_interpreter_beginning_of_line_or_process_mark

    !> Set the process mark at point.  Port of
    !> comint.el:comint-set-process-mark.
    subroutine command_interpreter_set_process_mark(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: process
        type(status_type) :: st

        call current_process(process, status)
        if (.not. status%ok()) return
        call set_marker(process_mark(process), point(), st)
        call message("Process mark set")
    end subroutine command_interpreter_set_process_mark

    ! ------------------------------------------------------------------
    ! The commands: their arguments read, then the procedures above
    ! ------------------------------------------------------------------

    !> The count of the prefix argument.  Not a port.
    integer function prefix_count()
        prefix_count = int(prefix_numeric_value(current_prefix_argument))
    end function prefix_count

    !> Cycle backwards through input history, saving input.
    subroutine execute_command_interpreter_previous_input(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call command_interpreter_previous_input(prefix_count(), status)
    end subroutine execute_command_interpreter_previous_input

    !> Cycle forwards through input history.
    subroutine execute_command_interpreter_next_input(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call command_interpreter_next_input(prefix_count(), status)
    end subroutine execute_command_interpreter_next_input

    !> Search backwards through input history for match for REGULAR_EXPRESSION.
    subroutine execute_command_interpreter_previous_matching_input(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regular_expression
        integer :: n

        call command_interpreter_regular_expression_argument("Previous input matching (regexp): ", regular_expression, n, status)
        if (status%ok()) call command_interpreter_previous_matching_input(regular_expression, n, status)
    end subroutine execute_command_interpreter_previous_matching_input

    !> Search forwards through input history for match for REGULAR_EXPRESSION.
    subroutine execute_command_interpreter_next_matching_input(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regular_expression
        integer :: n

        call command_interpreter_regular_expression_argument("Next input matching (regexp): ", regular_expression, n, status)
        if (status%ok()) call command_interpreter_next_matching_input(regular_expression, n, status)
    end subroutine execute_command_interpreter_next_matching_input

    !> Search backwards through input history for match for current input.
    subroutine execute_command_interpreter_previous_matching_input_from_input(status)
        type(status_type), intent(out) :: status

        call command_interpreter_previous_matching_input_from_input(prefix_count(), status)
    end subroutine execute_command_interpreter_previous_matching_input_from_input

    !> Search forwards through input history for match for current input.
    subroutine execute_command_interpreter_next_matching_input_from_input(status)
        type(status_type), intent(out) :: status

        call command_interpreter_next_matching_input_from_input(prefix_count(), status)
    end subroutine execute_command_interpreter_next_matching_input_from_input

    !> Search backward through buffer for input fields that match REGULAR_EXPRESSION.
    subroutine execute_command_interpreter_backward_matching_input(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regular_expression
        integer :: n

        call command_interpreter_regular_expression_argument("Backward input matching (regexp): ", regular_expression, n, status)
        if (status%ok()) call command_interpreter_backward_matching_input(regular_expression, n, status)
    end subroutine execute_command_interpreter_backward_matching_input

    !> Search forward through buffer for input fields that match REGULAR_EXPRESSION.
    subroutine execute_command_interpreter_forward_matching_input(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regular_expression
        integer :: n

        call command_interpreter_regular_expression_argument("Forward input matching (regexp): ", regular_expression, n, status)
        if (status%ok()) call command_interpreter_forward_matching_input(regular_expression, n, status)
    end subroutine execute_command_interpreter_forward_matching_input

    !> Move to end of Nth next prompt in the buffer.
    subroutine execute_command_interpreter_next_prompt(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call command_interpreter_next_prompt(prefix_count())
    end subroutine execute_command_interpreter_next_prompt

    !> Move to end of Nth previous prompt in the buffer.
    subroutine execute_command_interpreter_previous_prompt(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call command_interpreter_previous_prompt(prefix_count())
    end subroutine execute_command_interpreter_previous_prompt

    !> Go to the beginning of line, then skip past the prompt, if any.
    subroutine execute_command_interpreter_beginning_of_line(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call command_interpreter_beginning_of_line(current_prefix_argument%kind /= PREFIX_NONE)
    end subroutine execute_command_interpreter_beginning_of_line

    !> Cut current line, ignoring read-only and field properties.
    subroutine execute_command_interpreter_cut_whole_line(status)
        type(status_type), intent(out) :: status

        call command_interpreter_cut_whole_line(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_command_interpreter_cut_whole_line

    !> Like `cut_region', but ignore read-only properties, if safe.
    subroutine execute_command_interpreter_cut_region(status)
        type(status_type), intent(out) :: status
        integer(int64) :: b, e

        call interactive_region(b, e, status)
        if (.not. status%ok()) return
        call command_interpreter_cut_region(b, e, status)
    end subroutine execute_command_interpreter_cut_region

    !> Append output from interpreter since last input to FILENAME.
    subroutine execute_command_interpreter_append_output_to_file(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file

        call read_file_name("Append output to file: ", file, status)
        if (status%ok()) call command_interpreter_append_output_to_file(file, status)
    end subroutine execute_command_interpreter_append_output_to_file

    ! ------------------------------------------------------------------
    ! History expansion
    ! ------------------------------------------------------------------

    !> Return the string ARGUMENT places along the input ring.
    !> Moves relative to `command_interpreter_input_ring_index'.  Port of
    !> comint.el:comint-previous-input-string.
    function command_interpreter_previous_input_string(argument) result(item)
        integer, intent(in) :: argument
        character(len=:), allocatable :: item
        type(status_type) :: st
        integer :: k

        item = ""
        k = entry_of(current)
        if (k == 0) return
        if (.not. command_interpreter_buffers(k)%has_ring) return

        if (command_interpreter_buffers(k)%input_ring_index >= 0) then
            call command_interpreter_buffers(k)%input_ring%reference(argument + command_interpreter_buffers(k)%input_ring_index, &
                                                                     item, st)
        else
            call command_interpreter_buffers(k)%input_ring%reference(argument, item, st)
        end if
    end function command_interpreter_previous_input_string

    ! The length of the input ring (0 without one).  Not a port.
    integer function ring_length()
        integer :: k

        ring_length = 0
        k = entry_of(current)
        if (k == 0) return
        if (command_interpreter_buffers(k)%has_ring) ring_length = command_interpreter_buffers(k)%input_ring%count()
    end function ring_length

    ! (setq comint-input-ring-index INDEX), -1 for nil.  Not a port.
    subroutine set_input_ring_index(index)
        integer, intent(in) :: index
        integer :: k

        call make_local_entry(k)
        command_interpreter_buffers(k)%input_ring_index = index
    end subroutine set_input_ring_index

    ! comint-input-ring-index, -1 for nil.  Not a port.
    integer function input_ring_index()
        integer :: k

        input_ring_index = -1
        k = entry_of(current)
        if (k > 0) input_ring_index = command_interpreter_buffers(k)%input_ring_index
    end function input_ring_index

    !> Return true if the number of quotes between BEGINNING and END is odd.
    !> Quotes are single and double.  Port of
    !> comint.el:comint-within-quotes.
    logical function command_interpreter_within_quotes(beginning, end)
        integer(int64), intent(in) :: beginning, end
        integer :: countsq, countdq

        countsq = command_interpreter_how_many_region("\(^\|[^\]\)'", beginning, end)
        countdq = command_interpreter_how_many_region("\(^\|[^\]\)""", beginning, end)
        command_interpreter_within_quotes = modulo(countsq, 2) == 1 .or. modulo(countdq, 2) == 1
    end function command_interpreter_within_quotes

    !> Return number of matches for REGULAR_EXPRESSION from BEGINNING to END.  Port of
    !> comint.el:comint-how-many-region.
    integer function command_interpreter_how_many_region(regular_expression, beginning, end) result(count)
        use search_mod, only: regular_expression_search_forward, match_data, set_match_data
        character(len=*), intent(in) :: regular_expression
        integer(int64), intent(in) :: beginning, end
        integer(int64), allocatable :: saved(:)
        type(status_type) :: st
        integer(int64) :: original_point, position

        count = 0
        original_point = point()
        saved = match_data()
        call goto_character(beginning)

        do
            call regular_expression_search_forward(regular_expression, position, st, bound=end, noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. position == 0) exit
            count = count + 1
        end do

        call set_match_data(saved)
        call goto_character(original_point)
    end function command_interpreter_how_many_region

    ! From STRING, return the args depending on the range specified in the
    ! text from BEGIN to END.  If BEGIN is 0, assume all args.  Ignore
    ! leading `:'.  Range can be x-y, x-, -y, where x/y can be [0-9], *,
    ! ^, $.  Port of comint.el:comint-args.
    function command_interpreter_range_arguments(string, begin, end) result(arguments)
        use search_mod, only: match_data, set_match_data
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: begin, end
        character(len=:), allocatable :: arguments
        character(len=:), allocatable :: range
        integer(int64), allocatable :: saved(:)
        type(status_type) :: st
        integer :: nth, mth, d
        logical :: has_nth, has_mth
        integer(int64) :: b

        saved = match_data()

        if (begin == 0) then
            arguments = command_interpreter_arguments(string, 0)
        else
            b = begin
            if (character_after_is(begin, ":")) b = begin + 1
            call buffer_substring(b, end, range, st)
            has_nth = .true.

            if (smatch("^[*^]", range)) then
                nth = 1
            else if (smatch("^-", range)) then
                nth = 0
            else if (range == "$") then
                has_nth = .false.
                nth = 0
            else
                nth = string_to_integer(range)
            end if

            has_mth = .true.

            if (smatch("[-*$]$", range)) then
                has_mth = .false.
                mth = 0
            else if (index(range, "-") > 0) then
                d = index(range, "-")
                mth = string_to_integer(range(d + 1:))
            else
                has_mth = has_nth
                mth = nth
            end if

            if (has_nth .and. has_mth) then
                arguments = command_interpreter_arguments(string, nth, mth)
            else if (has_nth) then
                arguments = command_interpreter_arguments(string, nth=nth)
            else if (has_mth) then
                arguments = command_interpreter_arguments(string, mth=mth)
            else
                arguments = command_interpreter_arguments(string)
            end if
        end if

        call set_match_data(saved)

    contains

        ! (string-to-number S): the leading integer, 0 if none.
        integer function string_to_integer(s)
            character(len=*), intent(in) :: s
            integer :: j, ios

            j = 0

            do while (j < len(s))
                if (verify(s(j + 1:j + 1), "0123456789") /= 0) exit
                j = j + 1
            end do

            string_to_integer = 0

            if (j > 0) then
                read (s(1:j), *, iostat=ios) string_to_integer
                if (ios /= 0) string_to_integer = 0
            end if
        end function string_to_integer
    end function command_interpreter_range_arguments

    ! Whether the character at POSITION is C.  Not a port.
    logical function character_after_is(position, c)
        integer(int64), intent(in) :: position
        character(len=1), intent(in) :: c
        character(len=:), allocatable :: s
        type(status_type) :: st

        character_after_is = .false.
        if (position < point_minimum() .or. position >= point_maximum()) return
        call buffer_substring(position, position + 1, s, st)
        character_after_is = s == c
    end function character_after_is

    ! Whether REGULAR_EXPRESSION matches STRING (without changing the match data).
    ! Not a port.
    logical function smatch(regular_expression, string)
        character(len=*), intent(in) :: regular_expression, string
        type(status_type) :: st

        smatch = string_match(regular_expression, string, st, inhibit_modify=.true.) > 0
    end function smatch

    !> Expand input command history references before point.
    !> Expansion is dependent on the value of `command_interpreter_input_autoexpand'.
    !>
    !> This function depends on the buffer's idea of the input history,
    !> which may not match the command interpreter's idea, assuming it has
    !> one.
    !>
    !> Assumes history syntax is like typical Un*x shells'.  However,
    !> since Fortmacs cannot know the interpreter's idea of input line
    !> numbers, assuming it has one, it cannot expand absolute input line
    !> number references.
    !>
    !> If the optional argument SILENT is true, never complain even if
    !> history reference seems erroneous.
    !>
    !> If the optional argument START is present, that specifies the start
    !> of the text to scan for history references, rather than the logical
    !> beginning of line.
    !>
    !> See `command_interpreter_magic_space' and
    !> `command_interpreter_replace_by_expanded_history_before_point'.
    !>
    !> Returns true if successful.  Port of
    !> comint.el:comint-replace-by-expanded-history.
    recursive logical function command_interpreter_replace_by_expanded_history(status, silent, start) result(done)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: silent
        integer(int64), intent(in), optional :: start
        integer(int64) :: tick

        status = status_type()
        done = .false.
        if (.not. has_history_references(silent, start)) return
        ! Looks like there might be history references in the command.
        tick = buffers(current)%text%modiff
        call command_interpreter_replace_by_expanded_history_before_point(silent, status, start)
        done = tick /= buffers(current)%text%modiff
    end function command_interpreter_replace_by_expanded_history

    ! Whether there are input command history references at point to
    ! expand (comint-c-a-p-replace-by-expanded-history returns the
    ! function doing the expansion then).  Port of
    ! comint.el:comint-c-a-p-replace-by-expanded-history, in part.
    recursive logical function has_history_references(silent, start) result(applies)
        logical, intent(in), optional :: silent
        integer(int64), intent(in), optional :: start
        type(value_type) :: field
        type(status_type) :: st
        integer(int64) :: original_point
        integer :: kind

        applies = .false.
        if (command_interpreter_input_autoexpand%is_off()) return

        if (command_interpreter_use_prompt_regular_expression%value) then
            ! Use comint-prompt-regexp
            original_point = point()
            call beginning_of_line()
            applies = looking_at(command_interpreter_prompt_regular_expression%value//"!\|\^", st)
            call goto_character(original_point)
        else
            ! Use input fields.  User input that hasn't been entered yet,
            ! at the end of the buffer, has a nil `field' property.
            field = get_character_property(point(), value_type(name="field"))
            if (.not. allocated(field%a)) applies = smatch("!\|^\^", field_string_no_properties(point()))
        end if

        if (.not. applies) return
        call command_interpreter_replace_by_expanded_history_before_point(silent, st, start, dry_run=.true., kind=kind)
        applies = kind /= DRY_RUN_NONE
    end function has_history_references

    !> Expand directory stack reference before point.
    !> See `command_interpreter_replace_by_expanded_history'.
    !>
    !> If the optional argument START is present, that specifies the start
    !> of the text to scan for history references, rather than the logical
    !> beginning of line.
    !>
    !> If DRY_RUN is true, return in KIND what the first reference would
    !> do before performing any actual side-effect (upstream returns it to
    !> DRY-RUN).  Port of
    !> comint.el:comint-replace-by-expanded-history-before-point.
    recursive subroutine command_interpreter_replace_by_expanded_history_before_point(silent, status, start, dry_run, kind)
        logical, intent(in), optional :: silent
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: start
        logical, intent(in), optional :: dry_run
        integer, intent(out), optional :: kind
        character(len=:), allocatable :: exp, pref, old, new, input
        type(status_type) :: st
        type(handle_type) :: original_point
        integer(int64) :: toend, beginning, moved, mb1, me1, mb2, me2, mb4, me4, position, mb0, me0
        integer :: number, horizontal_position
        logical :: quiet, dry, matched, found
        character(len=32) :: item_text

        status = status_type()
        quiet = .false.
        if (present(silent)) quiet = silent
        dry = .false.
        if (present(dry_run)) dry = dry_run
        if (present(kind)) kind = DRY_RUN_NONE
        call copy_marker_at(point(), original_point, .false., st)
        toend = line_end_position() - point()

        if (present(start)) then
            beginning = start
        else
            beginning = command_interpreter_line_beginning_position()
        end if

        call goto_character(beginning)

        do
            moved = skip_characters_forward("^!^", st, line_end_position() - toend)
            if (point() >= line_end_position() - toend) exit
            ! This seems a bit complex.  We look for references such as !!,
            ! !-num, !foo, !?foo, !{bar}, !?{bar}, ^oh, ^my^, ^god^it,
            ! ^never^ends^.  If that wasn't enough, the plings can be
            ! suffixed with argument range specifiers.  Argument ranges
            ! are complex too, so we hive off the input line, referenced
            ! with plings, with the range string to `comint-args'.
            call set_input_ring_index(-1)

            if (character_after_is(point() - 1, "\") .or. command_interpreter_within_quotes(beginning, point())) then
                ! The history is quoted, or we're in quotes.
                call goto_character(point() + 1)
            else if (looking_at("![0-9]+\($\|[^-]\)", st)) then
                ! We cannot know the interpreter's idea of input line
                ! numbers.
                if (dry) then
                    if (present(kind)) kind = DRY_RUN_MESSAGE
                    exit
                end if

                call goto_character(match_end(0, st))
                call message("Absolute reference cannot be expanded")
            else if (looking_at("!-\([0-9]+\):?\([0-9^$*-]+\)?", st)) then
                ! Just a number of args from `number' lines backward.
                if (dry) then
                    if (present(kind)) kind = DRY_RUN_HISTORY
                    exit
                end if

                number = int(number_in(match_beginning(1, st), match_end(1, st))) - 1

                if (number <= ring_length()) then
                    mb2 = match_beginning(2, st)
                    me2 = match_end(2, st)
                    call replace_match(command_interpreter_range_arguments(command_interpreter_previous_input_string(number), mb2, &
                                                                           me2), st, &
                                       fixedcase=.true., literal=.true.)
                    call set_input_ring_index(number)
                    write (item_text, '("History item: ", i0)') number + 1
                    call message(trim(item_text))
                else
                    call goto_character(match_end(0, st))
                    call message("Relative reference exceeds input history size")
                end if
            else if (looking_at_either("!!?:?\([0-9^$*-]+\)", "!!")) then
                ! Just a number of args from the previous input line.
                if (dry) then
                    if (present(kind)) kind = DRY_RUN_EXPAND
                    exit
                end if

                call replace_match(command_interpreter_range_arguments(command_interpreter_previous_input_string(0), &
                                                                       match_beginning(1, st), &
                                                                       match_end(1, st)), st, fixedcase=.true., literal=.true.)
                call message("History item: previous")
            else if (looking_at("!\??\({\(.+\)}\|\(\sw+\)\):?\([0-9^$*-]+\)?", st)) then
                ! Most recent input starting with or containing (possibly
                ! protected) string, maybe just a number of args.  Phew.
                if (dry) then
                    if (present(kind)) kind = DRY_RUN_EXPAND
                    exit
                end if

                mb1 = match_beginning(1, st)
                me1 = match_end(1, st)
                mb2 = match_beginning(2, st)
                me2 = match_end(2, st)
                mb4 = match_beginning(4, st)
                me4 = match_end(4, st)
                mb0 = match_beginning(0, st)
                me0 = match_end(0, st)

                if (mb2 > 0) then
                    call buffer_substring(mb2, me2, exp, st)
                else
                    call buffer_substring(mb1, me1, exp, st)
                end if

                if (looking_at("!\?", st, inhibit_modify=.true.)) then
                    pref = ""
                else
                    pref = "^"
                end if

                call command_interpreter_previous_matching_input_string_position(pref//regular_expression_quote(exp), 1, &
                                                                                 horizontal_position, st)
                ! (restore the match data the search changed)
                matched = looking_at("!\??\({\(.+\)}\|\(\sw+\)\):?\([0-9^$*-]+\)?", st)

                if (horizontal_position < 0) then
                    call goto_character(me0)

                    if (.not. quiet) then
                        call message("Not found")
                        call ding()
                    end if
                else
                    call set_input_ring_index(horizontal_position)
                    call command_interpreter_input_ring_reference(horizontal_position, input)
                    call replace_match(command_interpreter_range_arguments(input, mb4, me4), st, fixedcase=.true., literal=.true.)
                    write (item_text, '("History item: ", i0)') horizontal_position + 1
                    call message(trim(item_text))
                end if
            else if (looking_at("\^\([^^]+\)\^?\([^^]*\)\^?", st)) then
                ! Quick substitution on the previous input line.
                if (dry) then
                    if (present(kind)) kind = DRY_RUN_EXPAND
                    exit
                end if

                call buffer_substring(match_beginning(1, st), match_end(1, st), old, st)
                call buffer_substring(match_beginning(2, st), match_end(2, st), new, st)
                mb0 = match_beginning(0, st)
                call replace_match(command_interpreter_previous_input_string(0), st, fixedcase=.true., literal=.true.)
                position = point()
                call goto_character(mb0)
                call search_forward(old, found, st, bound=position)

                if (.not. found) then
                    if (.not. quiet) then
                        call set_error(status, USER_ERROR_CODE, "Not found")
                        exit
                    end if
                else
                    call replace_match(new, st, fixedcase=.true., literal=.true.)
                    call message("History item: substituted")
                end if
            else
                call goto_character(point() + 1)
            end if
        end do

        call goto_character(marked_position(original_point))
        call destroy_marker(original_point, st)

    contains

        ! The number in the buffer from B to E.
        integer(int64) function number_in(b, e)
            integer(int64), intent(in) :: b, e
            character(len=:), allocatable :: s
            type(status_type) :: st2
            integer :: ios

            call buffer_substring(b, e, s, st2)
            number_in = 0
            read (s, *, iostat=ios) number_in
            if (ios /= 0) number_in = 0
        end function number_in
    end subroutine command_interpreter_replace_by_expanded_history_before_point

    ! (or (looking-at RE1) (looking-at RE2)), the match data of the one
    ! that matched.  Not a port.
    logical function looking_at_either(re1, re2)
        character(len=*), intent(in) :: re1, re2
        type(status_type) :: st

        looking_at_either = looking_at(re1, st)
        if (.not. looking_at_either) looking_at_either = looking_at(re2, st)
    end function looking_at_either

    ! (search-forward STRING BOUND t): FOUND says whether it matched.
    ! Not a port.
    subroutine search_forward(string, found, status, bound)
        use search_mod, only: search_forward_position => search_forward
        character(len=*), intent(in) :: string
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64), intent(in) :: bound
        integer(int64) :: position

        call search_forward_position(string, position, status, bound=bound, noerror=SEARCH_NOERROR)
        found = status%ok() .and. position > 0
    end subroutine search_forward

    !> Expand input history references before point and insert ARGUMENT
    !> spaces.  A useful command to bind to SPC.  See
    !> `command_interpreter_replace_by_expanded_history'.  Port of
    !> comint.el:comint-magic-space.
    recursive subroutine command_interpreter_magic_space(argument, status)
        integer(int64), intent(in) :: argument
        type(status_type), intent(out) :: status
        logical :: done

        done = command_interpreter_replace_by_expanded_history(status)
        if (.not. status%ok()) return
        call self_insert_command(argument, int(iachar(" "), int64), status)
    end subroutine command_interpreter_magic_space

    recursive subroutine execute_command_interpreter_magic_space(status)
        type(status_type), intent(out) :: status

        call command_interpreter_magic_space(prefix_numeric_value(current_prefix_argument), status)
    end subroutine execute_command_interpreter_magic_space

    recursive subroutine execute_command_interpreter_replace_by_expanded_history(status)
        type(status_type), intent(out) :: status
        logical :: done

        done = command_interpreter_replace_by_expanded_history(status)
    end subroutine execute_command_interpreter_replace_by_expanded_history

    !> Insert the INDEXth argument from the previous Comint command-line
    !> at point.  Spaces are added at beginning and/or end of the inserted
    !> string if necessary to ensure that it's separated from adjacent
    !> arguments.  Interactively, if no prefix argument is given, the last
    !> argument is inserted.  Repeated interactive invocations will cycle
    !> through the same argument from progressively earlier commands
    !> (using the value of INDEX specified with the first command).
    !> Values of INDEX < 0 count from the end, so INDEX = -1 is the last
    !> argument.  This command is like "M-." in Bash and zsh.  HAS_INDEX
    !> .false. means no INDEX.  Port of
    !> comint.el:comint-insert-previous-argument.
    recursive subroutine command_interpreter_insert_previous_argument(index, has_index, status)
        integer, intent(in) :: index
        logical, intent(in) :: has_index
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: input_string
        type(status_type) :: st
        integer :: k, argument_index
        logical :: has

        status = status_type()
        call make_local_entry(k)
        argument_index = index
        has = has_index

        if (associated(last_command, this_command)) then
            ! Delete last input inserted by this command.
            call delete_region(marked_position(command_interpreter_buffers(k)%insert_previous_argument_last_start_position), &
                               point(), st)
            argument_index = command_interpreter_buffers(k)%insert_previous_argument_last_index
            has = command_interpreter_buffers(k)%has_insert_previous_argument_last_index
        else
            ! This is a non-repeat invocation, so initialize state.
            if (has .and. command_interpreter_insert_previous_argument_from_end%value) argument_index = -argument_index
            command_interpreter_buffers(k)%input_ring_index = -1
            command_interpreter_buffers(k)%insert_previous_argument_last_index = argument_index
            command_interpreter_buffers(k)%has_insert_previous_argument_last_index = has

            ! First usage; initialize to a marker
            if (command_interpreter_buffers(k)%insert_previous_argument_last_start_position%index == 0) then
                call make_marker(command_interpreter_buffers(k)%insert_previous_argument_last_start_position)
            end if
        end if

        ! Make sure we're not in the prompt, and add a beginning space if
        ! necessary.
        if (point() <= command_interpreter_line_beginning_position()) then
            call command_interpreter_beginning_of_line(.false.)
        else
            call just_one_space(1_int64, st)
        end if

        ! Remember the beginning of what we insert, so we can delete it if
        ! the command is repeated.
        call set_marker(command_interpreter_buffers(k)%insert_previous_argument_last_start_position, point(), st)
        ! Insert the argument.
        input_string = command_interpreter_previous_input_string(0)

        if (has) then
            call insert(command_interpreter_arguments(input_string, argument_index, argument_index), st)
        else
            call insert(command_interpreter_arguments(input_string), st)
        end if

        ! Make next invocation return arg from previous input
        call make_local_entry(k)
        command_interpreter_buffers(k)%input_ring_index = max(command_interpreter_buffers(k)%input_ring_index, 0) + 1
        ! Add a terminating space if necessary.
        if (.not. is_end_of_line()) call just_one_space(1_int64, st)
    end subroutine command_interpreter_insert_previous_argument

    recursive subroutine execute_command_interpreter_insert_previous_argument(status)
        type(status_type), intent(out) :: status

        if (current_prefix_argument%kind == PREFIX_NONE) then
            call command_interpreter_insert_previous_argument(0, .false., status)
        else
            call command_interpreter_insert_previous_argument(int(prefix_numeric_value(current_prefix_argument)), .true., status)
        end if
    end subroutine execute_command_interpreter_insert_previous_argument

    ! ------------------------------------------------------------------
    ! Isearch in comint input history
    ! ------------------------------------------------------------------

    !> Search for a string backward in input history using Isearch.  Port of
    !> comint.el:comint-history-isearch-backward.
    recursive subroutine command_interpreter_history_incremental_search_backward(status)
        type(status_type), intent(out) :: status

        status = status_type()
        command_interpreter_force_history_incremental_search = .true.
        call incremental_search_mode(.false., .false.)
    end subroutine command_interpreter_history_incremental_search_backward

    !> Search for a regular expression backward in input history using
    !> Isearch.  Port of comint.el:comint-history-isearch-backward-regexp.
    recursive subroutine command_interpreter_history_search_backward_regular_expression(status)
        type(status_type), intent(out) :: status

        status = status_type()
        command_interpreter_force_history_incremental_search = .true.
        call incremental_search_mode(.false., .true.)
    end subroutine command_interpreter_history_search_backward_regular_expression

    ! The beginning of the line of the prompt: (save-excursion (goto-char
    ! (comint-line-beginning-position)) (forward-line 0) (point)).  Not a port.
    integer(int64) function prompt_line_start()
        integer(int64) :: original_point

        original_point = point()
        call goto_character(command_interpreter_line_beginning_position())
        call beginning_of_line()
        prompt_line_start = point()
        call goto_character(original_point)
    end function prompt_line_start

    !> Set up a comint for using Isearch to search the input history.
    !> Intended to be added to `incremental_search_mode_hook' in `command_interpreter_mode'.  Port of
    !> comint.el:comint-history-isearch-setup (no lazy count).
    recursive subroutine command_interpreter_history_incremental_search_setup(status)
        type(status_type), intent(out) :: status

        status = status_type()
        ! Prompt is not empty like in Async Shell Command buffers or in
        ! finished shell buffers
        if (prompt_line_start() == command_interpreter_line_beginning_position()) return
        if (.not. (command_interpreter_force_history_incremental_search .or. command_interpreter_history_incremental_search%value &
                   .or. &
                   (command_interpreter_history_incremental_search%choice &
                    == COMMAND_INTERPRETER_HISTORY_INCREMENTAL_SEARCH_DO_WHAT_I_MEAN &
                    .and. is_command_interpreter_after_pmark()))) return
        incremental_search_message_prefix_add = "history "
        call incremental_search_search_function_function%set_local(command_interpreter_history_incremental_search_search)
        call incremental_search_message_function%set_local(command_interpreter_history_incremental_search_message)
        call incremental_search_wrap_function%set_local(command_interpreter_history_incremental_search_wrap)
        call incremental_search_push_state_function%set_local(command_interpreter_history_incremental_search_push_state)
        call incremental_search_mode_end_hook%prepend(command_interpreter_history_incremental_search_end, local=.true.)
    end subroutine command_interpreter_history_incremental_search_setup

    !> Clean up the comint after terminating Isearch in comint.  Port of
    !> comint.el:comint-history-isearch-end.
    recursive subroutine command_interpreter_history_incremental_search_end(status)
        type(status_type), intent(out) :: status
        type(status_type) :: st
        integer :: k

        status = status_type()
        call make_local_entry(k)

        if (is_overlay(command_interpreter_buffers(k)%history_incremental_search_message_overlay)) then
            call delete_overlay(command_interpreter_buffers(k)%history_incremental_search_message_overlay)
        end if

        if (allocated(incremental_search_message_prefix_add)) deallocate (incremental_search_message_prefix_add)
        call incremental_search_search_function_function%clear_local()
        call incremental_search_message_function%clear_local()
        call incremental_search_wrap_function%clear_local()
        call incremental_search_push_state_function%clear_local()
        ! Force isearch to not change mark.
        call set_incremental_search_original_point(point())
        call incremental_search_mode_end_hook%remove(command_interpreter_history_incremental_search_end, local=.true.)
        ! (isearch-suspended is not ported: it is always nil here.)
        command_interpreter_force_history_incremental_search = .false.
    end subroutine command_interpreter_history_incremental_search_end

    !> Put input history item of the absolute history position POSITION (-1
    !> for none).  Port of comint.el:comint-goto-input.
    recursive subroutine command_interpreter_goto_input(position, status)
        integer, intent(in) :: position
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: old, item
        integer :: k

        status = status_type()
        call make_local_entry(k)

        ! If leaving the edit line, save partial unfinished input.
        if (command_interpreter_buffers(k)%input_ring_index < 0) then
            call execute_get_old_input(old, status)
            if (.not. status%ok()) return
            call make_local_entry(k)
            command_interpreter_buffers(k)%stored_incomplete_input = old
        end if

        command_interpreter_buffers(k)%input_ring_index = position
        call command_interpreter_delete_input(status)
        if (.not. status%ok()) return
        call make_local_entry(k)

        if (position >= 0 .and. ring_length() > 0) then
            call command_interpreter_input_ring_reference(position, item)
            call insert(item, status)
        else if (allocated(command_interpreter_buffers(k)%stored_incomplete_input)) then
            ! Restore partial unfinished input.
            if (len(command_interpreter_buffers(k)%stored_incomplete_input) > 0) then
                call insert(command_interpreter_buffers(k)%stored_incomplete_input, status)
            end if
        end if
    end subroutine command_interpreter_goto_input

    !> Return the proper search function, for Isearch in input history.
    !> Port of comint.el:comint-history-isearch-search.
    recursive subroutine command_interpreter_history_incremental_search_search(fn)
        procedure(incremental_search_search_procedure), pointer, intent(out) :: fn

        fn => history_search
    end subroutine command_interpreter_history_incremental_search_search

    ! The search function comint-history-isearch-search returns.
    ! Port of comint.el:comint-history-isearch-search (the function it returns).
    recursive subroutine history_search(string, bound, noerror, position, status)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: bound
        logical, intent(in) :: noerror
        integer(int64), intent(out) :: position
        type(status_type), intent(out) :: status
        procedure(incremental_search_search_procedure), pointer :: search_function
        type(handle_type) :: process
        type(status_type) :: st
        logical :: found

        ! Use standard functions to search within comint text
        search_function => incremental_search_search_function_default()
        status = status_type()

        ! Avoid lazy-highlighting matches in the comint prompt and in the
        ! output when searching forward.  Lazy-highlight calls this lambda
        ! with the bound arg, so skip the prompt and the output.
        if (bound > 0 .and. incremental_search_forward .and. .not. is_command_interpreter_after_pmark()) then
            call current_process(process, st)
            if (st%ok()) call goto_character(pmark_position(process))
        end if

        ! 1. First try searching in the initial comint text
        if (incremental_search_forward) then
            call search_function(string, bound, noerror, position, status)
        else
            call search_function(string, command_interpreter_line_beginning_position(), noerror, position, status)
        end if

        if (.not. status%ok() .or. position > 0) return
        ! 2. If the above search fails, start putting next/prev history
        ! elements in the comint successively, and search the string in
        ! them.  Do this only when bound is nil (i.e. not while
        ! lazy-highlighting search strings in the current comint text).
        if (bound > 0) return
        found = .false.

        do while (.not. found)
            if (incremental_search_forward) then
                ! Signal an error here explicitly, because
                ! `comint-next-input' doesn't signal an error.
                if (input_ring_index() < 0) return
                call command_interpreter_next_input(1, st)
                if (.not. st%ok()) return
                call goto_character(command_interpreter_line_beginning_position())
            else
                ! Signal an error here explicitly, because
                ! `comint-previous-input' doesn't signal an error.
                if (input_ring_index() == ring_length() - 1) return
                call command_interpreter_previous_input(1, st)
                if (.not. st%ok()) return
                call goto_character(point_maximum())
            end if

            call set_incremental_search_barrier(point())
            call set_incremental_search_original_point(point())

            ! After putting the next/prev history element, search the
            ! string in them again, until comint-next-input or
            ! comint-previous-input raises an error at the beginning/end
            ! of history.
            if (incremental_search_forward) then
                call search_function(string, 0_int64, noerror, position, st)
            else
                ! For backward search, don't search in the comint prompt
                call search_function(string, command_interpreter_line_beginning_position(), noerror, position, st)
            end if

            if (.not. st%ok()) return
            found = position > 0
        end do

        ! Return point of the new search result
        position = point()
    end subroutine history_search

    !> Display the input history search prompt.
    !> If there are no search errors, this function displays an overlay
    !> with the Isearch prompt which replaces the original comint prompt.
    !> Otherwise, it displays the standard Isearch message returned from
    !> the function `incremental_search_message'.  Port of
    !> comint.el:comint-history-isearch-message.
    recursive subroutine command_interpreter_history_incremental_search_message(c_q_hack, ellipsis)
        logical, intent(in) :: c_q_hack, ellipsis
        type(status_type) :: st
        character(len=32) :: item_text
        integer :: k

        if (.not. (incremental_search_success .and. .not. allocated(incremental_search_error))) then
            ! Use standard function `isearch-message' when not in comint
            ! prompt, or search fails, or has an error (like incomplete
            ! regexp).  This function displays isearch message in the echo
            ! area, so it's possible to see what is wrong in the search
            ! string.
            call incremental_search_message_display(c_q_hack, ellipsis)
            return
        end if

        ! Otherwise, put the overlay with the standard isearch prompt over
        ! the initial comint prompt.
        call make_local_entry(k)

        if (is_overlay(command_interpreter_buffers(k)%history_incremental_search_message_overlay)) then
            call move_overlay(command_interpreter_buffers(k)%history_incremental_search_message_overlay, prompt_line_start(), &
                              command_interpreter_line_beginning_position(), st)
        else
            call make_overlay(command_interpreter_buffers(k)%history_incremental_search_message_overlay, prompt_line_start(), &
                              command_interpreter_line_beginning_position(), st)
            call overlay_put(command_interpreter_buffers(k)%history_incremental_search_message_overlay, "evaporate", &
                             value_type(.true.), st)
        end if

        call overlay_put(command_interpreter_buffers(k)%history_incremental_search_message_overlay, "display", &
                         value_type(text=incremental_search_message_prefix(incremental_search_nonincremental, ellipsis)), st)

        if (command_interpreter_buffers(k)%input_ring_index >= 0 .and. .not. ellipsis) then
            ! Display the current history index.
            write (item_text, '("History item: ", i0)') command_interpreter_buffers(k)%input_ring_index + 1
            call message(trim(item_text))
        else
            ! Or clear a previous isearch message.
            call message("")
        end if
    end subroutine command_interpreter_history_incremental_search_message

    !> Wrap the input history search when search fails.
    !> Move point to the first history element for a forward search, or
    !> to the last history element for a backward search.  Port of
    !> comint.el:comint-history-isearch-wrap.
    recursive subroutine command_interpreter_history_incremental_search_wrap()
        type(status_type) :: st

        ! When `comint-history-isearch-search' fails on reaching the
        ! beginning/end of the history, wrap the search to the first/last
        ! input history element.
        if (incremental_search_forward) then
            call command_interpreter_goto_input(ring_length() - 1, st)
            call goto_character(command_interpreter_line_beginning_position())
        else
            call command_interpreter_goto_input(-1, st)
            call goto_character(point_maximum())
        end if
    end subroutine command_interpreter_history_incremental_search_wrap

    !> Save a function restoring the state of input history search.
    !> Save `command_interpreter_input_ring_index' to the additional state parameter in
    !> the search status stack.  Port of
    !> comint.el:comint-history-isearch-push-state.
    recursive subroutine command_interpreter_history_incremental_search_push_state(data, pop)
        character(len=:), allocatable, intent(out) :: data
        procedure(incremental_search_pop_procedure), pointer, intent(out) :: pop
        character(len=20) :: digits

        write (digits, "(i0)") input_ring_index()
        data = trim(digits)
        pop => command_interpreter_history_incremental_search_pop_state
    end subroutine command_interpreter_history_incremental_search_push_state

    !> Restore the input history search state.
    !> Go to the history element by the absolute history position
    !> HISTORY_POSITION.  Port of comint.el:comint-history-isearch-pop-state.
    recursive subroutine command_interpreter_history_incremental_search_pop_state(history_position)
        character(len=*), intent(in) :: history_position
        type(status_type) :: st
        integer :: n, ios

        read (history_position, *, iostat=ios) n
        if (ios /= 0) n = -1
        call command_interpreter_goto_input(n, st)
    end subroutine command_interpreter_history_incremental_search_pop_state

    ! ------------------------------------------------------------------
    ! Scrolling on input, stopped subjobs
    ! ------------------------------------------------------------------

    !> Go to the end of buffer in all windows showing it.
    !> Movement occurs if point in the selected window is not after the
    !> process mark, and `this_command' is an insertion command.
    !> Insertion commands recognized are `self_insert_command',
    !> `command_interpreter_magic_space', `paste', and `hilit_paste'.  Depends on the
    !> value of `command_interpreter_scroll_to_bottom_on_input'.
    !>
    !> This function should be a pre-command hook.  Port of
    !> comint.el:comint-preinput-scroll-to-bottom.
    recursive subroutine command_interpreter_preinput_scroll_to_bottom(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: process, current
        type(handle_type), allocatable :: leaves(:)
        integer :: scroll, i

        status = status_type()
        if (command_interpreter_scroll_to_bottom_on_input%is_off()) return
        scroll = command_interpreter_scroll_to_bottom_on_input%choice
        ! (Upstream's list also has hilit-yank, which is not ported.)
        if (.not. (associated(this_command, commands%self_insert_command) .or. &
                   associated(this_command, commands%command_interpreter_magic_space) .or. associated(this_command, &
                                                                                                      commands%paste))) return
        current = current_buffer()
        process = get_buffer_process(current)
        if (process%index == 0) return
        if (point() >= pmark_position(process)) return

        if (scroll == COMMAND_INTERPRETER_SCROLL_TO_BOTTOM_ON_INPUT_THIS) then
            call goto_character(point_maximum())
        else
            call window_leaves(selected_frame%root_window, leaves)

            do i = 1, size(leaves)
                if (same_handle(windows(leaves(i)%index)%buffer, current)) then
                    call set_window_point(windows(leaves(i)%index), point_maximum(), status)
                end if
            end do

            call goto_character(point_maximum())
        end if
    end subroutine command_interpreter_preinput_scroll_to_bottom

    !> Stop the current subjob.
    !>
    !> WARNING: if there is no current subjob, you can end up suspending
    !> the top-level process running in the buffer.  If you accidentally
    !> do this, use \[command_interpreter_continue_subjob] to resume the process.
    !> (This is not a problem with most shells, since they ignore this
    !> signal.)  Port of comint.el:comint-stop-subjob.
    recursive subroutine command_interpreter_suspend_subjob(status)
        type(status_type), intent(out) :: status

        call command_interpreter_skip_input(status)
        if (.not. status%ok()) return
        call suspend_process(status, current_group=is_command_interpreter_pty())
    end subroutine command_interpreter_suspend_subjob

    !> Send CONT signal to process buffer's process group.
    !> Useful if you accidentally suspend the top-level process.  Port of
    !> comint.el:comint-continue-subjob.
    subroutine command_interpreter_continue_subjob(status)
        type(status_type), intent(out) :: status

        call continue_process(status, current_group=is_command_interpreter_pty())
    end subroutine command_interpreter_continue_subjob

    ! ------------------------------------------------------------------
    ! Passwords
    ! ------------------------------------------------------------------

    !> The default value of `command_interpreter_password_prompt_regular_expression', made from
    !> `password_word_equivalents' and `password_colon_equivalents'.
    !> Port of the value of comint.el comint-password-prompt-regexp.
    function command_interpreter_password_prompt_regular_expression_default() result(regular_expression)
        character(len=:), allocatable :: regular_expression
        character(len=MAXIMUM_MULTIBYTE_LENGTH) :: buffer
        character(len=:), allocatable :: colons
        type(status_type) :: st
        integer(int32) :: nbytes
        integer :: i

        colons = ""

        do i = 1, size(password_colon_equivalents)
            call character_string(password_colon_equivalents(i), buffer, nbytes, st)
            colons = colons//buffer(1:nbytes)
        end do

        regular_expression = "\(^ *\|"// &
                  regular_expression_optimization([character(len=24) :: "Enter", "enter", "Enter same", "enter same", "Enter the", &
                                                              "enter the", &
                                                              "Current", &
                                                              ! Ansible.  (Bug#78442)
                                                              "Vault", "SSH", "BECOME", &
                                                              "Enter Auth", "enter auth", "Old", "old", "New", "new", "login", &
                                                              "Kerberos", "CVS", "UNIX", " SMB", "LDAP", "PEM", "SUDO", &
                                                              "[sudo]", "[sudo: authenticate]", "[su: authenticate]", &
                                            "doas", "Repeat", "Bad", "Retype", "Verify"], REGULAR_EXPRESSION_OPTIMIZATION_GROUP)// &
                             ! Allow for user name to precede password equivalent
                             ! (Bug#31075).
                             " +.*\)"// &
                             "\(?:"//regular_expression_optimization(password_word_equivalents)//"\|Response\)"// &
                             "\(?:\(?:, try\)? *again\| (empty for no passphrase)\| (again)\)?"// &
                             ! "[[:alpha:]]" used to be "for", which fails to match
                             ! non-English.
                             "\(?: [[:alpha:]]+ .+\)?[[:blank:]]*"// &
                             "["//colons//"][[:space:]]*\'"// &
                             ! The ccrypt encryption dialog doesn't end with a colon, so
                             ! treat it specially.
                             "\|^Enter encryption key: (repeat) *\'"// &
                             ! Ansible.  The vault-id syntax is a guess.  (Bug#78442)
                             "\|^Vault password ([^@-][^@]*): \'"// &
                             ! Default openssh format: "user@host's password:".
                             "\|^[^@ "//achar(9)//achar(10)//"]+@[^@ "//achar(9)//achar(10)//"]+'s password: *\'"// &
                             ! openssh-8.6p1 format: "(user@host) Password:".
                             ! "(user@host) Password for user@host:"  (Bug#79424)
                             "\|^([^)@ "//achar(9)//achar(10)//"]+@[^)@ "//achar(9)//achar(10)//"]+) Password\(?: for [^)@ "// &
                             achar(9)//achar(10)//"]+@[^)@ "//achar(9)//achar(10)//"]+\)?: *\'"
    end function command_interpreter_password_prompt_regular_expression_default

    !> Read a password with PROMPT (obsolete: use `read_password').  Port of
    !> comint.el:comint-read-noecho.
    recursive subroutine command_interpreter_read_noecho(prompt, result, status)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status

        call read_password(prompt, result, status)
    end subroutine command_interpreter_read_noecho

    !> Read a string without echoing.
    !> Then send it to the process running in the current buffer.
    !> The string is sent using `command_interpreter_input_sender'.
    !> Security bug: your string can still be temporarily recovered with
    !> \[view_lossage]; `clear_this_command_keys' can fix that.  Port of
    !> comint.el:comint-send-invisible.
    recursive subroutine command_interpreter_send_invisible(status, prompt)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: prompt
        type(handle_type) :: process, wbuf
        character(len=:), allocatable :: prefix, prefix_prompt, string
        procedure(command_interpreter_password_procedure), pointer :: password_function
        integer :: k
        logical :: found

        status = status_type()
        wbuf = windows(selected_frame%selected_window%index)%buffer

        if (same_handle(wbuf, current_buffer())) then
            prefix = ""
        else
            prefix = "(In buffer "//buffers(current)%name//") "
        end if

        process = get_buffer_process(current_buffer())

        if (process%index == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "Buffer "//buffers(current)%name//" has no process")
            return
        end if

        if (present(prompt)) then
            prefix_prompt = prefix//prompt
        else
            prefix_prompt = prefix//"Non-echoed text: "
        end if

        found = .false.
        call make_local_entry(k)
        password_function => command_interpreter_password_function%get_current()

        if (associated(password_function)) then
            call password_function(prefix_prompt, string, found)
        end if

        if (.not. found) then
            call read_password(prefix_prompt, string, status)
            found = status%ok()
        end if

        if (found) then
            call command_interpreter_snapshot_last_prompt()
            call make_local_entry(k)
            call execute_input_sender(process, string, status)
        else
            call message("Warning: text will be echoed")
            status = status_type()
        end if
    end subroutine command_interpreter_send_invisible

    recursive subroutine execute_command_interpreter_send_invisible(status)
        type(status_type), intent(out) :: status

        call command_interpreter_send_invisible(status)
    end subroutine execute_command_interpreter_send_invisible

    !> Prompt in the minibuffer for password and send without echoing.
    !> Looks for a match to `command_interpreter_password_prompt_regular_expression' in order to
    !> detect the need to (prompt and) send a password.  Ignores any
    !> carriage returns (\r) in STRING.
    !>
    !> This function could be in the list
    !> `command_interpreter_output_filter_functions'.  Port of
    !> comint.el:comint-watch-for-password-prompt.
    recursive subroutine command_interpreter_watch_for_password_prompt(string, status)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text, content, prompt
        type(pending_prompt_type) :: pending
        type(handle_type) :: timer
        logical :: saved_fold, matched
        integer :: b, e

        status = status_type()
        text = string_limit_end(without_cr(string), command_interpreter_password_prompt_maximum_length)
        saved_fold = case_fold_search%value
        case_fold_search = .true.
        matched = string_match(command_interpreter_password_prompt_regular_expression, text, status) > 0
        case_fold_search = saved_fold

        if (.not. status%ok() .or. .not. matched) then
            status = status_type()
            return
        end if

        ! (string-trim (match-string 0 text) "[ \n\r\t\v\f\b\a]+" "\n+")
        content = match_string(0, text)
        b = verify(content, " "//achar(10)//achar(13)//achar(9)//achar(11)//achar(12)//achar(8)//achar(7))
        e = verify(content, achar(10), back=.true.)

        if (b == 0) then
            content = ""
        else
            content = content(b:e)
        end if

        prompt = content

        if (len(content) > 0) then
            if (content(len(content):len(content)) /= " ") prompt = content//" "
        end if

        ! Use `run-at-time' in order not to pause execution of the process
        ! filter with a minibuffer
        if (.not. allocated(pending_prompts)) allocate (pending_prompts(0))
        pending%buffer = current_buffer()
        pending%prompt = prompt
        pending_prompts = [pending_prompts, pending]
        call execute_at_time(timer, 0.0_real64, send_pending_password, status)

    contains

        ! (string-replace "\r" "" S)
        function without_cr(s) result(r)
            character(len=*), intent(in) :: s
            character(len=:), allocatable :: r
            integer :: j, n

            allocate (character(len=len(s)) :: r)
            n = 0

            do j = 1, len(s)
                if (s(j:j) == achar(13)) cycle
                n = n + 1
                r(n:n) = s(j:j)
            end do

            r = r(1:n)
        end function without_cr

        ! (string-limit S N t): the last N bytes of S, not splitting a
        ! character.
        function string_limit_end(s, nmax) result(r)
            character(len=*), intent(in) :: s
            integer, intent(in) :: nmax
            character(len=:), allocatable :: r
            integer :: b

            if (len(s) <= nmax) then
                r = s
                return
            end if

            b = len(s) - nmax + 1

            ! Skip UTF-8 continuation bytes.
            do while (b <= len(s))
                if (iand(iachar(s(b:b)), 192) /= 128) exit
                b = b + 1
            end do

            r = s(b:)
        end function string_limit_end
    end subroutine command_interpreter_watch_for_password_prompt

    ! The timer function of `command_interpreter_watch_for_password_prompt': send the
    ! oldest pending password prompt's password.  Not a port.
    recursive subroutine send_pending_password(status)
        type(status_type), intent(out) :: status
        type(pending_prompt_type) :: p
        type(handle_type) :: old, process
        type(status_type) :: st

        status = status_type()
        if (.not. allocated(pending_prompts)) return
        if (size(pending_prompts) == 0) return
        p = pending_prompts(1)
        pending_prompts = pending_prompts(2:)
        if (.not. is_buffer_live(p%buffer)) return
        old = current_buffer()
        call set_buffer(p%buffer, st)
        command_interpreter_prompt_recursion_depth = command_interpreter_prompt_recursion_depth + 1

        if (command_interpreter_prompt_recursion_depth > 10) then
            call message("Password prompt recursion too deep")
        else
            process = get_buffer_process(current_buffer())
            if (process%index /= 0) call command_interpreter_send_invisible(status, p%prompt)
        end if

        command_interpreter_prompt_recursion_depth = command_interpreter_prompt_recursion_depth - 1
        if (is_buffer_live(old)) call set_buffer(old, st)
    end subroutine send_pending_password

    ! ------------------------------------------------------------------
    ! Completion
    ! ------------------------------------------------------------------

    !> Complete a list of recent inputs entered into the current buffer.
    !> Like `minibuffer_complete_history' but completes on comint inputs.
    !> This function makes `command_interpreter_dynamic_list_input_ring' obsolete.
    !> Port of comint.el:comint-complete-input-ring.
    recursive subroutine command_interpreter_complete_input_ring(status)
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: elements(:)
        type(completion_table_type) :: table
        procedure(cir_test_procedure), pointer :: saved_test
        character(len=:), allocatable :: item
        type(status_type) :: st
        integer :: k, i, n
        logical :: res

        status = status_type()
        k = entry_of(current)
        n = 0

        if (k /= 0) then
            if (command_interpreter_buffers(k)%has_ring) n = command_interpreter_buffers(k)%input_ring%count()
        end if

        if (n == 0) then
            call set_error(status, USER_ERROR_CODE, "No history available")
            return
        end if

        allocate (elements(n))

        do i = 1, n
            call command_interpreter_buffers(k)%input_ring%reference(i - 1, item, st)
            elements(i) = value_type(text=item)
        end do

        table = completion_list_table(make_list(elements), "command_interpreter_input")
        table%unsorted = .true.
        saved_test => completion_in_region_mode_test_function
        completion_in_region_mode_test_function => completions_shown
        res = completion_in_region(command_interpreter_line_beginning_position(), point_maximum(), table)
        completion_in_region_mode_test_function => saved_test
    end subroutine command_interpreter_complete_input_ring

    ! Whether *Completions* is shown (minibuffer--completions-visible as
    ! the `completion_in_region_mode_test_function' of
    ! `command_interpreter_complete_input_ring').  Port of
    ! minibuffer.el:minibuffer--completions-visible.
    recursive logical function completions_shown()
        type(handle_type) :: window

        window = completions_visible()
        completions_shown = window%index /= 0
    end function completions_shown

    !> Return expanded DIRECTORY, with `command_interpreter_file_name_prefix' if
    !> absolute: EXPANDED.  Port of comint.el:comint-directory.
    recursive subroutine command_interpreter_directory(directory, expanded, status)
        character(len=*), intent(in) :: directory
        character(len=:), allocatable, intent(out) :: expanded
        type(status_type), intent(out) :: status

        if (is_file_name_absolute(directory)) then
            call expand_file_name(command_interpreter_file_name_prefix%value//directory, expanded, status)
        else
            call expand_file_name(directory, expanded, status)
        end if
    end subroutine command_interpreter_directory

    !> Return the word of WORD_CHARACTERS at point as WORD, FOUND false if none
    !> is found.  Word constituents are considered to be those in
    !> WORD_CHARACTERS, which is like the inside of a "[...]" (see
    !> `skip_characters_forward'), plus all non-ASCII characters.  The match
    !> data is set to match the word.  Port of comint.el:comint-word.
    recursive subroutine command_interpreter_word(word_characters, word, found)
        character(len=*), intent(in) :: word_characters
        character(len=:), allocatable, intent(out) :: word
        logical, intent(out) :: found
        integer(int64) :: here, startpoint, moved
        integer(int32) :: c
        type(status_type) :: st
        logical :: have, giveup

        ! FIXME: Need to handle "..." and '...' quoting in shell.el!
        ! This should be combined with completion parsing somehow.
        found = .false.
        word = ""
        here = point()
        giveup = .false.

        do while (.not. giveup)
            startpoint = point()
            moved = skip_characters_backward("\\"//word_characters, st)
            call character_after(c, have, point() - 2)

            if (have .and. c == iachar("\")) then
                call character_after(c, have, point() - 1)

                if (have) then
                    if (any(quote_characters() == c)) call goto_character(point() - 2)
                end if
            end if

            ! FIXME: This isn't consistent with Bash, at least -- not all
            ! non-ASCII chars should be word constituents.
            if (.not. is_beginning_of_buffer()) then
                if (preceding_character() >= 128) call goto_character(point() - 1)
            end if

            giveup = point() == startpoint
        end do

        ! Set match-data to match the entire string.
        if (point() < here) then
            call set_match_data([point(), here])
            call buffer_substring(point(), here, word, st)
            found = .true.
        end if

        call goto_character(here)
    end subroutine command_interpreter_word

    ! The characters of `command_interpreter_file_name_quote_characters'.  Not a port.
    function quote_characters() result(chars)
        integer(int32), allocatable :: chars(:)
        integer(int32) :: c, n
        integer(int64) :: i

        allocate (chars(0))
        if (.not. allocated(command_interpreter_file_name_quote_characters%value)) return
        i = 1

        do while (i <= len(command_interpreter_file_name_quote_characters%value))
            call string_character_and_length(command_interpreter_file_name_quote_characters%value, i, c, n)
            chars = [chars, c]
            i = i + n
        end do
    end function quote_characters

    ! Return the filename at point as-is as FILENAME, FOUND false if none
    ! is found.  See `command_interpreter_word'.  Port of
    ! comint.el:comint--match-partial-filename.
    recursive subroutine match_partial_filename(filename, found)
        character(len=:), allocatable, intent(out) :: filename
        logical, intent(out) :: found

        call command_interpreter_word(command_interpreter_file_name_characters%value, filename, found)
    end subroutine match_partial_filename

    ! QSTR unquoted as USTR, and QUPOS the position in QSTR that
    ! corresponds to UPOS in USTR (-1 for none).  Port of
    ! comint.el:comint--unquote&requote-argument.
    recursive subroutine unquote_requote_argument(qstr, upos_in, ustr, qupos)
        character(len=*), intent(in) :: qstr
        integer(int64), intent(in) :: upos_in
        character(len=:), allocatable, intent(out) :: ustr
        integer(int64), intent(out) :: qupos
        character(len=:), allocatable :: regular_expression, value
        integer(int64) :: upos, qpos, match, mend
        type(status_type) :: st
        logical :: found

        upos = upos_in
        qpos = 0
        ustr = ""
        qupos = -1
        regular_expression = "\$\(?:\([[:alpha:]][[:alnum:]]*\)\|{\(?1:[^{}]+\)}\)"
        if (size(quote_characters()) > 0) regular_expression = regular_expression//"\|\\\(.\)"

        do
            match = string_match(regular_expression, qstr, st, start=qpos + 1)
            if (match == 0) exit
            match = match - 1
            mend = match_end(0, st) - 1
            call push(substring(qstr, qpos, match), match)

            if (match_beginning(2, st) > 0) then
                call push(match_string(2, qstr), mend)
            else if (match_beginning(1, st) > 0) then
                call getenv(match_string(1, qstr), value, found)
                if (.not. found) value = ""
                call push(value, -mend)
            end if

            qpos = mend
        end do

        call push(substring(qstr, qpos), count_characters(qstr))

    contains

        subroutine push(string, end)
            character(len=*), intent(in) :: string
            integer(int64), intent(in) :: end

            ustr = ustr//string
            upos = upos - count_characters(string)

            if (qupos < 0 .and. upos <= 0) then
                if (end < 0) then
                    qupos = -end
                else
                    qupos = upos + end
                end if
            end if
        end subroutine push
    end subroutine unquote_requote_argument

    !> STRING with environment variables substituted and quoting `\'
    !> removed, as USTR (the default `command_interpreter_unquote_function').  Port of
    !> comint.el:comint--unquote-argument.
    recursive subroutine command_interpreter_unquote_argument(string, ustr)
        character(len=*), intent(in) :: string
        character(len=:), allocatable, intent(out) :: ustr
        integer(int64) :: qupos

        call unquote_requote_argument(string, 0_int64, ustr, qupos)
    end subroutine command_interpreter_unquote_argument

    !> The default `command_interpreter_requote_function'.  See
    !> `completion_table_with_quoting'.  Port of
    !> comint.el:comint--requote-argument.
    recursive subroutine command_interpreter_requote_argument(upos, qstr, qpos, qfun)
        integer(int64), intent(in) :: upos
        character(len=*), intent(in) :: qstr
        integer(int64), intent(out) :: qpos
        procedure(completion_quote_procedure), pointer, intent(out) :: qfun
        character(len=:), allocatable :: ustr

        call unquote_requote_argument(qstr, upos, ustr, qpos)
        qfun => command_interpreter_quote_filename
    end subroutine command_interpreter_requote_argument

    !> Return the unquoted&expanded filename at point as FILENAME, FOUND
    !> false if none is found.  Environment variables are substituted.
    !> See `command_interpreter_word'.  Port of comint.el:comint-match-partial-filename.
    recursive subroutine command_interpreter_match_partial_filename(filename, found)
        character(len=:), allocatable, intent(out) :: filename
        logical, intent(out) :: found
        character(len=:), allocatable :: unquoted

        call match_partial_filename(filename, found)

        if (found) then
            unquoted = filename
            call command_interpreter_unquote_argument(unquoted, filename)
        end if
    end subroutine command_interpreter_match_partial_filename

    !> Return FILENAME with magic characters quoted, as QUOTED.
    !> Magic characters are those in `command_interpreter_file_name_quote_characters'.
    !> Port of comint.el:comint-quote-filename.
    recursive subroutine command_interpreter_quote_filename(filename, quoted)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(out) :: quoted
        character(len=:), allocatable :: regular_expression, replaced
        integer(int64) :: i, mend
        integer(int64), allocatable :: saved(:)
        type(status_type) :: st

        quoted = filename
        if (size(quote_characters()) == 0) return
        regular_expression = regular_expression_optimization_character_set(quote_characters())
        saved = match_data()
        i = 1

        do while (string_match(regular_expression, quoted, st, start=i) > 0)
            mend = match_end(0, st)
            call replace_match("\\\&", st, string=quoted, result=replaced)
            quoted = replaced
            i = mend + 1
        end do

        call set_match_data(saved)
    end subroutine command_interpreter_quote_filename

    !> The file name completion table of comint: `completion_file_name_table'
    !> with `command_interpreter_file_name_prefix' before absolute file names.  Port of
    !> comint.el:comint-completion-file-name-table.
    recursive subroutine command_interpreter_completion_file_name_table(string, action, result, status, test, suffix)
        character(len=*), intent(in) :: string
        integer, intent(in) :: action
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_test_procedure), optional :: test
        character(len=*), intent(in), optional :: suffix
        type(status_type) :: st

        if (.not. is_file_name_absolute(string)) then
            call completion_file_name_table(string, action, result, status, test, suffix)
            return
        end if

        select case (action)
        case (COMPLETE_ALL, COMPLETE_TEST)
            call completion_file_name_table(command_interpreter_file_name_prefix%value//string, action, result, status, test, &
                                            suffix)
        case (COMPLETE_TRY)
            call completion_file_name_table(command_interpreter_file_name_prefix%value//string, action, result, status, test, &
                                            suffix)

            if (vtag(result) == V_STRING) then
                if (string_match("\`"//regular_expression_quote(command_interpreter_file_name_prefix%value), xstring(result), &
                                 st) > 0) then
                    result = value_type(text=substring(xstring(result), match_end(0, st) - 1))
                end if
            end if
        case default
            call completion_file_name_table(string, action, result, status, test, suffix)
        end select
    end subroutine command_interpreter_completion_file_name_table

    ! The table of `command_interpreter_file_name_completion_data': file names quoted
    ! with `command_interpreter_unquote_function' and `command_interpreter_requote_function',
    ! completed with `read_file_name_completion_ignore_case' and
    ! `command_interpreter_completion_fignore' (the lambda of
    ! comint--complete-file-name-data).  Port of
    ! comint.el:comint--complete-file-name-data, in part.
    recursive subroutine file_name_table(string, action, result, status, test, suffix)
        character(len=*), intent(in) :: string
        integer, intent(in) :: action
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_test_procedure), optional :: test
        character(len=*), intent(in), optional :: suffix
        type(completion_table_type) :: table
        character(len=len(completion_ignored_extensions)) :: saved_extensions(size(completion_ignored_extensions))
        procedure(completion_unquote_procedure), pointer :: unquote
        procedure(completion_requote_procedure), pointer :: requote
        character(len=8), allocatable :: fignore(:)
        logical :: saved_case
        integer :: n

        call quote_functions(unquote, requote)
        table = completion_table_with_quoting(command_interpreter_completion_file_name_table, unquote, requote)
        saved_case = completion_ignore_case%value
        saved_extensions = completion_ignored_extensions
        completion_ignore_case%value = read_file_name_completion_ignore_case
        completion_ignored_extensions = ""
        fignore = command_interpreter_completion_fignore%get_current()
        n = min(size(fignore), size(completion_ignored_extensions))
        completion_ignored_extensions(1:n) = fignore(1:n)
        call quoting_table_call(table, string, action, result, status, test, suffix)
        completion_ignore_case%value = saved_case
        completion_ignored_extensions = saved_extensions
    end subroutine file_name_table

    ! The current buffer's `command_interpreter_unquote_function' and
    ! `command_interpreter_requote_function'.  Not a port.
    subroutine quote_functions(unquote, requote)
        procedure(completion_unquote_procedure), pointer, intent(out) :: unquote
        procedure(completion_requote_procedure), pointer, intent(out) :: requote

        unquote => command_interpreter_unquote_function%get_current()
        requote => command_interpreter_requote_function%get_current()
    end subroutine quote_functions

    ! The FILESUFFIX of `command_interpreter_completion_addsuffix'.  Not a port.
    function file_suffix() result(suffix)
        character(len=:), allocatable :: suffix

        suffix = ""
        if (allocated(command_interpreter_completion_addsuffix%value)) suffix = command_interpreter_completion_addsuffix%value
    end function file_suffix

    ! The `:exit_function' of `command_interpreter_file_name_completion_data': add
    ! the FILESUFFIX of `command_interpreter_completion_addsuffix' (or move over it)
    ! when completion is finished.  Port of
    ! comint.el:comint--complete-file-name-data, in part.
    recursive subroutine file_suffix_exit(string, finished)
        character(len=*), intent(in) :: string
        integer, intent(in) :: finished
        character(len=:), allocatable :: suffix
        type(status_type) :: st

        ! (STRING is unused.)
        if (finished /= COMPLETION_FINISHED) return
        suffix = file_suffix()

        if (looking_at(regular_expression_quote(suffix), st)) then
            call goto_character(match_end(0, st))
        else
            call insert(suffix, st)
        end if
    end subroutine file_suffix_exit

    !> Return the completion data for file name at point.  Port of
    !> comint.el:comint--complete-file-name-data.
    recursive subroutine command_interpreter_file_name_completion_data(data)
        type(completion_at_point_function_data_type), intent(out) :: data
        character(len=:), allocatable :: filename
        type(status_type) :: st
        logical :: found

        call match_partial_filename(filename, found)
        data%found = .true.

        if (found) then
            data%start = match_beginning(0, st)
            data%end = match_end(0, st)
        else
            data%start = point()
            data%end = point()
        end if

        data%table = completion_function_table(file_name_table, "file")
        if (len(file_suffix()) > 0) data%exit_function => file_suffix_exit
    end subroutine command_interpreter_file_name_completion_data

    !> Return completion data for filename at point, if any.  Port of
    !> comint.el:comint-filename-completion.
    recursive subroutine command_interpreter_filename_completion(data, status)
        type(completion_at_point_function_data_type), intent(out) :: data
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: filename
        logical :: found

        status = status_type()
        data = completion_at_point_function_data_type()
        call match_partial_filename(filename, found)
        if (found) call command_interpreter_file_name_completion_data(data)
    end subroutine command_interpreter_filename_completion

    !> Expand input command history at point.
    !> For use on `completion_at_point_functions': DATA gives the function
    !> performing the expansion when there are history references.
    !> Port of comint.el:comint-c-a-p-replace-by-expanded-history (SILENT
    !> and START false).
    recursive subroutine command_interpreter_c_a_p_replace_by_expanded_history(data, status)
        type(completion_at_point_function_data_type), intent(out) :: data
        type(status_type), intent(out) :: status

        status = status_type()
        data = completion_at_point_function_data_type()
        if (.not. has_history_references()) return
        data%found = .true.
        data%perform => perform_expand_history
    end subroutine command_interpreter_c_a_p_replace_by_expanded_history

    ! The function comint-c-a-p-replace-by-expanded-history returns.
    ! Port of comint.el:comint-c-a-p-replace-by-expanded-history, in part.
    recursive logical function perform_expand_history() result(changed)
        integer(int64) :: tick
        type(status_type) :: st

        ! Looks like there might be history references in the command.
        tick = buffers(current)%text%modiff
        call command_interpreter_replace_by_expanded_history_before_point(status=st)
        changed = tick /= buffers(current)%text%modiff
    end function perform_expand_history

    !> Run `command_interpreter_dynamic_complete_functions' until one returns
    !> completion data (for `completion_at_point_functions').  Port of
    !> comint.el:comint-completion-at-point.
    recursive subroutine command_interpreter_completion_at_point(data, status)
        type(completion_at_point_function_data_type), intent(out) :: data
        type(status_type), intent(out) :: status
        type(completion_at_point_function_entry_type), allocatable :: functions(:)
        integer :: k

        status = status_type()
        data = completion_at_point_function_data_type()
        functions = command_interpreter_dynamic_complete_functions%functions()

        do k = 1, size(functions)
            call functions(k)%fn(data, status)
            if (.not. status%ok()) return
            if (data%found) return
        end do
    end subroutine command_interpreter_completion_at_point

    !> Dynamically complete the filename at point.
    !> Completes if after a filename.
    !> This function is similar to `command_interpreter_replace_by_expanded_filename',
    !> except that it won't change parts of the filename already entered
    !> in the buffer; it just adds completion characters to the end of the
    !> filename.  A completions listing may be shown in a separate buffer
    !> if completion is ambiguous.
    !>
    !> Completion is dependent on the value of
    !> `command_interpreter_completion_addsuffix', `command_interpreter_completion_recexact' and
    !> `command_interpreter_completion_fignore', and the timing of completions listing
    !> is dependent on the value of `command_interpreter_completion_autolist'.
    !>
    !> Returns true if successful.  Port of
    !> comint.el:comint-dynamic-complete-filename.
    recursive logical function command_interpreter_dynamic_complete_filename(status) result(done)
        type(status_type), intent(out) :: status
        type(completion_at_point_function_data_type) :: data
        character(len=:), allocatable :: filename
        logical :: found

        status = status_type()
        done = .false.
        call match_partial_filename(filename, found)
        if (.not. found) return
        if (.not. selected_frame%minibuffer_selected) call message("Completing file name...")
        call command_interpreter_file_name_completion_data(data)
        done = completion_in_region(data%start, data%end, data%table)
    end function command_interpreter_dynamic_complete_filename

    subroutine execute_command_interpreter_dynamic_complete_filename(status)
        type(status_type), intent(out) :: status
        logical :: done

        done = command_interpreter_dynamic_complete_filename(status)
    end subroutine execute_command_interpreter_dynamic_complete_filename

    !> Dynamically expand and complete the filename at point.
    !> Replace the filename with an expanded, canonicalized and completed
    !> replacement, i.e. substituting environment variables (e.g. $HOME),
    !> `~'s, `..', and `.', and making the filename absolute.  For
    !> expansion see `expand_file_name' and `substitute_in_file_name'.
    !> For completion see `command_interpreter_dynamic_complete_filename'.  Port of
    !> comint.el:comint-replace-by-expanded-filename.
    recursive subroutine command_interpreter_replace_by_expanded_filename(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: filename, expanded
        logical :: found, done

        status = status_type()
        call command_interpreter_match_partial_filename(filename, found)
        if (.not. found) return
        call expand_file_name(filename, expanded, status)
        if (.not. status%ok()) return
        call replace_match(expanded, status, fixedcase=.true., literal=.true.)
        if (.not. status%ok()) return
        done = command_interpreter_dynamic_complete_filename(status)
    end subroutine command_interpreter_replace_by_expanded_filename

    !> Display a list of possible completions for the filename at point.
    !> Port of comint.el:comint-dynamic-list-filename-completions.
    recursive subroutine command_interpreter_dynamic_list_filename_completions(status)
        type(status_type), intent(out) :: status
        type(completion_at_point_function_data_type) :: data
        type(completion_table_type) :: saved_table
        procedure(completion_test_procedure), pointer :: saved_test
        type(handle_type) :: ol

        status = status_type()
        call command_interpreter_file_name_completion_data(data)
        saved_table = minibuffer_completion_table
        saved_test => minibuffer_completion_test_function
        minibuffer_completion_table = data%table
        minibuffer_completion_test_function => null()
        call make_overlay(ol, data%start, data%end, status, rear_advance=.true.)

        if (status%ok()) then
            call overlay_put(ol, "field", value_type(name="completion"), status)
            call minibuffer_completion_help()
            call destroy_overlay(ol)
        end if

        minibuffer_completion_table = saved_table
        minibuffer_completion_test_function => saved_test
    end subroutine command_interpreter_dynamic_list_filename_completions

    ! ------------------------------------------------------------------
    ! Setup
    ! ------------------------------------------------------------------

    !> Define Comint mode, its map, faces and settings, and register the
    !> commands.  Not a port.
    subroutine initialize_command_interpreter()
        type(status_type) :: st

        if (associated(major_modes%command_interpreter_mode%execute)) return
        call major_modes%command_interpreter_mode%define("command_interpreter_mode", "Comint", command_interpreter_mode, &
                                                         command_interpreter_mode_hook)
        major_modes%command_interpreter_mode%special = .true.
        command_interpreter_mode_map = major_modes%command_interpreter_mode%map
        call define_faces()
        if (.not. allocated(command_interpreter_prompt_regular_expression%value)) command_interpreter_prompt_regular_expression = &
            "^"
        if (.not. allocated(command_interpreter_delimiter_arguments%value)) command_interpreter_delimiter_arguments = ""
        if (.not. allocated(command_interpreter_input_ring_separator%value)) command_interpreter_input_ring_separator = achar(10)
        if (.not. allocated(command_interpreter_input_history_ignore%value)) command_interpreter_input_history_ignore = "^#"
        if (.not. allocated(command_interpreter_terminfo_terminal%value)) command_interpreter_terminfo_terminal = "dumb"
        call forward_setting(command_interpreter_prompt_regular_expression)
        call forward_setting(command_interpreter_delimiter_arguments)
        call forward_setting(command_interpreter_prompt_read_only)
        call forward_setting(command_interpreter_highlight_input)
        call forward_setting(command_interpreter_input_ignoredups)
        call forward_setting(command_interpreter_input_ring_file_name)
        call forward_setting(command_interpreter_pager)
        call forward_setting(command_interpreter_input_ring_file_prefix)
        call forward_setting(command_interpreter_move_point_for_output)
        call forward_setting(command_interpreter_move_point_for_matching_input)
        call forward_setting(command_interpreter_scroll_show_maximum_output)
        call forward_setting(command_interpreter_buffer_maximum_size)
        call forward_setting(command_interpreter_input_ring_size)
        call forward_setting(command_interpreter_input_ring_separator)
        call forward_setting(command_interpreter_input_history_ignore)
        call forward_setting(command_interpreter_process_echoes)
        call forward_setting(command_interpreter_end_of_line_on_send)
        call forward_setting(command_interpreter_use_prompt_regular_expression)
        call forward_setting(command_interpreter_terminfo_terminal)
        call forward_setting(command_interpreter_inhibit_carriage_motion)
        if (.not. allocated(command_interpreter_completion_addsuffix%value)) command_interpreter_completion_addsuffix = " "
        if (.not. allocated(command_interpreter_file_name_prefix%value)) command_interpreter_file_name_prefix = ""
        if (.not. allocated(command_interpreter_file_name_characters%value)) command_interpreter_file_name_characters = &
            "[]~/A-Za-z0-9+@:_.$#%,={}-"

        if (.not. allocated(command_interpreter_file_name_quote_characters%value)) then
            command_interpreter_file_name_quote_characters = ""
        end if

        call forward_setting(command_interpreter_file_name_prefix)
        call forward_setting(command_interpreter_file_name_characters)
        call forward_setting(command_interpreter_file_name_quote_characters)
        call command_interpreter_dynamic_complete_functions%prepend(command_interpreter_filename_completion)
        call command_interpreter_dynamic_complete_functions%prepend(command_interpreter_c_a_p_replace_by_expanded_history)
        ! (The default value lists comint-postoutput-scroll-to-bottom
        ! before comint-watch-for-password-prompt; `prepend' puts a function
        ! first.)
        call command_interpreter_output_filter_functions%prepend(command_interpreter_watch_for_password_prompt)
        call command_interpreter_output_filter_functions%prepend(command_interpreter_postoutput_scroll_to_bottom)
        call initialize_multilingual_environment_configuration()

        if (.not. allocated(command_interpreter_password_prompt_regular_expression)) then
            command_interpreter_password_prompt_regular_expression = &
                command_interpreter_password_prompt_regular_expression_default()
        end if

        commands%command_interpreter_send_invisible%execute => execute_command_interpreter_send_invisible
        call append_locals_clearer(clear_command_interpreter_locals)

        if (.not. associated(command_interpreter_get_old_input%procedure)) then
            command_interpreter_get_old_input%procedure => command_interpreter_get_old_input_default
        end if

        if (.not. associated(command_interpreter_input_sender%procedure)) then
            command_interpreter_input_sender%procedure => command_interpreter_simple_send
        end if

        if (.not. associated(command_interpreter_input_filter%procedure)) then
            command_interpreter_input_filter%procedure => is_command_interpreter_nonblank
        end if

        if (.not. associated(command_interpreter_unquote_function%procedure)) then
            command_interpreter_unquote_function%procedure => command_interpreter_unquote_argument
        end if

        if (.not. associated(command_interpreter_requote_function%procedure)) then
            command_interpreter_requote_function%procedure => command_interpreter_requote_argument
        end if

        ! Keys:
        call keymap_set(command_interpreter_mode_map, "M-p", "command_interpreter_previous_input", st)
        call keymap_set(command_interpreter_mode_map, "M-n", "command_interpreter_next_input", st)
        call keymap_set(command_interpreter_mode_map, "C-<up>", "command_interpreter_previous_input", st)
        call keymap_set(command_interpreter_mode_map, "C-<down>", "command_interpreter_next_input", st)
        call keymap_set(command_interpreter_mode_map, "C-x <up>", "command_interpreter_complete_input_ring", st)
        call keymap_set(command_interpreter_mode_map, "M-r", "command_interpreter_history_search_backward_regular_expression", st)
        call keymap_set(command_interpreter_mode_map, "C-c M-r", "command_interpreter_previous_matching_input_from_input", st)
        call keymap_set(command_interpreter_mode_map, "C-c M-s", "command_interpreter_next_matching_input_from_input", st)
        call keymap_set(command_interpreter_mode_map, "C-M-l", "command_interpreter_show_output", st)
        call keymap_set(command_interpreter_mode_map, "RET", "command_interpreter_send_input", st)
        call keymap_set(command_interpreter_mode_map, "C-d", "command_interpreter_delchar_or_maybe_eof", st)
        ! The following two are standardly bound to delete-forward-char, but
        ! they should never do EOF, just delete.
        call keymap_set(command_interpreter_mode_map, "<delete>", "delete_forward_character", st)
        call keymap_set(command_interpreter_mode_map, "<kp-delete>", "delete_forward_character", st)
        call keymap_set(command_interpreter_mode_map, "C-c SPC", "command_interpreter_accumulate", st)
        call keymap_set(command_interpreter_mode_map, "C-c C-x", "command_interpreter_get_next_from_history", st)
        call keymap_set(command_interpreter_mode_map, "C-c C-a", "command_interpreter_beginning_of_line_or_process_mark", st)
        call keymap_set(command_interpreter_mode_map, "C-c C-u", "command_interpreter_cut_input", st)
        call keymap_set(command_interpreter_mode_map, "C-c C-w", "backward_cut_word", st)
        call keymap_set(command_interpreter_mode_map, "C-c C-c", "command_interpreter_interrupt_subjob", st)
        call keymap_set(command_interpreter_mode_map, "C-c C-z", "command_interpreter_suspend_subjob", st)
        call keymap_set(command_interpreter_mode_map, "C-c C-\", "command_interpreter_quit_subjob", st)
        call keymap_set(command_interpreter_mode_map, "C-c RET", "command_interpreter_copy_old_input", st)
        call keymap_set(command_interpreter_mode_map, "C-c C-o", "command_interpreter_delete_output", st)
        call keymap_set(command_interpreter_mode_map, "C-c M-o", "command_interpreter_clear_buffer", st)
        call keymap_set(command_interpreter_mode_map, "C-c C-r", "command_interpreter_show_output", st)
        call keymap_set(command_interpreter_mode_map, "C-c C-e", "command_interpreter_show_maximum_output", st)
        call keymap_set(command_interpreter_mode_map, "C-c C-n", "command_interpreter_next_prompt", st)
        call keymap_set(command_interpreter_mode_map, "C-c C-p", "command_interpreter_previous_prompt", st)

        if (.not. is_keymap_live(command_interpreter_repeat_map)) then
            call make_sparse_keymap(command_interpreter_repeat_map)
            call keymap_set(command_interpreter_repeat_map, "C-n", "command_interpreter_next_prompt", st)
            call keymap_set(command_interpreter_repeat_map, "C-p", "command_interpreter_previous_prompt", st)
            call set_repeat_map(command_interpreter_repeat_map)
        end if

        call keymap_set(command_interpreter_mode_map, "C-c C-d", "command_interpreter_send_eof", st)
        call keymap_set(command_interpreter_mode_map, "C-c C-s", "command_interpreter_write_output", st)
        call keymap_set(command_interpreter_mode_map, "C-c .", "command_interpreter_insert_previous_argument", st)
        commands%command_interpreter_mode%execute => command_interpreter_mode
        commands%command_interpreter_execute%execute => execute_command_interpreter_execute
        commands%command_interpreter_send_input%execute => execute_command_interpreter_send_input
        commands%command_interpreter_previous_input%execute => execute_command_interpreter_previous_input
        commands%command_interpreter_next_input%execute => execute_command_interpreter_next_input
        commands%command_interpreter_previous_matching_input%execute => execute_command_interpreter_previous_matching_input
        commands%command_interpreter_next_matching_input%execute => execute_command_interpreter_next_matching_input
        commands%command_interpreter_previous_matching_input_from_input%execute => &
            execute_command_interpreter_previous_matching_input_from_input
        commands%command_interpreter_next_matching_input_from_input%execute => &
            execute_command_interpreter_next_matching_input_from_input
        commands%command_interpreter_restore_input%execute => command_interpreter_restore_input
        commands%command_interpreter_backward_matching_input%execute => execute_command_interpreter_backward_matching_input
        commands%command_interpreter_forward_matching_input%execute => execute_command_interpreter_forward_matching_input
        commands%command_interpreter_show_output%execute => command_interpreter_show_output
        commands%command_interpreter_show_maximum_output%execute => command_interpreter_show_maximum_output
        commands%command_interpreter_delchar_or_maybe_eof%execute => command_interpreter_delchar_or_maybe_eof
        commands%command_interpreter_send_eof%execute => command_interpreter_send_eof
        commands%command_interpreter_accumulate%execute => command_interpreter_accumulate
        commands%command_interpreter_get_next_from_history%execute => command_interpreter_get_next_from_history
        commands%command_interpreter_beginning_of_line_or_process_mark%execute => &
            command_interpreter_beginning_of_line_or_process_mark
        commands%command_interpreter_beginning_of_line%execute => execute_command_interpreter_beginning_of_line
        commands%command_interpreter_goto_process_mark%execute => execute_command_interpreter_goto_process_mark
        commands%command_interpreter_set_process_mark%execute => command_interpreter_set_process_mark
        commands%command_interpreter_cut_input%execute => command_interpreter_cut_input
        commands%command_interpreter_interrupt_subjob%execute => command_interpreter_interrupt_subjob
        commands%comint_stop_subjob%execute => comint_stop_subjob
        commands%command_interpreter_quit_subjob%execute => command_interpreter_quit_subjob
        commands%command_interpreter_suspend_subjob%execute => command_interpreter_suspend_subjob
        commands%command_interpreter_continue_subjob%execute => command_interpreter_continue_subjob
        commands%command_interpreter_replace_by_expanded_history%execute => execute_command_interpreter_replace_by_expanded_history
        commands%command_interpreter_magic_space%execute => execute_command_interpreter_magic_space
        commands%command_interpreter_insert_previous_argument%execute => execute_command_interpreter_insert_previous_argument
        commands%command_interpreter_history_incremental_search_backward%execute => &
            command_interpreter_history_incremental_search_backward
        commands%command_interpreter_history_search_backward_regular_expression%execute => &
            command_interpreter_history_search_backward_regular_expression
        commands%command_interpreter_copy_old_input%execute => command_interpreter_copy_old_input
        commands%command_interpreter_delete_output%execute => execute_command_interpreter_delete_output
        commands%command_interpreter_write_output%execute => execute_command_interpreter_write_output
        commands%command_interpreter_append_output_to_file%execute => execute_command_interpreter_append_output_to_file
        commands%command_interpreter_clear_buffer%execute => command_interpreter_clear_buffer
        commands%command_interpreter_truncate_buffer%execute => execute_command_interpreter_truncate_buffer
        commands%command_interpreter_strip_control_m%execute => execute_command_interpreter_strip_control_m
        commands%command_interpreter_next_prompt%execute => execute_command_interpreter_next_prompt
        commands%command_interpreter_previous_prompt%execute => execute_command_interpreter_previous_prompt
        commands%command_interpreter_cut_whole_line%execute => execute_command_interpreter_cut_whole_line
        commands%command_interpreter_cut_region%execute => execute_command_interpreter_cut_region
        commands%command_interpreter_complete_input_ring%execute => command_interpreter_complete_input_ring
        commands%command_interpreter_dynamic_complete_filename%execute => execute_command_interpreter_dynamic_complete_filename
        commands%command_interpreter_replace_by_expanded_filename%execute => command_interpreter_replace_by_expanded_filename
        commands%command_interpreter_dynamic_list_filename_completions%execute => &
            command_interpreter_dynamic_list_filename_completions
    end subroutine initialize_command_interpreter

    !> The faces of comint.el.  Not a port.
    subroutine define_faces()
        type(named_face_type), pointer :: f

        f => command_interpreter_highlight_input_face
        call f%define("command_interpreter_highlight_input", "Face to use to highlight user input.")
        call f%specification_entry(display_when(), face_attributes(weight="bold"))
        f => command_interpreter_highlight_prompt
        call f%define("command_interpreter_highlight_prompt", "Face to use to highlight prompts.")
        call f%specification_entry(display_when(), face_attributes(inherit="minibuffer_prompt"))
    end subroutine define_faces
end module command_interpreter_mod
