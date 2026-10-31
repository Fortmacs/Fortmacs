! comint_fm.f90 -- general command interpreter in a window stuff.
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
!> goes in through `comint_output_filter' (carriage motion, the prompt
!> highlighted and marked as output fields); RET (`comint_send_input')
!> sends what follows the process mark, keeping it in the input ring
!> (M-p, M-n, C-c M-r, C-c M-s); C-c C-c, C-c C-\ and C-c C-d interrupt,
!> quit and end the input of the current subjob; C-c C-o, C-c C-r and
!> C-c C-e work on the last output.
!>
!> A comint buffer's own state (the input ring, its markers, its input
!> sender and old-input function) is kept by buffer here; the input ring
!> and its indices are permanent, as upstream's are.
!>
!> Input history expansion (`comint_input_autoexpand',
!> `comint_replace_by_expanded_history', `comint_magic_space',
!> `comint_insert_previous_argument' on C-c .), Isearch in the input
!> history (`comint_history_isearch_backward_regexp' on M-r,
!> `comint_history_isearch'), `comint_stop_subjob' (C-c C-z) and
!> `comint_continue_subjob', and `comint_scroll_to_bottom_on_input' are
!> ported.
!>
!> Password prompts are recognized (`comint_watch_for_password_prompt'
!> on `comint_output_filter_functions', `comint_password_prompt_regexp')
!> and answered with `comint_send_invisible'.
!>
!> Completion: `comint_completion_at_point' on
!> `completion_at_point_functions' runs `comint_dynamic_complete_functions'
!> (history references, then file names quoted with `\' for
!> `comint_file_name_quote_list'), `comint_dynamic_complete_filename',
!> `comint_replace_by_expanded_filename',
!> `comint_dynamic_list_filename_completions' and
!> `comint_complete_input_ring' (C-x <up>).
!>
!> Not ported yet (Deferred): `comint_dynamic_list_input_ring' (C-c C-l)
!> and `comint_dynamic_list_completions' (choosing from the list needs
!> `choose_completion_string'); the status of a stopped
!> process (process_fm reports none); redirection (`comint_redirect_*'); network
!> connections and ptys without a program as PROGRAM; `comint_fontify_input_mode'
!> and indirect buffers; the mouse (`comint_insert_input'), menus and
!> `comint_repeat_map'; `insert_in_front_hooks' on output (the
!> property is set, text property hooks are not ported);
!> `filter_buffer_substring_function'; `window_point_insertion_type';
!> `comint_osc_process_output'; source file commands
!> (`comint_get_source' and friends); `comint_run' with switches read
!> after C-u (an error says so); coding systems.
module comint_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use error_fm, only: status_type, set_error, format_error, format_message, FM_GENERIC_ERROR, FM_USER_ERROR
    use handle_fm, only: handle_type, null_handle
    use lisp_fm, only: value_type, nil, nilp, t_value, list, consp, symbolp, symbols, xsymbol, list_elements, &
                       command_proc
    use lread_fm, only: sym
    use data_fm, only: string_setting, logical_setting, integer_setting, symbol_value, set, value_eq, &
                       add_local_variables_killer, forward_setting
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, get_buffer_create, buffer_live_p, &
                         barf_if_buffer_read_only
    use marker_fm, only: make_marker, free_marker, marker_buffer, marker_position, set_marker, copy_marker_at, &
                         detach_marker
    use insdel_fm, only: insert
    use editfns_fm, only: interactive_region, point, point_min, point_max, goto_char, widen, narrow_to_region, delete_region, &
                          buffer_substring, line_beginning_position, line_end_position, following_char, eobp, &
                          bobp, bolp, field_beginning, field_end, field_string, constrain_to_field, &
                          inhibit_field_text_motion, eolp
    use textprop_fm, only: get_char_property, put_text_property, get_text_property, add_text_properties, &
                           remove_list_of_text_properties, &
                           remove_text_properties, next_single_char_property_change, previous_single_char_property_change
    use cmds_fm, only: delete_char, forward_char, forward_line, beginning_of_line, end_of_line, self_insert_command
    use syntax_fm, only: skip_chars_forward
    use search_fm, only: looking_at, string_match, re_search_backward, match_beginning, match_end, regexp_quote, &
                         SEARCH_NOERROR, replace_match, case_fold_search
    use subr_fm, only: silent_modifications_type, silent_modifications_begin, silent_modifications_end, match_string
    use eval_fm, only: hook_type, string_hook_type, string_filter_hook_type
    use keymap_fm, only: keymap_set, lookup_key, key_description, command_remapping, current_global_map, &
                         make_sparse_keymap, keymap_live_p, set_repeat_map
    use modes_fm, only: major_modes
    use simple_fm, only: kill_region, copy_region_as_kill, push_mark, kill_whole_line, just_one_space
    use process_fm, only: process_arg_type, make_process, get_buffer_process, process_status, process_mark, &
                          process_buffer, delete_process, set_process_filter, process_send_string, process_send_region, &
                          process_send_eof, interrupt_process, kill_process, quit_process, accept_process_output, &
                          processp, process_connection_type, PROCESS_CONNECTION_PIPE, PROCESS_CONNECTION_PTY, &
                          stop_process, continue_process
    use window_fm, only: windows, window_type, window_point, set_window_point, set_window_start, recenter, &
                         window_leaves, window_text_cols, select_window, same_handle
    use frame_fm, only: selected_frame
    use xdisp_fm, only: message, set_mode_line_process, mode_line_part_type
    use font_core_fm, only: font_lock_defaults
    use font_lock_fm, only: font_lock_append_text_property, font_lock_remove_face_from_text_property
    use faces_fm, only: named_face_type, display_when, face_attrs
    use fileio_fm, only: file_readable_p, file_writable_p, file_accessible_directory_p, insert_file_contents, &
                         write_region, expand_file_name
    use minibuf_fm, only: read_from_minibuffer, read_string
    use minibuffer_fm, only: read_file_name
    use paragraphs_fm, only: paragraph_start, forward_paragraph
    use callint_fm, only: current_prefix_arg, prefix_numeric_value, call_interactively, this_command
    use keyboard_fm, only: last_command, this_command_keys
    use dispnew_fm, only: ding
    use commands_fm, only: commands
    use ring_fm, only: ring_type
    use version_fm, only: upstream_version
    use isearch_fm, only: isearch_mode, isearch_forward, isearch_success, isearch_error, isearch_nonincremental, &
                          isearch_message_prefix, isearch_message_display, isearch_search_proc, isearch_pop_proc, &
                          isearch_search_fun_default, isearch_search_fun_function, isearch_message_function, &
                          isearch_wrap_function, isearch_push_state_function, isearch_mode_hook, isearch_mode_end_hook, &
                          isearch_message_prefix_add, set_isearch_barrier, set_isearch_opoint
    use itree_fm, only: make_overlay, move_overlay, delete_overlay, overlay_put, overlayp
    use keyboard_fm, only: pre_command_hook
    use auth_source_fm, only: read_passwd
    use mule_conf_fm, only: password_word_equivalents, password_colon_equivalents, syms_of_mule_conf
    use regexp_opt_fm, only: regexp_opt, REGEXP_OPT_T
    use character_fm, only: char_string, MAX_MULTIBYTE_LENGTH
    use timer_fm, only: run_at_time
    use lisp_fm, only: make_string, make_list, stringp, xstring
    use minibuf_table_fm, only: completion_table_type, completion_predicate, completion_unquote_proc, &
                                completion_requote_proc, completion_quote_proc, completion_table_with_quoting, &
                                capf_data_type, COMPLETION_FINISHED, COMPLETE_TRY, COMPLETE_ALL, COMPLETE_TEST, &
                                completion_list_table, completion_function_table
    use minibuf_fm, only: completion_ignore_case, quoting_table_call
    use minibuffer_fm, only: completion_in_region, completion_in_region_mode_predicate, cir_predicate_proc, &
                             completions_visible, completion_at_point_functions, completion_file_name_table, &
                             read_file_name_completion_ignore_case, minibuffer_completion_table, &
                             minibuffer_completion_predicate, minibuffer_completion_help
    use eval_fm, only: capf_hook_type, capf_entry_type
    use search_fm, only: set_match_data, match_data
    use syntax_fm, only: skip_chars_backward
    use editfns_fm, only: char_after, preceding_char
    use character_fm, only: string_char_and_length
    use fns_fm, only: substring, schars
    use env_fm, only: getenv
    use fileio_fm, only: file_name_absolute_p
    use dired_fm, only: completion_ignored_extensions
    use regexp_opt_fm, only: regexp_opt_charset
    use itree_fm, only: free_overlay
    implicit none(type, external)
    private

    public :: comint_mode, syms_of_comint, comint_check_proc, make_comint_in_buffer, make_comint, comint_run, &
              comint_exec, comint_term_environment, comint_nonblank_p, comint_output_filter, comint_carriage_motion, &
              comint_send_input, comint_simple_send, comint_send_string, comint_send_region, comint_snapshot_last_prompt, &
              comint_get_old_input_default, comint_copy_old_input, comint_skip_prompt, comint_after_pmark_p, &
              comint_line_beginning_position, comint_bol, comint_add_to_input_history, comint_previous_input, &
              comint_next_input, comint_previous_matching_input, comint_next_matching_input, &
              comint_previous_matching_input_from_input, comint_next_matching_input_from_input, comint_restore_input, &
              comint_delete_input, comint_delete_output, comint_write_output, comint_append_output_to_file, &
              comint_show_output, comint_show_maximum_output, comint_clear_buffer, comint_truncate_buffer, &
              comint_strip_ctrl_m, comint_interrupt_subjob, comint_kill_subjob, comint_quit_subjob, comint_kill_input, &
              comint_delchar_or_maybe_eof, comint_send_eof, comint_backward_matching_input, &
              comint_forward_matching_input, comint_next_prompt, comint_previous_prompt, comint_update_fence, &
              comint_kill_whole_line, comint_kill_region, comint_get_next_from_history, comint_accumulate, &
              comint_goto_process_mark, comint_bol_or_process_mark, comint_set_process_mark, comint_read_input_ring, &
              comint_write_input_ring, comint_postoutput_scroll_to_bottom, comint_adjust_window_point, &
              comint_input_ring_item, comint_input_ring_length, comint_last_input_start, comint_last_input_end, &
              comint_last_output_start, set_comint_get_old_input, set_comint_input_sender, set_comint_input_filter, &
              comint_ptyp, comint_arguments, comint_delim_arg, comint_substitute_in_file_name, &
              comint_previous_input_string, comint_within_quotes, comint_how_many_region, &
              comint_replace_by_expanded_history, comint_c_a_p_replace_by_expanded_history, &
              comint_replace_by_expanded_history_before_point, comint_magic_space, comint_insert_previous_argument, &
              comint_history_isearch_backward, comint_history_isearch_backward_regexp, comint_history_isearch_setup, &
              comint_history_isearch_end, comint_goto_input, comint_preinput_scroll_to_bottom, comint_stop_subjob, &
              comint_continue_subjob, comint_complete_input_ring, comint_word, comint_match_partial_filename, &
              comint_quote_filename, comint_unquote_argument, comint_requote_argument, &
              comint_completion_file_name_table, comint_file_name_completion_data, comint_filename_completion, &
              comint_completion_at_point, comint_dynamic_complete_filename, comint_replace_by_expanded_filename, &
              comint_dynamic_list_filename_completions, set_comint_completion_fignore, set_comint_quote_functions, &
              comint_directory
    public :: comint_input_sender_proc, comint_get_old_input_proc, comint_input_filter_proc, comint_password_proc
    public :: comint_send_invisible, comint_read_noecho, comint_watch_for_password_prompt, &
              set_comint_password_function, comint_password_prompt_regexp_default

    abstract interface
        !> A `comint_input_sender': send to PROCESS the input STRING.
        !> Not a port.
        subroutine comint_input_sender_proc(process, string, status)
            import :: handle_type, status_type
            implicit none(type, external)
            type(handle_type), intent(in) :: process
            character(len=*), intent(in) :: string
            type(status_type), intent(out) :: status
        end subroutine comint_input_sender_proc

        !> A `comint_get_old_input' function: the old text at point, as
        !> INPUT.  Not a port.
        subroutine comint_get_old_input_proc(input, status)
            import :: status_type
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: input
            type(status_type), intent(out) :: status
        end subroutine comint_get_old_input_proc

        !> A `comint_input_filter': whether INPUT may be saved on the input
        !> history.  Not a port.
        logical function comint_input_filter_proc(input)
            implicit none(type, external)
            character(len=*), intent(in) :: input
        end function comint_input_filter_proc
    end interface

    abstract interface
        !> A `comint_password_function': the password for PROMPT, FOUND
        !> false if normal password prompting should occur.  Not a port.
        recursive subroutine comint_password_proc(prompt, password, found)
            implicit none(type, external)
            character(len=*), intent(in) :: prompt
            character(len=:), allocatable, intent(out) :: password
            logical, intent(out) :: found
        end subroutine comint_password_proc
    end interface

    !> Values of `comint_move_point_for_output'.
    integer, parameter, public :: COMINT_MOVE_NIL = 0, COMINT_MOVE_T = 1, COMINT_MOVE_ALL = 2, &
                                  COMINT_MOVE_THIS = 3, COMINT_MOVE_OTHERS = 4

    !> Values of `comint_move_point_for_matching_input'.
    integer, parameter, public :: COMINT_MATCHING_AFTER_INPUT = 0, COMINT_MATCHING_END_OF_LINE = 1

    !> Regexp to recognize prompts in the inferior process.
    !> Defaults to "^", the null string at BOL.
    !>
    !> This variable is only used if the variable
    !> `comint_use_prompt_regexp' is true.
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
    type(string_setting), target, public :: comint_prompt_regexp = string_setting(name="comint_prompt_regexp")

    !> If true, the comint prompt is read only.
    !> The read only region includes the newline before the prompt.
    !> This does not affect existing prompts.
    !> Certain derived modes may override this option.
    !>
    !> If you set this option to true, then the safe way to temporarily
    !> override the read-only-ness of comint prompts is to call
    !> `comint_kill_whole_line' or `comint_kill_region' with no
    !> narrowing in effect.  This way you will be certain that none of
    !> the remaining prompts will be accidentally messed up.  Port of
    !> comint.el comint-prompt-read-only.
    type(logical_setting), target, public :: comint_prompt_read_only = &
                                             logical_setting(name="comint_prompt_read_only", value=.false.)

    !> If true, highlight input with `comint_highlight_input' face.
    !> Otherwise keep the original highlighting untouched.  Port of
    !> comint.el comint-highlight-input.
    type(logical_setting), target, public :: comint_highlight_input = &
                                             logical_setting(name="comint_highlight_input", value=.true.)

    !> If true, don't add input matching the last on the input ring.
    !> This mirrors the optional behavior of bash.
    !>
    !> This variable is buffer-local.  Port of comint.el
    !> comint-input-ignoredups.
    type(logical_setting), target, public :: comint_input_ignoredups = &
                                             logical_setting(name="comint_input_ignoredups", value=.false.)

    !> If allocated, name of the file to read/write input history.
    !> See also `comint_read_input_ring' and `comint_write_input_ring'.
    !> `comint_mode' makes this a buffer-local variable.  You probably want
    !> to set this in a mode hook, rather than customize the default
    !> value.  Port of comint.el comint-input-ring-file-name.
    type(string_setting), target, public :: comint_input_ring_file_name = &
                                            string_setting(name="comint_input_ring_file_name")

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
    !> Emacs buffer, so in those cases pagination might need to be
    !> disabled.  Disabling pagination means that some programs will
    !> produce large amounts of output, but most such programs have other
    !> ways to limit their output, such as additional arguments or Emacs
    !> interfaces.  To disable pagination, this variable's value should be
    !> a string that names a program, such as "cat", which passes through
    !> all of the output without any filtering or delays.  Comint will then
    !> set the PAGER variable to name that program, when it invokes
    !> external programs.  Port of comint.el comint-pager.
    type(string_setting), target, public :: comint_pager = string_setting(name="comint_pager")

    !> The prefix to skip when parsing the input ring file.
    !> This is useful in Zsh when the extended_history option is on.  Port
    !> of comint.el comint-input-ring-file-prefix.
    type(string_setting), target, public :: comint_input_ring_file_prefix = &
                                            string_setting(name="comint_input_ring_file_prefix")

    !> Controls whether interpreter output moves point to the end of the
    !> output.
    !> If COMINT_MOVE_NIL, then output never moves point to the output.
    !>  (If the output occurs at point, it is inserted before point.)
    !> If COMINT_MOVE_T or COMINT_MOVE_ALL, move point in all windows
    !> showing the buffer.
    !> If COMINT_MOVE_THIS, move point only the selected window.
    !> If COMINT_MOVE_OTHERS, move point only in other windows, not in the
    !> selected window.
    !>
    !> The default is COMINT_MOVE_NIL.
    !>
    !> See the variable `comint_scroll_show_maximum_output' and the
    !> function `comint_postoutput_scroll_to_bottom'.
    !> This variable is buffer-local in all Comint buffers.  Port of
    !> comint.el comint-move-point-for-output.
    type(integer_setting), target, public :: comint_move_point_for_output = &
                                             integer_setting(name="comint_move_point_for_output", value=COMINT_MOVE_NIL)

    !> Controls where to place point after matching input.
    !> \<comint_mode_map>This influences the commands
    !> \[comint_previous_matching_input_from_input] and
    !> \[comint_next_matching_input_from_input].
    !> If COMINT_MATCHING_AFTER_INPUT, point will be positioned after the
    !> input typed by the user, but before the rest of the history entry
    !> that has been inserted.  If COMINT_MATCHING_END_OF_LINE, point will
    !> be positioned at the end of the current logical (not visual) line
    !> after insertion.  Port of comint.el
    !> comint-move-point-for-matching-input.
    type(integer_setting), target, public :: comint_move_point_for_matching_input = &
                                             integer_setting(name="comint_move_point_for_matching_input", &
                                                             value=COMINT_MATCHING_AFTER_INPUT)

    !> Controls how to scroll due to interpreter output.
    !> This variable applies when point is at the end of the buffer
    !> (either because it was originally there, or because
    !> `comint_move_point_for_output' said to move it there)
    !> and output from the subprocess is inserted.
    !>
    !> True means scroll so that the window is full of text
    !> and point is on the last line.  A value of false
    !> means don't do anything special--scroll normally.
    !>
    !> See also the variable `comint_move_point_for_output' and the function
    !> `comint_postoutput_scroll_to_bottom'.
    !> This variable is buffer-local in all Comint buffers.  Port of
    !> comint.el comint-scroll-show-maximum-output.
    type(logical_setting), target, public :: comint_scroll_show_maximum_output = &
                                             logical_setting(name="comint_scroll_show_maximum_output", value=.true.)

    !> The maximum size in lines for Comint buffers.
    !> Comint buffers are truncated from the top to be no greater than this
    !> number, if the function `comint_truncate_buffer' is on
    !> `comint_output_filter_functions'.  Port of comint.el
    !> comint-buffer-maximum-size.
    type(integer_setting), target, public :: comint_buffer_maximum_size = &
                                             integer_setting(name="comint_buffer_maximum_size", value=1024)

    !> Size of the input history ring in `comint_mode'.  Port of comint.el
    !> comint-input-ring-size.
    type(integer_setting), target, public :: comint_input_ring_size = &
                                             integer_setting(name="comint_input_ring_size", value=500)

    !> Separator between commands in the history file.  Port of comint.el
    !> comint-input-ring-separator.
    type(string_setting), target, public :: comint_input_ring_separator = &
                                            string_setting(name="comint_input_ring_separator")

    !> Regexp for history entries that should be ignored when Comint
    !> initializes.  Port of comint.el comint-input-history-ignore.
    type(string_setting), target, public :: comint_input_history_ignore = &
                                            string_setting(name="comint_input_history_ignore")

    !> If true, assume that the subprocess echoes any input.
    !> If so, delete one copy of the input so that only one copy eventually
    !> appears in the buffer.
    !>
    !> This variable is buffer-local.  Port of comint.el
    !> comint-process-echoes.
    type(logical_setting), target, public :: comint_process_echoes = &
                                             logical_setting(name="comint_process_echoes", value=.false.)

    !> True means go to the end of the line before sending input.
    !> See `comint_send_input'.  Port of comint.el comint-eol-on-send.
    type(logical_setting), target, public :: comint_eol_on_send = &
                                             logical_setting(name="comint_eol_on_send", value=.true.)

    !> If true, use `comint_prompt_regexp' to recognize prompts.
    !> If false, then program output and user-input are given different
    !> `field' properties, which Emacs commands can use to distinguish them
    !> (in particular, common movement commands such as
    !> `beginning_of_line' respect field boundaries in a natural way).
    !> Port of comint.el comint-use-prompt-regexp.
    type(logical_setting), target, public :: comint_use_prompt_regexp = &
                                             logical_setting(name="comint_use_prompt_regexp", value=.false.)

    !> Values of `comint_input_autoexpand'.
    integer, parameter, public :: COMINT_AUTOEXPAND_NIL = 0, COMINT_AUTOEXPAND_T = 1, COMINT_AUTOEXPAND_INPUT = 2, &
                                  COMINT_AUTOEXPAND_HISTORY = 3

    !> If not COMINT_AUTOEXPAND_NIL, expand input command history
    !> references on completion.  This mirrors the optional behavior of
    !> tcsh (its autoexpand and histlist).
    !>
    !> If the value is COMINT_AUTOEXPAND_INPUT, then the expansion is seen
    !> on input.  If the value is COMINT_AUTOEXPAND_HISTORY, then the
    !> expansion is only when inserting into the buffer's input ring.
    !> See also `comint_magic_space' and `completion_at_point'.
    !>
    !> This variable is buffer-local.  Port of comint.el
    !> comint-input-autoexpand.
    type(integer_setting), target, public :: comint_input_autoexpand = &
                                             integer_setting(name="comint_input_autoexpand", value=COMINT_AUTOEXPAND_NIL)

    !> Values of `comint_scroll_to_bottom_on_input'.
    integer, parameter, public :: COMINT_SCROLL_ON_INPUT_NIL = 0, COMINT_SCROLL_ON_INPUT_T = 1, &
                                  COMINT_SCROLL_ON_INPUT_ALL = 2, COMINT_SCROLL_ON_INPUT_THIS = 3

    !> Controls whether input to interpreter causes window to scroll.
    !> If COMINT_SCROLL_ON_INPUT_NIL, then do not scroll.  If
    !> COMINT_SCROLL_ON_INPUT_T or COMINT_SCROLL_ON_INPUT_ALL, scroll all
    !> windows showing buffer.  If COMINT_SCROLL_ON_INPUT_THIS, scroll only
    !> the selected window.
    !>
    !> The default is COMINT_SCROLL_ON_INPUT_NIL.
    !>
    !> See `comint_preinput_scroll_to_bottom'.  This variable is
    !> buffer-local.  Port of comint.el comint-scroll-to-bottom-on-input.
    type(integer_setting), target, public :: comint_scroll_to_bottom_on_input = &
                                             integer_setting(name="comint_scroll_to_bottom_on_input", &
                                                             value=COMINT_SCROLL_ON_INPUT_NIL)

    !> If true, `comint_insert_previous_argument' counts args from the
    !> end.  If this variable is false, the default,
    !> `comint_insert_previous_argument' counts the arguments from the
    !> beginning; if true, it counts from the end instead.  This emulates
    !> the behavior of `ESC-NUM ESC-.' in both Bash and zsh: in Bash,
    !> `number' counts from the beginning (variable is false), while in
    !> zsh, it counts from the end.  Port of comint.el
    !> comint-insert-previous-argument-from-end.
    type(logical_setting), target, public :: comint_insert_previous_argument_from_end = &
                                             logical_setting(name="comint_insert_previous_argument_from_end", &
                                                             value=.false.)

    !> Values of `comint_history_isearch'.
    integer, parameter, public :: COMINT_HISTORY_ISEARCH_NIL = 0, COMINT_HISTORY_ISEARCH_DWIM = 1, &
                                  COMINT_HISTORY_ISEARCH_T = 2

    !> If not COMINT_HISTORY_ISEARCH_NIL, Isearch in input history only,
    !> not in comint buffer output.  If COMINT_HISTORY_ISEARCH_T, usual
    !> Isearch keys like \`C-r' and \`C-M-r' in comint mode search in the
    !> input history.  If COMINT_HISTORY_ISEARCH_DWIM, Isearch keys search
    !> in the input history only when initial point position is at the
    !> comint command line.  When starting Isearch from other parts of the
    !> comint buffer, they search in the comint buffer.  If
    !> COMINT_HISTORY_ISEARCH_NIL, Isearch operates on the whole comint
    !> buffer.  Port of comint.el comint-history-isearch.
    type(integer_setting), target, public :: comint_history_isearch = &
                                             integer_setting(name="comint_history_isearch", value=COMINT_HISTORY_ISEARCH_NIL)

    !> Value to use for TERM when the system uses terminfo.
    !> If the system's terminfo database contains a definition for the
    !> "dumb-emacs-ansi" terminal (as all recent versions of terminfo do),
    !> set this to "dumb-emacs-ansi" and then some terminfo-aware programs
    !> will send colorized output when run under Comint.  Port of comint.el
    !> comint-terminfo-terminal.
    type(string_setting), target, public :: comint_terminfo_terminal = &
                                            string_setting(name="comint_terminfo_terminal")

    !> If false, Comint will interpret `carriage control' characters in
    !> output.  See `comint_carriage_motion' for details.  Port of
    !> comint.el comint-inhibit-carriage-motion.
    type(logical_setting), target, public :: comint_inhibit_carriage_motion = &
                                             logical_setting(name="comint_inhibit_carriage_motion", value=.false.)

    !> The characters to recognize as separate arguments in input.
    !> Strings comprising a character in this list will separate the
    !> arguments surrounding them, and also be regarded as arguments in
    !> their own right (unlike whitespace).  See `comint_arguments'.
    !> Defaults to the empty string.
    !>
    !> For shells, a good value is "|&<>();".
    !>
    !> This is a good thing to set in mode hooks.  Port of comint.el
    !> comint-delimiter-argument-list (the characters as a string).
    type(string_setting), target, public :: comint_delimiter_argument_list = &
                                            string_setting(name="comint_delimiter_argument_list")

    !> True directs the `comint_input_sender' function not to send a
    !> newline.  Port of comint.el comint-input-sender-no-newline.
    logical, public :: comint_input_sender_no_newline = .false.

    !> Hook run upon entry to `comint_mode'.
    !> This is run before the process is cranked up.  Port of comint.el
    !> comint-mode-hook.
    type(hook_type), target, public :: comint_mode_hook = hook_type(name="comint_mode_hook")

    !> Hook run each time a process is exec'd by `comint_exec'.
    !> This is called after the process is cranked up.  It is useful for
    !> things that must be done each time a process is executed in a
    !> Comint mode buffer (e.g., `set_process_query_on_exit_flag').  In
    !> contrast, `comint_mode_hook' is only executed once, when the buffer
    !> is created.  Port of comint.el comint-exec-hook.
    type(hook_type), target, public :: comint_exec_hook = hook_type(name="comint_exec_hook")

    !> Functions to call after output is inserted into the buffer.
    !> One possible function is `comint_postoutput_scroll_to_bottom'.
    !> These functions get one argument, a string containing the text as
    !> originally inserted.  Note that this might not be the same as the
    !> buffer contents between `comint_last_output_start' and the buffer's
    !> `process_mark', if other filter functions have already modified the
    !> buffer.
    !>
    !> See also `comint_preoutput_filter_functions'.
    !>
    !> You can use `add' to add functions to this hook either globally or
    !> locally.  Port of comint.el comint-output-filter-functions
    !> (ansi_color_fm adds `ansi_color_process_output').
    type(string_hook_type), target, public :: comint_output_filter_functions = &
                                              string_hook_type(name="comint_output_filter_functions", &
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
    type(string_filter_hook_type), target, public :: comint_preoutput_filter_functions = &
                                                     string_filter_hook_type(name="comint_preoutput_filter_functions", &
                                                                             permanent_local=.true.)

    !> Abnormal hook run before input is sent to the process.
    !> These functions get one argument, a string containing the text to
    !> send.  Port of comint.el comint-input-filter-functions.
    type(string_hook_type), target, public :: comint_input_filter_functions = &
                                              string_hook_type(name="comint_input_filter_functions", &
                                                               permanent_local=.true.)

    !> Keymap for Comint mode.  Port of comint.el comint-mode-map (the
    !> keys of ported commands).
    type(handle_type), public :: comint_mode_map

    !> Keymap to repeat comint key sequences.  Used in `repeat_mode'.
    !> Port of comint.el comint-repeat-map.
    type(handle_type), public :: comint_repeat_map

    !> Face to use to highlight user input: the face `comint_highlight_input'
    !> (the variable of that name is the setting).  Port of
    !> comint.el:comint-highlight-input (the face).
    type(named_face_type), target, public :: comint_highlight_input_face

    !> Face to use to highlight prompts.  Port of
    !> comint.el:comint-highlight-prompt.
    type(named_face_type), target, public :: comint_highlight_prompt

    !> A Comint buffer's own state: upstream's buffer-local variables
    !> comint-input-ring, comint-input-ring-index,
    !> comint-save-input-ring-index, comint-ptyp (permanent), and
    !> comint-last-input-start, comint-last-input-end,
    !> comint-last-output-start, comint-accum-marker, comint-last-prompt,
    !> comint-stored-incomplete-input,
    !> comint-matching-input-from-input-string, comint-get-old-input,
    !> comint-input-sender and comint-input-filter.
    type :: comint_buffer_type
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
        procedure(comint_get_old_input_proc), pointer, nopass :: get_old_input => null()
        procedure(comint_input_sender_proc), pointer, nopass :: input_sender => null()
        procedure(comint_input_filter_proc), pointer, nopass :: input_filter => null()
        !> comint-insert-previous-argument-last-start-pos and
        !> comint-insert-previous-argument-last-index.
        type(handle_type) :: insert_previous_argument_last_start_pos
        integer :: insert_previous_argument_last_index = 0
        logical :: has_insert_previous_argument_last_index = .false.
        !> comint-history-isearch-message-overlay.
        type(handle_type) :: history_isearch_message_overlay
        !> comint-password-function.
        procedure(comint_password_proc), pointer, nopass :: password_function => null()
        !> comint-completion-fignore.
        character(len=8), allocatable :: completion_fignore(:)
        !> comint-unquote-function and comint-requote-function (null for the
        !> default values).
        procedure(completion_unquote_proc), pointer, nopass :: unquote => null()
        procedure(completion_requote_proc), pointer, nopass :: requote => null()
    end type comint_buffer_type

    !> Regexp matching prompts for passwords in the inferior process.
    !> This is used by `comint_watch_for_password_prompt'.  Port of
    !> comint.el comint-password-prompt-regexp (made by
    !> `comint_password_prompt_regexp_default' when Comint is set up).
    character(len=:), allocatable, public :: comint_password_prompt_regexp

    !> The maximum amount of text to examine when matching password
    !> prompts.  This is used by `comint_watch_for_password_prompt' to
    !> reduce the amount of time spent searching for password prompts.
    !> Port of comint.el comint-password-prompt-max-length.
    integer, public :: comint_password_prompt_max_length = 256

    ! comint--prompt-recursion-depth.
    integer :: comint_prompt_recursion_depth = 0

    ! A password prompt waiting for the timer `comint_watch_for_password_prompt'
    ! starts (the arguments of its lambda).
    type :: pending_prompt_type
        type(handle_type) :: buffer
        character(len=:), allocatable :: prompt
    end type pending_prompt_type

    type(pending_prompt_type), allocatable :: pending_prompts(:)

    !> True means to force searching in input history.
    !> If false, respect the option `comint_history_isearch'.  Port of
    !> comint.el comint--force-history-isearch.
    logical :: comint_force_history_isearch = .false.

    ! What comint-replace-by-expanded-history-before-point throws to
    ! DRY-RUN: nothing, `message', `history' or `expand'.
    integer, parameter :: DRY_RUN_NONE = 0, DRY_RUN_MESSAGE = 1, DRY_RUN_HISTORY = 2, DRY_RUN_EXPAND = 3

    type(comint_buffer_type), allocatable :: comint_buffers(:)

    !> comint--prompt-rear-nonsticky: text properties set on the prompt
    !> that are not to leak past it.
    character(len=*), parameter :: PROMPT_REAR_NONSTICKY(5) = [character(len=31) :: "field", &
                                                                                   "inhibit_line_move_field_capture", "read_only", &
                                                                                          "font_lock_face", "insert_in_front_hooks"]

    !> If true, automatically list possibilities on partial completion.
    !> This mirrors the optional behavior of tcsh.  Port of comint.el
    !> comint-completion-autolist.
    type(logical_setting), target, public :: comint_completion_autolist = &
                                             logical_setting(name="comint_completion_autolist", value=.false.)

    !> The string FILESUFFIX added to file names on unambiguous or exact
    !> completion (" " by default; the empty string adds none).
    !> This mirrors the optional behavior of tcsh.  Port of comint.el
    !> comint-completion-addsuffix (.true. is " ", .false. is "", and the obsolete
    !> two values, DIRSUFFIX and FILESUFFIX, give FILESUFFIX).
    type(string_setting), target, public :: comint_completion_addsuffix = &
                                            string_setting(name="comint_completion_addsuffix")

    !> If true, use shortest completion if characters cannot be added.
    !> This mirrors the optional behavior of tcsh.
    !>
    !> A true value is useful if `comint_completion_autolist' is true too.
    !> Port of comint.el comint-completion-recexact.
    type(logical_setting), target, public :: comint_completion_recexact = &
                                             logical_setting(name="comint_completion_recexact", value=.false.)

    !> List of suffixes to be disregarded during file completion.
    !> This mirrors the optional behavior of bash and tcsh.
    !>
    !> Note that this applies to `comint_dynamic_complete_filename' only.
    !> Port of comint.el comint-completion-fignore (at most as many
    !> suffixes as `completion_ignored_extensions' has entries; a Comint
    !> buffer's own value is set with `set_comint_completion_fignore').
    character(len=8), allocatable, public :: comint_completion_fignore(:)

    !> Prefix prepended to absolute file names taken from process input.
    !> This is used by Comint's and shell's completion functions, and by
    !> shell's directory tracking functions.  Port of comint.el
    !> comint-file-name-prefix.
    type(string_setting), target, public :: comint_file_name_prefix = string_setting(name="comint_file_name_prefix")

    !> String of characters valid in a file name.
    !> Note that all non-ASCII characters are considered valid in a file
    !> name regardless of what this variable says.
    !>
    !> This is a good thing to set in mode hooks.  Port of comint.el
    !> comint-file-name-chars.
    type(string_setting), target, public :: comint_file_name_chars = string_setting(name="comint_file_name_chars")

    !> The characters to quote with `\' when in a file name.
    !>
    !> This is a good thing to set in mode hooks.  Port of comint.el
    !> comint-file-name-quote-list (a string of the characters).
    type(string_setting), target, public :: comint_file_name_quote_list = &
                                            string_setting(name="comint_file_name_quote_list")

    !> List of functions called to perform completion.
    !> Works like `completion_at_point_functions'.
    !> See also `completion_at_point'.
    !>
    !> This is a good thing to set in mode hooks.  Port of comint.el
    !> comint-dynamic-complete-functions.
    type(capf_hook_type), target, public :: comint_dynamic_complete_functions = &
                                            capf_hook_type(name="comint_dynamic_complete_functions")

    !> Function to use for completion of quoted data.
    !> See `completion_table_with_quoting' and `comint_requote_function'.
    !> Port of comint.el comint-unquote-function.
    procedure(completion_unquote_proc), pointer, public :: comint_unquote_function => comint_unquote_argument

    !> Function to use for completion of quoted data.
    !> See `completion_table_with_quoting' and `comint_unquote_function'.
    !> Port of comint.el comint-requote-function.
    procedure(completion_requote_proc), pointer, public :: comint_requote_function => comint_requote_argument

contains

    ! ------------------------------------------------------------------
    ! The buffers' own state
    ! ------------------------------------------------------------------

    !> The entry of buffer slot BI (0 for none).  Not a port.
    integer function entry_of(bi) result(k)
        integer(int64), intent(in) :: bi

        if (allocated(comint_buffers) .and. bi > 0) then
            do k = 1, size(comint_buffers)
                if (comint_buffers(k)%serial == buffers(bi)%serial) return
            end do
        end if

        k = 0
    end function entry_of

    !> The entry of the current buffer, made (empty) if needed.  Entries of
    !> dead buffers are dropped then.  Not a port.
    integer function current_entry() result(k)
        type(comint_buffer_type), allocatable :: kept(:)
        integer :: i, n

        k = entry_of(cur)
        if (k /= 0) return
        if (.not. allocated(comint_buffers)) allocate (comint_buffers(0))
        n = 0
        allocate (kept(size(comint_buffers) + 1))

        do i = 1, size(comint_buffers)
            if (buffer_live_p(comint_buffers(i)%buffer)) then
                n = n + 1
                call move_entry(comint_buffers(i), kept(n))
            else
                call free_locals(comint_buffers(i))
            end if
        end do

        n = n + 1
        kept(n)%serial = buffers(cur)%serial
        kept(n)%buffer = current_buffer()
        call move_alloc(kept, comint_buffers)
        comint_buffers = comint_buffers(1:n)
        k = n
    end function current_entry

    ! Not a port.
    subroutine move_entry(from, to)
        type(comint_buffer_type), intent(inout) :: from
        type(comint_buffer_type), intent(out) :: to

        to = from
    end subroutine move_entry

    !> Free the markers of entry E and forget its local variables.  Not a port.
    subroutine free_locals(e)
        type(comint_buffer_type), intent(inout) :: e
        type(status_type) :: st

        if (.not. e%local) return
        call free_marker(e%last_input_start, st)
        call free_marker(e%last_input_end, st)
        call free_marker(e%last_output_start, st)
        call free_marker(e%accum_marker, st)
        call forget_last_prompt(e)
        if (allocated(e%stored_incomplete_input)) deallocate (e%stored_incomplete_input)
        e%matching_input_from_input_string = ""
        e%get_old_input => null()
        e%input_sender => null()
        e%input_filter => null()

        if (e%insert_previous_argument_last_start_pos%index /= 0) then
            call free_marker(e%insert_previous_argument_last_start_pos, st)
        end if

        e%insert_previous_argument_last_start_pos = null_handle()
        e%has_insert_previous_argument_last_index = .false.
        e%history_isearch_message_overlay = null_handle()
        if (allocated(e%completion_fignore)) deallocate (e%completion_fignore)
        e%unquote => null()
        e%requote => null()
        e%password_function => null()
        e%local = .false.
    end subroutine free_locals

    ! Not a port.
    subroutine forget_last_prompt(e)
        type(comint_buffer_type), intent(inout) :: e
        type(status_type) :: st

        if (.not. e%has_last_prompt) return
        call free_marker(e%last_prompt_start, st)
        call free_marker(e%last_prompt_end, st)
        e%has_last_prompt = .false.
    end subroutine forget_last_prompt

    !> Forget the current buffer's local Comint variables (not the
    !> permanent ones), as `kill_all_local_variables' does.  Not a port.
    subroutine kill_comint_locals()
        integer :: k

        k = entry_of(cur)
        if (k /= 0) call free_locals(comint_buffers(k))
    end subroutine kill_comint_locals

    !> The current buffer's entry with its local variables (made as
    !> comint-mode makes them if the buffer has none).  Not a port.
    integer function local_entry() result(k)
        k = current_entry()
        if (.not. comint_buffers(k)%local) call make_locals(k)
    end function local_entry

    !> Make the local variables of entry K, as comint-mode does.  Not a port.
    subroutine make_locals(k)
        integer, intent(in) :: k
        type(status_type) :: st

        associate (e => comint_buffers(k))
            call copy_marker_at(point_min(), e%last_input_start, .false., st)
            call copy_marker_at(point_min(), e%last_input_end, .false., st)
            call make_marker(e%last_output_start)
            call make_marker(e%accum_marker)
            e%has_last_prompt = .false.
            e%matching_input_from_input_string = ""
            e%get_old_input => comint_get_old_input_default
            e%input_sender => comint_simple_send

            if (allocated(comint_completion_fignore)) then
                e%completion_fignore = comint_completion_fignore
            else
                allocate (e%completion_fignore(0))
            end if

            e%input_filter => comint_nonblank_p
            e%local = .true.
        end associate
    end subroutine make_locals

    !> Whether the current buffer has a local input ring (comint-input-ring).
    !> Not a port.
    logical function has_input_ring()
        integer :: k

        has_input_ring = .false.
        k = entry_of(cur)
        if (k /= 0) has_input_ring = comint_buffers(k)%has_ring
    end function has_input_ring

    !> The number of items of the current buffer's input ring.  Not a port.
    integer function comint_input_ring_length()
        integer :: k

        comint_input_ring_length = 0
        k = entry_of(cur)
        if (k == 0) return
        if (comint_buffers(k)%has_ring) comint_input_ring_length = comint_buffers(k)%input_ring%count()
    end function comint_input_ring_length

    !> Item INDEX (0 the newest) of the current buffer's input ring ("" if
    !> it has none).  Not a port.
    function comint_input_ring_item(index) result(item)
        integer, intent(in) :: index
        character(len=:), allocatable :: item
        type(status_type) :: st
        integer :: k

        item = ""
        k = entry_of(cur)
        if (k == 0) return
        if (comint_buffers(k)%has_ring) call comint_buffers(k)%input_ring%ref(index, item, st)
    end function comint_input_ring_item

    !> Position of a marker of the current buffer's state (0 if unset).
    !> Not a port.
    integer(int64) function marker_pos(m) result(pos)
        type(handle_type), intent(in) :: m
        type(status_type) :: st

        pos = 0
        if (m%index == 0) return
        call marker_position(m, pos, st)
        if (.not. st%ok()) pos = 0
    end function marker_pos

    !> The position of comint-last-input-start in the current buffer.
    !> Not a port.
    integer(int64) function comint_last_input_start()
        integer :: k

        comint_last_input_start = 0
        k = entry_of(cur)
        if (k /= 0) comint_last_input_start = marker_pos(comint_buffers(k)%last_input_start)
    end function comint_last_input_start

    !> The position of comint-last-input-end in the current buffer.
    !> Not a port.
    integer(int64) function comint_last_input_end()
        integer :: k

        comint_last_input_end = 0
        k = entry_of(cur)
        if (k /= 0) comint_last_input_end = marker_pos(comint_buffers(k)%last_input_end)
    end function comint_last_input_end

    !> The position of comint-last-output-start in the current buffer.
    !> Not a port.
    integer(int64) function comint_last_output_start()
        integer :: k

        comint_last_output_start = 0
        k = entry_of(cur)
        if (k /= 0) comint_last_output_start = marker_pos(comint_buffers(k)%last_output_start)
    end function comint_last_output_start

    !> True if communications via pty; false if by pipe.  Buffer local.
    !> This is to work around a bug in Emacs process signaling.  Port of
    !> comint.el comint-ptyp.
    logical function comint_ptyp()
        integer :: k

        comint_ptyp = .true.
        k = entry_of(cur)
        if (k /= 0) comint_ptyp = comint_buffers(k)%ptyp
    end function comint_ptyp

    !> Give the current buffer its own `comint_get_old_input': the
    !> function that returns old text in Comint mode.
    !> This function is called when return is typed while the point is in
    !> old text.  It returns the text to be submitted as process input.
    !> The default is `comint_get_old_input_default', which either grabs
    !> the current input field or grabs the current line and strips off
    !> leading text matching `comint_prompt_regexp', depending on the value
    !> of `comint_use_prompt_regexp'.  Port of the buffer-local setting of comint.el
    !> comint-get-old-input.
    subroutine set_comint_get_old_input(fn)
        procedure(comint_get_old_input_proc) :: fn
        integer :: k

        if (cur == 0) return
        k = local_entry()
        comint_buffers(k)%get_old_input => fn
    end subroutine set_comint_get_old_input

    !> Give the current buffer its own `comint_input_sender': the
    !> function to actually send to PROCESS the STRING submitted by user.
    !> Usually this is just `comint_simple_send', but if your mode needs
    !> to massage the input string, put a different function here.
    !> `comint_simple_send' just sends the string plus a newline.
    !> (If `comint_input_sender_no_newline' is true, it omits the newline.)
    !> This is called from the user command `comint_send_input'.  Port of
    !> the buffer-local setting of comint.el comint-input-sender.
    subroutine set_comint_input_sender(fn)
        procedure(comint_input_sender_proc) :: fn
        integer :: k

        if (cur == 0) return
        k = local_entry()
        comint_buffers(k)%input_sender => fn
    end subroutine set_comint_input_sender

    !> Give the current buffer its own `comint_input_filter': the
    !> predicate for filtering additions to input history.
    !> Takes one argument, the input.  If true, the input may be saved on
    !> the input history list.  Default is to save anything that isn't all
    !> whitespace.  Port of the buffer-local setting of comint.el comint-input-filter.
    subroutine set_comint_input_filter(fn)
        procedure(comint_input_filter_proc) :: fn
        integer :: k

        if (cur == 0) return
        k = local_entry()
        comint_buffers(k)%input_filter => fn
    end subroutine set_comint_input_filter

    !> Abnormal hook run when prompted for a password.
    !> This function gets one argument, a string containing the prompt.
    !> It may return a string containing the password, or unallocated if normal
    !> password prompting should occur.  Sets the current buffer's
    !> comint.el comint-password-function (buffer-local); without FN, null.
    !> Not a port.
    subroutine set_comint_password_function(fn)
        procedure(comint_password_proc), optional :: fn
        integer :: k

        if (cur == 0) return
        k = local_entry()

        if (present(fn)) then
            comint_buffers(k)%password_function => fn
        else
            comint_buffers(k)%password_function => null()
        end if
    end subroutine set_comint_password_function

    ! ------------------------------------------------------------------
    ! Small helpers
    ! ------------------------------------------------------------------

    !> Bind `inhibit_read_only' to true, keeping its value in SAVED.
    !> Not a port.
    subroutine inhibit_read_only_begin(saved)
        type(value_type), intent(out) :: saved
        type(status_type) :: st

        call symbol_value(sym("inhibit_read_only"), saved, st)
        call set(sym("inhibit_read_only"), t_value(), st)
    end subroutine inhibit_read_only_begin

    !> End the binding of `inhibit_read_only_begin'.  Not a port.
    subroutine inhibit_read_only_end(saved)
        type(value_type), intent(in) :: saved
        type(status_type) :: st

        call set(sym("inhibit_read_only"), saved, st)
    end subroutine inhibit_read_only_end

    !> The current buffer's process, or an error: "Current buffer has no
    !> process".  Not a port.
    subroutine current_process(proc, status)
        type(handle_type), intent(out) :: proc
        type(status_type), intent(out) :: status

        status = status_type()
        proc = get_buffer_process(current_buffer())
        if (proc%index == 0) call set_error(status, FM_USER_ERROR, "Current buffer has no process")
    end subroutine current_process

    !> The position of the process mark of PROC (0 if it points nowhere).
    !> Not a port.
    integer(int64) function pmark_pos(proc)
        type(handle_type), intent(in) :: proc

        pmark_pos = marker_pos(process_mark(proc))
    end function pmark_pos

    !> The text properties (`rear_nonsticky' with `comint__prompt_rear_nonsticky').
    !> Not a port.
    function rear_nonsticky_plist() result(v)
        type(value_type) :: v

        v = list(sym("rear_nonsticky"), list(sym(trim(PROMPT_REAR_NONSTICKY(1))), &
                                             sym(trim(PROMPT_REAR_NONSTICKY(2))), &
                                             sym(trim(PROMPT_REAR_NONSTICKY(3))), &
                                             sym(trim(PROMPT_REAR_NONSTICKY(4))), &
                                             sym(trim(PROMPT_REAR_NONSTICKY(5)))))
    end function rear_nonsticky_plist

    !> Whether the name V is named NAME.  Not a port.
    logical function symbol_is(v, name)
        type(value_type), intent(in) :: v
        character(len=*), intent(in) :: name

        symbol_is = .false.
        if (.not. symbolp(v)) return
        if (nilp(v)) return
        symbol_is = symbols(xsymbol(v))%name == name
    end function symbol_is

    !> The `field' property of the character at POS (get-char-property).
    !> Not a port.
    function field_at(pos) result(v)
        integer(int64), intent(in) :: pos
        type(value_type) :: v
        type(status_type) :: st

        call get_text_property(pos, sym("field"), v, st)
    end function field_at

    ! ------------------------------------------------------------------
    ! The mode
    ! ------------------------------------------------------------------

    !> Major mode for interacting with an inferior interpreter.
    !> Interpreter name is same as buffer name, sans the asterisks.
    !> Return at end of buffer sends line as input.
    !> Return not at end copies rest of line to end and sends it.
    !> Setting variable `comint_eol_on_send' means jump to the end of the
    !> line before submitting new input.
    !>
    !> This mode is customized to create major modes such as Inferior Lisp
    !> mode, Shell mode, etc.  This can be done by setting the hooks
    !> `comint_input_filter_functions', `comint_input_filter',
    !> `comint_input_sender' and `comint_get_old_input' to appropriate
    !> functions, and the variable `comint_prompt_regexp' to the
    !> appropriate regular expression.
    !>
    !> The mode maintains an input history of size `comint_input_ring_size'.
    !> You can access this with the commands \[comint_next_input] and
    !> \[comint_previous_input].
    !> Addition is controlled by the variable `comint_input_ignoredups'.
    !>
    !> Input to, and output from, the subprocess can cause the window to
    !> scroll to the end of the buffer.  See variables
    !> `comint_output_filter_functions', `comint_preoutput_filter_functions'
    !> and `comint_move_point_for_output'.
    !>
    !> Entry to this mode runs the hooks on `comint_mode_hook'.  Port of
    !> comint.el:comint-mode.
    recursive subroutine comint_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        if (.not. associated(major_modes%comint_mode%run)) call syms_of_comint()
        call major_modes%comint_mode%begin(saved, status)
        if (.not. status%ok()) return
        call comint_mode_body()
        call major_modes%comint_mode%finish(saved, status)
    end subroutine comint_mode

    !> The body of comint-mode (also run by the modes derived from it,
    !> through their parent).  Port of comint.el:comint-mode (its body).
    subroutine comint_mode_body()
        type(mode_line_part_type) :: parts(1)
        integer :: k

        parts(1)%text = ":%s"
        parts(1)%face = ""
        call set_mode_line_process(parts)
        k = current_entry()
        call make_locals(k)
        call comint_prompt_regexp%make_local()
        call comint_delimiter_argument_list%make_local()
        call comint_input_ring_size%make_local()
        call comint_input_ring_file_name%make_local()
        call comint_input_autoexpand%make_local()
        call comint_input_ignoredups%make_local()
        call comint_eol_on_send%make_local()
        call comint_scroll_to_bottom_on_input%make_local()
        call comint_move_point_for_output%make_local()
        call comint_scroll_show_maximum_output%make_local()
        call comint_process_echoes%make_local()
        k = current_entry()

        if (.not. comint_buffers(k)%has_ring) then
            call comint_buffers(k)%input_ring%make(int(comint_input_ring_size%value))
            comint_buffers(k)%has_ring = .true.
        end if

        call pre_command_hook%add(comint_preinput_scroll_to_bottom, depth=90, local=.true.)
        call font_lock_defaults%set_local(keywords_only=.true.)
        call comint_file_name_chars%make_local()
        call comint_file_name_quote_list%make_local()
        ! dir tracking on remote files
        call comint_file_name_prefix%set_local("")
        call isearch_mode_hook%add(comint_history_isearch_setup, local=.true.)
        call completion_at_point_functions%add(comint_completion_at_point, local=.true.)
    end subroutine comint_mode_body

    !> Return true if there is a living process associated with BUFFER.
    !> Living means the status is `run' (`open' and `stop' are not
    !> ported).  Port of comint.el:comint-check-proc.
    logical function comint_check_proc(buffer)
        type(handle_type), intent(in) :: buffer
        type(handle_type) :: proc

        comint_check_proc = .false.
        proc = get_buffer_process(buffer)
        if (proc%index /= 0) comint_check_proc = process_status(proc) == "run"
    end function comint_check_proc

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
    recursive subroutine make_comint_in_buffer(name, program, result, status, buffer_name, startfile, switches)
        character(len=*), intent(in) :: name, program
        type(handle_type), intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: buffer_name, startfile
        type(process_arg_type), intent(in), optional :: switches(:)
        type(handle_type) :: old
        type(status_type) :: st
        type(process_arg_type), allocatable :: args(:)

        if (present(buffer_name)) then
            call get_buffer_create(buffer_name, result, status)
        else
            call get_buffer_create("*"//name//"*", result, status)
        end if

        if (.not. status%ok()) return
        ! If no process, or nuked process, crank up a new one and put
        ! buffer in comint mode.  Otherwise, leave buffer and existing
        ! process alone.
        if (comint_check_proc(result)) return
        old = current_buffer()
        call set_buffer(result, status)
        if (.not. status%ok()) return
        if (.not. derived_from_comint()) call comint_mode(status)
        if (buffer_live_p(old)) call set_buffer(old, st)
        if (.not. status%ok()) return

        if (present(switches)) then
            args = switches
        else
            allocate (args(0))
        end if

        call comint_exec(result, name, program, args, status, startfile)
    end subroutine make_comint_in_buffer

    !> Whether the current buffer's major mode is derived from Comint mode.
    !> Not a port.
    logical function derived_from_comint()
        use derived_fm, only: derived_mode_p

        derived_from_comint = derived_mode_p("comint_mode")
    end function derived_from_comint

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
    recursive subroutine make_comint(name, program, result, status, startfile, switches)
        character(len=*), intent(in) :: name, program
        type(handle_type), intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: startfile
        type(process_arg_type), intent(in), optional :: switches(:)

        call make_comint_in_buffer(name, program, result, status, startfile=startfile, switches=switches)
    end subroutine make_comint

    !> Run PROGRAM in a Comint buffer and switch to that buffer.
    !>
    !> If SWITCHES are supplied, they are passed to PROGRAM.
    !>
    !> The buffer name is made by surrounding the file name of PROGRAM with
    !> `*'s.
    !>
    !> See `make_comint' and `comint_exec'.  Port of comint.el:comint-run
    !> (the hooks `comint-NAME-hook' are not applicable: hooks are named
    !> objects).
    recursive subroutine comint_run(program, status, switches)
        use fileio_fm, only: file_name_nondirectory
        use window_fm, only: switch_to_buffer
        character(len=*), intent(in) :: program
        type(status_type), intent(out) :: status
        type(process_arg_type), intent(in), optional :: switches(:)
        type(handle_type) :: buf
        character(len=:), allocatable :: name

        name = file_name_nondirectory(program)
        call make_comint(name, program, buf, status, switches=switches)
        if (.not. status%ok()) return
        call switch_to_buffer(buffers(buf%index)%name, status)
    end subroutine comint_run

    !> The command: read PROGRAM and run it.  The interactive form of
    !> comint.el:comint-run.
    !>
    !> Run PROGRAM in a Comint buffer and switch to that buffer.
    recursive subroutine cmd_comint_run(status)
        character(len=:), allocatable :: program
        type(status_type), intent(out) :: status

        call read_string("Run program: ", program, status)
        if (.not. status%ok()) return

        if (consp(current_prefix_arg)) then
            call set_error(status, FM_GENERIC_ERROR, "Reading switches for comint_run is not supported yet")
            return
        end if

        call comint_run(program, status)
    end subroutine cmd_comint_run

    !> Start up a process named NAME in buffer BUFFER for Comint modes.
    !> Runs the given COMMAND with SWITCHES, and initial input from
    !> STARTFILE.
    !>
    !> This function blasts any old process running in the buffer, and
    !> does not set the buffer mode.  You can use this to cheaply run a
    !> series of processes in the same Comint buffer.  The hook
    !> `comint_exec_hook' is run after each exec.  ENVIRONMENT, if
    !> present, are "NAME=VALUE" entries put after Comint's own in the
    !> process's environment (as binding `process_environment' around the
    !> call).  Port of comint.el:comint-exec (COMMAND a program).
    recursive subroutine comint_exec(buffer, name, command, switches, status, startfile, environment)
        type(handle_type), intent(in) :: buffer
        character(len=*), intent(in) :: name, command
        type(process_arg_type), intent(in) :: switches(:)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: startfile
        type(process_arg_type), intent(in), optional :: environment(:)
        type(handle_type) :: old, proc
        type(status_type) :: st
        character(len=:), allocatable :: text
        integer :: k

        old = current_buffer()
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        ! Blast any old process.
        proc = get_buffer_process(buffer)
        if (proc%index /= 0) call delete_process(proc, st)
        ! Crank up a new process.
        call comint_exec_1(name, buffer, command, switches, proc, status, environment)

        if (.not. status%ok()) then
            if (buffer_live_p(old)) call set_buffer(old, st)
            return
        end if

        call set_process_filter(proc, st, comint_output_filter)
        ! t if pty, nil if pipe.
        k = current_entry()
        comint_buffers(k)%ptyp = process_connection_type%value
        ! Jump to the end, and set the process mark.
        call goto_char(point_max())
        call set_marker(process_mark(proc), point(), st)

        ! Feed it the startfile.
        if (present(startfile)) then
            call temp_buffer_file_contents(startfile, text, status)
            if (.not. status%ok()) return
            call comint_send_string(proc, text, status)
            if (.not. status%ok()) return
        end if

        call comint_exec_hook%run(status)
        if (buffer_live_p(old)) call set_buffer(old, st)
    end subroutine comint_exec

    !> The contents of FILE (with-temp-buffer, insert-file-contents,
    !> buffer-string).  Not a port.
    subroutine temp_buffer_file_contents(file, text, status)
        use buffer_fm, only: kill_buffer, generate_new_buffer_name
        use editfns_fm, only: buffer_string
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: text
        type(status_type), intent(out) :: status
        type(handle_type) :: old, tmp
        type(status_type) :: st
        integer(int64) :: inserted

        text = ""
        old = current_buffer()
        call get_buffer_create(generate_new_buffer_name(" *temp*"), tmp, status)
        if (.not. status%ok()) return
        call set_buffer(tmp, st)
        call insert_file_contents(file, .false., inserted, status)
        if (status%ok()) text = buffer_string()
        call set_buffer(old, st)
        call kill_buffer(tmp, st)
    end subroutine temp_buffer_file_contents

    !> Crank up the process for comint-exec in the appropriate environment.
    !> Port of comint.el:comint-exec-1.
    recursive subroutine comint_exec_1(name, buffer, command, switches, proc, status, environment)
        character(len=*), intent(in) :: name, command
        type(handle_type), intent(in) :: buffer
        type(process_arg_type), intent(in) :: switches(:)
        type(handle_type), intent(out) :: proc
        type(status_type), intent(out) :: status
        type(process_arg_type), intent(in), optional :: environment(:)
        type(process_arg_type), allocatable :: env(:), argv(:)
        character(len=:), allocatable :: saved_dir
        logical :: had_dir
        integer :: k

        call comint_term_environment(env)
        call append_arg(env, "INSIDE_EMACS="//upstream_version//",comint")
        if (allocated(comint_pager%value)) call append_arg(env, "PAGER="//comint_pager%value)

        if (present(environment)) then
            do k = 1, size(environment)
                call append_arg(env, environment(k)%s)
            end do
        end if

        had_dir = allocated(buffers(cur)%directory)
        if (had_dir) saved_dir = buffers(cur)%directory

        if (had_dir) then
            if (.not. file_accessible_directory_p(saved_dir)) buffers(cur)%directory = "/"
        else
            buffers(cur)%directory = "/"
        end if

        allocate (argv(size(switches) + 1))
        argv(1)%s = command

        do k = 1, size(switches)
            argv(k + 1)%s = switches(k)%s
        end do

        call make_process(name, argv, proc, status, buffer=buffer, environment=env)

        if (had_dir) then
            buffers(cur)%directory = saved_dir
        else
            deallocate (buffers(cur)%directory)
        end if
        ! (Coding systems are not ported.)
    end subroutine comint_exec_1

    !> Return an environment variable list for terminal configuration.
    !> Port of comint.el:comint-term-environment (a system using
    !> terminfo).
    subroutine comint_term_environment(env)
        type(process_arg_type), allocatable, intent(out) :: env(:)
        character(len=16) :: cols
        integer :: width

        width = 80
        if (selected_frame%selected_window%index /= 0) width = window_text_cols(windows(selected_frame%selected_window%index))
        write (cols, "(i0)") width
        allocate (env(3))
        env(1)%s = "TERM="//comint_terminfo_terminal%value
        env(2)%s = "TERMCAP="
        env(3)%s = "COLUMNS="//trim(cols)
    end subroutine comint_term_environment

    !> Return true if STR contains non-whitespace syntax.  Port of
    !> comint.el:comint-nonblank-p.
    logical function comint_nonblank_p(str)
        character(len=*), intent(in) :: str
        type(status_type) :: st

        comint_nonblank_p = string_match("\`\s *\'", str, st, inhibit_modify=.true.) == 0
    end function comint_nonblank_p

    ! ------------------------------------------------------------------
    ! The input history
    ! ------------------------------------------------------------------

    !> Set the buffer's `comint_input_ring' from a history file.
    !> The name of the file is given by the variable
    !> `comint_input_ring_file_name'.
    !> The history ring is of size `comint_input_ring_size', regardless of
    !> file size.  If `comint_input_ring_file_name' is unallocated this
    !> function does nothing.
    !>
    !> If the optional argument SILENT is true, we say nothing about a
    !> failure to read the history file.
    !>
    !> This function is useful for major mode commands and mode hooks.
    !>
    !> The commands stored in the history file are separated by the
    !> `comint_input_ring_separator', and entries that match
    !> `comint_input_history_ignore' are ignored.  The most recent command
    !> comes last.
    !>
    !> See also `comint_input_ignoredups' and `comint_write_input_ring'.
    !> Port of comint.el:comint-read-input-ring.
    subroutine comint_read_input_ring(silent)
        use buffer_fm, only: kill_buffer, generate_new_buffer_name
        logical, intent(in), optional :: silent
        type(ring_type) :: ring
        type(handle_type) :: old, tmp
        type(status_type) :: st
        character(len=:), allocatable :: file, separator, prefix, ignore, history, last
        integer(int64) :: inserted, found, start, end
        integer :: count, ring_size, ring_max_size, k
        logical :: quiet, ignoredups, have_prefix, keep

        quiet = .false.
        if (present(silent)) quiet = silent
        if (.not. allocated(comint_input_ring_file_name%value)) return
        if (len(comint_input_ring_file_name%value) == 0) return
        file = comint_input_ring_file_name%value

        if (.not. file_readable_p(file)) then
            if (.not. quiet) call message("Cannot read history file "//file)
            return
        end if

        count = 0
        ! Some users set HISTSIZE or `comint-input-ring-size' to huge
        ! numbers.  Don't allocate a huge ring right away; there might not
        ! be that much history.
        ring_size = int(min(1500_int64, comint_input_ring_size%value))
        call ring%make(ring_size)
        ! Use possibly buffer-local values of these variables.
        ring_max_size = int(comint_input_ring_size%value)
        separator = comint_input_ring_separator%value
        have_prefix = allocated(comint_input_ring_file_prefix%value)
        if (have_prefix) prefix = comint_input_ring_file_prefix%value
        ignore = comint_input_history_ignore%value
        ignoredups = comint_input_ignoredups%value
        old = current_buffer()
        call get_buffer_create(generate_new_buffer_name(" *temp*"), tmp, st)
        call set_buffer(tmp, st)
        call insert_file_contents(file, .false., inserted, st)
        call goto_char(point_max())

        do while (count < ring_max_size)
            call re_search_backward(separator, found, st, noerror=SEARCH_NOERROR)
            if (found == 0) exit
            end = match_beginning(0, st)
            call re_search_backward(separator, found, st, noerror=SEARCH_NOERROR)

            if (found /= 0) then
                call goto_char(match_end(0, st))
            else
                call goto_char(point_min())
            end if

            if (have_prefix) then
                ! Skip zsh extended_history stamps.
                if (looking_at(prefix, st)) call goto_char(match_end(0, st))
            end if

            start = point()
            call buffer_substring(start, end, history, st)
            keep = string_match(ignore, history, st) == 0

            if (keep .and. ignoredups .and. .not. ring%empty_p()) then
                call ring%ref(ring%count() - 1, last, st)
                keep = .not. (last == history .and. len(last) == len(history))
            end if

            if (keep) then
                if (count == ring_size) then
                    call ring%extend(min(ring_max_size - ring_size, ring_size))
                    ring_size = ring%size()
                end if

                call ring%insert_at_beginning(history)
                count = count + 1
            end if
        end do

        call set_buffer(old, st)
        call kill_buffer(tmp, st)
        k = current_entry()
        comint_buffers(k)%input_ring = ring
        comint_buffers(k)%has_ring = .true.
        comint_buffers(k)%input_ring_index = -1
    end subroutine comint_read_input_ring

    !> Writes the buffer's `comint_input_ring' to a history file.
    !> The name of the file is given by the variable
    !> `comint_input_ring_file_name'.  The original contents of the file
    !> are lost if `comint_input_ring' is not empty.  If
    !> `comint_input_ring_file_name' is unallocated this function does
    !> nothing.
    !>
    !> Useful within process sentinels.
    !>
    !> See also `comint_read_input_ring'.  Port of
    !> comint.el:comint-write-input-ring.
    subroutine comint_write_input_ring()
        use buffer_fm, only: kill_buffer, generate_new_buffer_name
        type(handle_type) :: old, tmp
        type(status_type) :: st
        type(ring_type) :: ring
        character(len=:), allocatable :: file, separator, item, msg
        integer :: k, index

        k = entry_of(cur)
        if (.not. allocated(comint_input_ring_file_name%value)) return
        if (len(comint_input_ring_file_name%value) == 0) return
        if (k == 0) return
        if (.not. comint_buffers(k)%has_ring) return
        if (comint_buffers(k)%input_ring%empty_p()) return
        file = comint_input_ring_file_name%value

        if (.not. file_writable_p(file)) then
            call message("Cannot write history file "//file)
            return
        end if

        ring = comint_buffers(k)%input_ring
        separator = comint_input_ring_separator%value
        index = ring%count()
        ! Write it all out into a buffer first.  Much faster, but messier,
        ! than writing it one line at a time.
        old = current_buffer()
        call get_buffer_create(generate_new_buffer_name(" *temp*"), tmp, st)
        call set_buffer(tmp, st)

        do while (index > 0)
            index = index - 1
            call ring%ref(index, item, st)
            call insert(item//separator, st)
        end do

        call write_region(file, .false., st, message=msg)
        call set_buffer(old, st)
        call kill_buffer(tmp, st)
    end subroutine comint_write_input_ring

    !> Read a regexp with PROMPT: REGEXP, and the prefix argument as N.
    !> Port of comint.el:comint-regexp-arg.
    subroutine comint_regexp_arg(prompt, regexp, n, status)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: regexp
        integer, intent(out) :: n
        type(status_type), intent(out) :: status
        type(value_type) :: saved_last, history
        type(value_type), allocatable :: elts(:)
        type(status_type) :: st

        ! Don't clobber this.
        saved_last = last_command
        call read_from_minibuffer(prompt, regexp, status, history="minibuffer_history_search_history")
        last_command = saved_last
        if (.not. status%ok()) return

        ! If the user didn't enter anything, nothing is added to m-h-s-h.
        ! Use the previous search regexp, if there is one.
        if (len(regexp) == 0) then
            call symbol_value(sym("minibuffer_history_search_history"), history, st)

            if (st%ok() .and. consp(history)) then
                call list_elements(history, elts)

                if (size(elts) > 0) then
                    block
                        use lisp_fm, only: stringp, xstring
                        if (stringp(elts(1))) regexp = xstring(elts(1))
                    end block
                end if
            end if
        end if

        n = int(prefix_numeric_value(current_prefix_arg))
    end subroutine comint_regexp_arg

    !> First make sure there is a ring and that we are after the process
    !> mark; ARG is the count (1 for 0, which restarts from the
    !> beginning).  Port of comint.el:comint-search-arg.
    subroutine comint_search_arg(arg, n, status)
        integer, intent(in) :: arg
        integer, intent(out) :: n
        type(status_type), intent(out) :: status
        integer :: k

        status = status_type()
        n = arg

        if (.not. comint_after_pmark_p()) then
            call set_error(status, FM_USER_ERROR, "Not at command line")
            return
        end if

        k = entry_of(cur)

        if (k == 0) then
            call set_error(status, FM_USER_ERROR, "Empty input ring")
            return
        end if

        if (.not. comint_buffers(k)%has_ring) then
            call set_error(status, FM_USER_ERROR, "Empty input ring")
        else if (comint_buffers(k)%input_ring%empty_p()) then
            call set_error(status, FM_USER_ERROR, "Empty input ring")
        else if (arg == 0) then
            ! arg of zero resets search from beginning, and uses arg of 1.
            comint_buffers(k)%input_ring_index = -1
            n = 1
        end if
    end subroutine comint_search_arg

    !> Restore unfinished input.  Port of comint.el:comint-restore-input.
    subroutine comint_restore_input(status)
        type(status_type), intent(out) :: status
        integer :: k

        status = status_type()
        k = entry_of(cur)
        if (k == 0) return
        if (comint_buffers(k)%input_ring_index < 0) return
        call comint_delete_input(status)
        if (.not. status%ok()) return

        if (allocated(comint_buffers(k)%stored_incomplete_input)) then
            if (len(comint_buffers(k)%stored_incomplete_input) > 0) then
                call insert(comint_buffers(k)%stored_incomplete_input, status)
                call message("Input restored")
            end if
        end if

        comint_buffers(k)%input_ring_index = -1
    end subroutine comint_restore_input

    !> Index to start a directional search, starting at
    !> `comint_input_ring_index'.  Port of comint.el:comint-search-start.
    integer function comint_search_start(k, arg) result(index)
        integer, intent(in) :: k, arg

        associate (e => comint_buffers(k))
            if (e%input_ring_index >= 0) then
                ! If a search is running, offset by 1 in direction of arg.
                index = modulo(e%input_ring_index + merge(1, -1, arg > 0), e%input_ring%count())
            else if (arg >= 0) then
                ! For a new search, start from beginning or end, as
                ! appropriate.
                index = 0
            else
                index = e%input_ring%count() - 1
            end if
        end associate
    end function comint_search_start

    !> Cycle backwards through input history, saving input.  Port of
    !> comint.el:comint-previous-input.
    subroutine comint_previous_input(arg, status)
        integer, intent(in) :: arg
        type(status_type), intent(out) :: status
        integer :: k
        logical :: leaving

        k = entry_of(cur)
        leaving = .false.

        if (k /= 0) then
            associate (e => comint_buffers(k))
                if (e%input_ring_index >= 0 .and. allocated(e%stored_incomplete_input) .and. e%has_ring) then
                    ! Leaving the "end" of the ring.
                    leaving = (arg < 0 .and. e%input_ring_index == 0) .or. &
                              (arg > 0 .and. e%input_ring_index == e%input_ring%count() - 1)
                end if
            end associate
        end if

        if (leaving) then
            call comint_restore_input(status)
        else
            call comint_previous_matching_input(".", arg, status)
        end if
    end subroutine comint_previous_input

    !> Cycle forwards through input history.  Port of
    !> comint.el:comint-next-input.
    subroutine comint_next_input(arg, status)
        integer, intent(in) :: arg
        type(status_type), intent(out) :: status

        call comint_previous_input(-arg, status)
    end subroutine comint_next_input

    !> Return the index matching REGEXP ARG places along the input ring
    !> (-1 for none).  Moves relative to START, or
    !> `comint_input_ring_index'.  Port of
    !> comint.el:comint-previous-matching-input-string-position.
    subroutine comint_previous_matching_input_string_position(regexp, arg_in, pos, status, start)
        character(len=*), intent(in) :: regexp
        integer, intent(in) :: arg_in
        integer, intent(out) :: pos
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: start
        character(len=:), allocatable :: item
        integer :: k, length, motion, n, prev, arg
        logical :: tried_each_ring_item
        type(status_type) :: st

        pos = -1
        status = status_type()
        k = entry_of(cur)

        if (k == 0) then
            call set_error(status, FM_USER_ERROR, "No history")
            return
        end if

        if (.not. comint_buffers(k)%has_ring) then
            call set_error(status, FM_USER_ERROR, "No history")
            return
        end if

        if (comint_buffers(k)%input_ring%empty_p()) then
            call set_error(status, FM_USER_ERROR, "No history")
            return
        end if

        arg = arg_in
        length = comint_buffers(k)%input_ring%count()
        motion = merge(1, -1, arg > 0)

        if (present(start)) then
            n = modulo(start - motion, length)
        else
            n = modulo(comint_search_start(k, arg) - motion, length)
        end if

        tried_each_ring_item = .false.

        ! Do the whole search as many times as the argument says.
        do while (arg /= 0 .and. .not. tried_each_ring_item)
            ! Step once.
            prev = n
            n = modulo(n + motion, length)

            ! If we haven't reached a match, step some more.
            do while (n < length .and. .not. tried_each_ring_item)
                call comint_buffers(k)%input_ring%ref(n, item, st)
                if (string_match(regexp, item, status) /= 0) exit
                if (.not. status%ok()) return
                n = modulo(n + motion, length)
                ! If we have gone all the way around in this search.
                tried_each_ring_item = n == prev
            end do

            if (arg > 0) then
                arg = arg - 1
            else
                arg = arg + 1
            end if
        end do

        ! Now that we know which ring element to use, if we found it,
        ! return that.
        call comint_buffers(k)%input_ring%ref(n, item, st)
        if (string_match(regexp, item, status) /= 0) pos = n
    end subroutine comint_previous_matching_input_string_position

    !> Delete all input between accumulation or process mark and point.
    !> Port of comint.el:comint-delete-input.
    subroutine comint_delete_input(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: proc
        integer(int64) :: from
        integer :: k

        status = status_type()
        from = 0
        k = entry_of(cur)

        if (k /= 0) then
            if (comint_buffers(k)%local) from = marker_pos(comint_buffers(k)%accum_marker)
        end if

        if (from == 0) then
            call current_process(proc, status)
            if (.not. status%ok()) return
            from = pmark_pos(proc)
        end if

        ! Can't use kill-region as it sets this-command.
        call delete_region(from, point_max(), status)
    end subroutine comint_delete_input

    !> Search backwards through input history for match for REGEXP.
    !> (Previous history elements are earlier commands.)
    !> With prefix argument N, search for Nth previous match.
    !> If N is negative, find the next or Nth next match.  Port of
    !> comint.el:comint-previous-matching-input.
    subroutine comint_previous_matching_input(regexp, n_in, status, restore)
        character(len=*), intent(in) :: regexp
        integer, intent(in) :: n_in
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: restore
        character(len=:), allocatable :: item, old
        character(len=16) :: num
        integer :: n, pos, k
        logical :: wrap

        call comint_search_arg(n_in, n, status)
        if (.not. status%ok()) return
        call comint_previous_matching_input_string_position(regexp, n, pos, status)
        if (.not. status%ok()) return

        ! Has a match been found?
        if (pos < 0) then
            call set_error(status, FM_USER_ERROR, "Not found")
            return
        end if

        k = entry_of(cur)
        wrap = .false.

        if (comint_buffers(k)%input_ring_index >= 0 .and. present(restore)) then
            if (restore) wrap = (n < 0 .and. comint_buffers(k)%input_ring_index < pos) .or. &
                                (n > 0 .and. comint_buffers(k)%input_ring_index > pos)
        end if

        if (wrap) then
            ! We have a wrap; restore contents.
            call comint_restore_input(status)
            return
        end if

        ! If leaving the edit line, save partial input.
        if (comint_buffers(k)%input_ring_index < 0) then
            call call_get_old_input(old, status)
            if (.not. status%ok()) return
            k = entry_of(cur)
            comint_buffers(k)%stored_incomplete_input = old
        end if

        comint_buffers(k)%input_ring_index = pos
        write (num, "(i0)") pos + 1
        call message("History item: "//trim(num))
        call comint_delete_input(status)
        if (.not. status%ok()) return
        call comint_input_ring_ref(pos, item)
        call insert(item, status)
    end subroutine comint_previous_matching_input

    !> Item POS of the current buffer's input ring.  Not a port.
    subroutine comint_input_ring_ref(pos, item)
        integer, intent(in) :: pos
        character(len=:), allocatable, intent(out) :: item
        type(status_type) :: st
        integer :: k

        item = ""
        k = entry_of(cur)
        if (k /= 0) call comint_buffers(k)%input_ring%ref(pos, item, st)
    end subroutine comint_input_ring_ref

    !> Run the current buffer's `comint_get_old_input'.  Not a port.
    subroutine call_get_old_input(input, status)
        character(len=:), allocatable, intent(out) :: input
        type(status_type), intent(out) :: status
        integer :: k

        k = local_entry()
        call comint_buffers(k)%get_old_input(input, status)
    end subroutine call_get_old_input

    !> Search forwards through input history for match for REGEXP.
    !> (Later history elements are more recent commands.)
    !> With prefix argument N, search for Nth following match.
    !> If N is negative, find the previous or Nth previous match.  Port of
    !> comint.el:comint-next-matching-input.
    subroutine comint_next_matching_input(regexp, n, status)
        character(len=*), intent(in) :: regexp
        integer, intent(in) :: n
        type(status_type), intent(out) :: status

        call comint_previous_matching_input(regexp, -n, status)
    end subroutine comint_next_matching_input

    !> Search backwards through input history for match for current input.
    !> (Previous history elements are earlier commands.)
    !> With prefix argument N, search for Nth previous match.
    !> If N is negative, search forwards for the -Nth following match.
    !> Port of comint.el:comint-previous-matching-input-from-input.
    subroutine comint_previous_matching_input_from_input(n, status)
        integer, intent(in) :: n
        type(status_type), intent(out) :: status
        type(handle_type) :: proc
        integer(int64) :: opoint, from
        integer :: k
        character(len=:), allocatable :: text

        opoint = point()
        k = local_entry()

        if (.not. (symbol_is(last_command, "comint_previous_matching_input_from_input") .or. &
                   symbol_is(last_command, "comint_next_matching_input_from_input"))) then
            ! Starting a new search.
            from = marker_pos(comint_buffers(k)%accum_marker)

            if (from == 0) then
                call current_process(proc, status)
                if (.not. status%ok()) return
                from = pmark_pos(proc)
            end if

            call buffer_substring(from, point(), text, status)
            if (.not. status%ok()) return
            comint_buffers(k)%matching_input_from_input_string = text
            comint_buffers(k)%input_ring_index = -1
        end if

        call comint_previous_matching_input("^"//regexp_quote(comint_buffers(k)%matching_input_from_input_string), &
                                            n, status, .true.)
        if (.not. status%ok()) return
        if (comint_move_point_for_matching_input%value == COMINT_MATCHING_AFTER_INPUT) call goto_char(opoint)
    end subroutine comint_previous_matching_input_from_input

    !> Search forwards through input history for match for current input.
    !> (Following history elements are more recent commands.)
    !> With prefix argument N, search for Nth following match.
    !> If N is negative, search backwards for the -Nth previous match.
    !> Port of comint.el:comint-next-matching-input-from-input.
    subroutine comint_next_matching_input_from_input(n, status)
        integer, intent(in) :: n
        type(status_type), intent(out) :: status

        call comint_previous_matching_input_from_input(-n, status)
    end subroutine comint_next_matching_input_from_input

    !> Add CMD to the input history.
    !> Ignore duplicates if `comint_input_ignoredups' is true.  Port of
    !> comint.el:comint-add-to-input-history.
    subroutine comint_add_to_input_history(cmd)
        character(len=*), intent(in) :: cmd
        character(len=:), allocatable :: last
        type(status_type) :: st
        integer :: k, size
        logical :: add

        k = local_entry()

        if (.not. comint_buffers(k)%has_ring) then
            call comint_buffers(k)%input_ring%make(int(comint_input_ring_size%value))
            comint_buffers(k)%has_ring = .true.
        end if

        add = comint_buffers(k)%input_filter(cmd)

        if (add .and. comint_input_ignoredups%value .and. .not. comint_buffers(k)%input_ring%empty_p()) then
            call comint_buffers(k)%input_ring%ref(0, last, st)
            add = .not. (last == cmd .and. len(last) == len(cmd))
        end if

        if (.not. add) return
        ! If `comint-input-ring' is full, maybe grow it.
        size = comint_buffers(k)%input_ring%size()

        if (size == comint_buffers(k)%input_ring%count() .and. size < comint_input_ring_size%value) then
            call comint_buffers(k)%input_ring%extend(int(min(int(size, int64), comint_input_ring_size%value - size)))
        end if

        call comint_buffers(k)%input_ring%insert(cmd)
    end subroutine comint_add_to_input_history

    !> Return an array of arguments from ARG, backwards.
    !> Break it up at the delimiters in `comint_delimiter_argument_list'.
    !> Characters marked LITERAL (e.g., backslash-escaped characters) are
    !> not considered to be delimiters.  Port of comint.el:comint-delim-arg
    !> (the `literal' text property of the string as an array).
    subroutine comint_delim_arg(arg, literal, args)
        character(len=*), intent(in) :: arg
        logical, intent(in) :: literal(:)
        type(process_arg_type), allocatable, intent(out) :: args(:)
        character(len=:), allocatable :: delims
        integer :: pos, start, n
        character :: ch

        delims = ""
        if (allocated(comint_delimiter_argument_list%value)) delims = comint_delimiter_argument_list%value

        if (len(delims) == 0) then
            allocate (args(1))
            args(1)%s = arg
            return
        end if

        allocate (args(0))
        n = len(arg)
        pos = 1

        do while (pos <= n)
            ch = arg(pos:pos)
            start = pos

            if (index(delims, ch) > 0 .and. .not. literal(pos)) then
                do while (pos <= n)
                    if (arg(pos:pos) /= ch) exit
                    pos = pos + 1
                end do
            else
                do while (pos <= n)
                    if (index(delims, arg(pos:pos)) > 0 .and. .not. literal(pos)) exit
                    pos = pos + 1
                end do
            end if

            call prepend_arg(args, arg(start:pos - 1))
        end do
    end subroutine comint_delim_arg

    !> Return from STRING the NTH to MTH arguments.
    !> NTH and/or MTH can be absent, which means the last argument.
    !> NTH and MTH can be negative to count from the end; -1 means
    !> the last argument.
    !> Returned arguments are separated by single spaces.  We assume
    !> whitespace separates arguments, except within quotes and except
    !> for a space or tab that immediately follows a backslash.  Also, a
    !> run of one or more of a single character in
    !> `comint_delimiter_argument_list' is a separate argument.
    !> Argument 0 is the command name.  Port of comint.el:comint-arguments.
    function comint_arguments(string, nth, mth) result(res)
        character(len=*), intent(in) :: string
        integer, intent(in), optional :: nth, mth
        character(len=:), allocatable :: res
        ! The first line handles ordinary characters and
        ! backslash-sequences.  The second matches "-quoted strings.  The
        ! third matches '-quoted strings.  The fourth matches `-quoted
        ! strings.  This seems to fit the syntax of BASH 2.0.
        character(len=*), parameter :: ARGPART = "[^ "//achar(10)//achar(9)//"""'`\]\|\(\\.\)\|"// &
                                       "\(""\([^""\]\|\\.\)*""\|'[^']*'\|`[^`]*`\)"
        type(process_arg_type), allocatable :: args(:), parts(:)
        logical, allocatable :: literal(:)
        type(status_type) :: st
        integer(int64) :: idx, mb, me, b1, e1, q
        integer :: pos, count, beg, n, m, k
        logical :: quotes, have_beg

        allocate (args(0))
        allocate (literal(len(string)))
        literal = .false.
        pos = 1
        count = 0
        have_beg = .false.
        beg = 0
        quotes = .false.

        ! Build a list of all the args until we have as many as we want.
        do
            if (present(mth)) then
                if (mth >= 0 .and. count > mth) exit
            end if

            idx = string_match(ARGPART, string, st, start=int(pos, int64))
            if (idx == 0) exit
            mb = match_beginning(0, st)
            me = match_end(0, st)
            ! Apply the `literal' text property to backslash-escaped
            ! characters, so that `comint-delim-arg' won't break them up.
            b1 = match_beginning(1, st)
            e1 = match_end(1, st)
            if (b1 > 0) literal(b1:e1 - 1) = .true.
            q = match_beginning(2, st)

            if (have_beg .and. pos == mb) then
                ! It's contiguous, part of the same arg.
                pos = int(me)
                quotes = quotes .or. q > 0
            else
                ! It's a new separate arg.
                if (have_beg) call push_arg()
                count = size(args)
                quotes = q > 0
                beg = int(mb)
                have_beg = .true.
                pos = int(me)
            end if
        end do

        if (have_beg) call push_arg()
        count = size(args)

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
        ! ARGS is backwards, the J'th argument (from 0) being
        ! ARGS(COUNT - J).
        res = ""

        do k = max(n, 0), count - 1 - m
            if (k > max(n, 0)) res = res//" "
            res = res//args(count - k)%s
        end do

    contains

        subroutine push_arg()
            character(len=:), allocatable :: str

            str = string(beg:pos - 1)

            if (quotes) then
                call prepend_arg(args, str)
            else
                call comint_delim_arg(str, literal(beg:pos - 1), parts)
                args = [parts, args]
            end if
        end subroutine push_arg
    end function comint_arguments

    !> Return FILENAME with environment variables substituted.
    !> Supports additional environment variable syntax of the command
    !> interpreter.  Port of comint.el:comint-substitute-in-file-name.
    function comint_substitute_in_file_name(filename) result(name)
        use fileio_fm, only: substitute_in_file_name
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: name

        name = substitute_in_file_name(filename)
    end function comint_substitute_in_file_name

    ! ------------------------------------------------------------------
    ! Sending input
    ! ------------------------------------------------------------------

    !> Send input to process.
    !> After the process output mark, sends all text from the process mark
    !> to point as input to the process.  Before the process output mark,
    !> calls value of variable `comint_get_old_input' to retrieve old
    !> input, copies it to the process mark, and sends it.
    !>
    !> This command also sends and inserts a final newline, unless
    !> NO_NEWLINE is true.
    !>
    !> The functions of `comint_input_filter_functions' are called on the
    !> input before sending it.  The input is entered into the input
    !> history ring, if the value of variable `comint_input_filter' returns
    !> true when called on the input.
    !>
    !> If variable `comint_eol_on_send' is true, then point is moved to
    !> the end of line before sending the input.
    !>
    !> After the input has been sent, if `comint_process_echoes' is true,
    !> then `comint_send_input' waits to see if the process outputs a
    !> string matching the input, and if so, deletes that part of the
    !> output.  If ARTIFICIAL is true, it inhibits such deletion.  Callers
    !> sending input not from the user should use ARTIFICIAL = true.
    !>
    !> The values of `comint_get_old_input', `comint_input_filter_functions',
    !> and `comint_input_filter' are chosen according to the command
    !> interpreter running in the buffer.  Port of
    !> comint.el:comint-send-input (input history expansion is Deferred).
    recursive subroutine comint_send_input(status, no_newline, artificial, sender, run_filters)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_newline, artificial
        !> Send with SENDER instead of `comint_input_sender' (the binding
        !> comint-skip-input makes).
        procedure(comint_input_sender_proc), optional :: sender
        !> False to run no `comint_input_filter_functions' (the binding
        !> comint-skip-input makes).
        logical, intent(in), optional :: run_filters
        type(handle_type) :: proc, pmark
        type(value_type) :: saved, props
        type(silent_modifications_type) :: silent
        type(status_type) :: st
        character(len=:), allocatable :: intxt, input, history, copy
        integer(int64) :: pm, beg, end, echo_len, lie, lis, start
        logical :: nonl, art, changed, got, filters, expanded
        integer :: k

        nonl = .false.
        if (present(no_newline)) nonl = no_newline
        art = .false.
        if (present(artificial)) art = artificial
        filters = .true.
        if (present(run_filters)) filters = run_filters
        ! Note that the input string does not include its terminal newline.
        call current_process(proc, status)
        if (.not. status%ok()) return
        k = local_entry()
        call widen()
        pmark = process_mark(proc)
        pm = pmark_pos(proc)

        if (point() >= pm) then
            if (comint_eol_on_send%value) then
                if (comint_use_prompt_regexp%value) then
                    call end_of_line()
                else
                    call goto_char(field_end(point(), .false.))
                end if
            end if

            call buffer_substring(pm, point(), intxt, status)
            if (.not. status%ok()) return
        else
            call call_get_old_input(copy, status)
            if (.not. status%ok()) return
            call goto_char(pm)
            call insert(copy, status)
            if (.not. status%ok()) return
            intxt = copy
        end if

        if (comint_input_autoexpand%value /= COMINT_AUTOEXPAND_INPUT) then
            ! Just whatever's already there.
            input = intxt
        else
            ! Expand and leave it visible in buffer.
            expanded = comint_replace_by_expanded_history(st, silent=.true., start=pm)
            call buffer_substring(pm, point(), input, st)
        end if

        if (comint_input_autoexpand%value /= COMINT_AUTOEXPAND_HISTORY) then
            history = input
        else
            ! This is messy 'cos ultimately the original functions used do
            ! insertion, rather than return strings.  We have to expand,
            ! then insert back.
            expanded = comint_replace_by_expanded_history(st, silent=.true., start=pm)
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
        call comint_add_to_input_history(history)

        if (filters) then
            if (nonl) then
                call comint_input_filter_functions%run(input, status)
            else
                call comint_input_filter_functions%run(input//achar(10), status)
            end if

            if (.not. status%ok()) return
        end if

        k = local_entry()
        beg = marker_pos(pmark)

        if (nonl) then
            end = point()
        else
            end = point() - 1
        end if

        call silent_modifications_begin(silent)

        if (end > beg) then
            if (comint_highlight_input%value) then
                props = list(sym("font_lock_face"), sym("comint_highlight_input"), &
                             sym("comint__fontify_input_inhibit_fontification"), t_value(), &
                             sym("front_sticky"), t_value())
                call add_text_properties(beg, end, props, changed, st)
            end if

            if (.not. comint_use_prompt_regexp%value) then
                ! Give old user input a field property of `input', to
                ! distinguish it from both process output and unsent input.
                ! The terminating newline is put into a special `boundary'
                ! field to make cursor movement between input and output
                ! fields smoother.
                props = list(sym("mouse_face"), sym("highlight"), sym("help_echo"), &
                             make_str("mouse-2: insert after prompt as new input"))
                call add_text_properties(beg, end, props, changed, st)
            end if
        end if

        if (.not. (nonl .or. comint_use_prompt_regexp%value)) then
            ! Cover the terminating newline.
            props = rear_nonsticky_plist()
            call add_text_properties(end, end + 1, props, changed, st)
            props = list(sym("field"), sym("boundary"), sym("inhibit_line_move_field_capture"), t_value())
            call add_text_properties(end, end + 1, props, changed, st)
        end if

        call silent_modifications_end(silent)
        call comint_snapshot_last_prompt()
        k = local_entry()
        comint_buffers(k)%save_input_ring_index = comint_buffers(k)%input_ring_index
        comint_buffers(k)%input_ring_index = -1
        ! Update the markers before we send the input in case we get
        ! output amidst sending the input.
        call set_marker(comint_buffers(k)%last_input_start, pm, st)
        call set_marker(comint_buffers(k)%last_input_end, point(), st)
        call set_marker(pmark, point(), st)
        ! Clear the "accumulation" marker.
        call detach_marker(comint_buffers(k)%accum_marker, st)
        comint_input_sender_no_newline = nonl

        if (present(sender)) then
            call sender(proc, input, status)
        else
            call comint_buffers(k)%input_sender(proc, input, status)
        end if

        comint_input_sender_no_newline = .false.
        if (.not. status%ok()) return
        ! Optionally delete echoed input (after checking it).
        k = local_entry()

        if (comint_process_echoes%value .and. .not. art) then
            lis = marker_pos(comint_buffers(k)%last_input_start)
            lie = marker_pos(comint_buffers(k)%last_input_end)
            echo_len = lie - lis

            ! Wait for all input to be echoed.
            do while (lie + echo_len > point_max())
                call accept_process_output(got, st, proc)
                if (.not. got) exit
                if (.not. same_text(lis, point_max() - echo_len, lie, point_max())) exit
            end do

            if (lie + echo_len <= point_max()) then
                if (same_text(lis, lie, lie, lie + echo_len)) then
                    ! Certain parts of the text to be deleted may have been
                    ! mistaken for prompts.  We have to prevent problems
                    ! when `comint_prompt_read_only' is .true..
                    call inhibit_read_only_begin(saved)
                    call delete_region(lie, lie + echo_len, st)

                    if (comint_prompt_read_only%value) then
                        block
                            integer(int64) :: opoint
                            opoint = point()
                            call goto_char(lie)
                            call comint_update_fence()
                            call goto_char(opoint)
                        end block
                    end if

                    call inhibit_read_only_end(saved)
                end if
            end if
        end if

        ! This used to call comint-output-filter-functions, but that
        ! scrolled the buffer in undesirable ways.
        k = local_entry()
        call set_marker(comint_buffers(k)%last_output_start, marker_pos(pmark), st)
        call comint_output_filter_functions%run("", status)

    contains

        !> Whether the text from B1 to E1 is the text from B2 to E2
        !> (compare-buffer-substrings gives 0).
        logical function same_text(b1, e1, b2, e2)
            integer(int64), intent(in) :: b1, e1, b2, e2
            character(len=:), allocatable :: t1, t2
            type(status_type) :: s

            call buffer_substring(max(b1, point_min()), min(e1, point_max()), t1, s)
            call buffer_substring(max(b2, point_min()), min(e2, point_max()), t2, s)
            same_text = t1 == t2 .and. len(t1) == len(t2)
        end function same_text
    end subroutine comint_send_input

    ! Not a port.
    function make_str(s) result(v)
        use lisp_fm, only: make_string
        character(len=*), intent(in) :: s
        type(value_type) :: v

        v = make_string(s)
    end function make_str

    !> The command: send input to process.  The interactive form of
    !> comint.el:comint-send-input.
    !>
    !> Send input to process.
    recursive subroutine cmd_comint_send_input(status)
        type(status_type), intent(out) :: status

        call comint_send_input(status)
    end subroutine cmd_comint_send_input

    !> Snapshot the current `comint_last_prompt'.
    !> Freezes the `font_lock_face' text property in place.  Port of
    !> comint.el:comint-snapshot-last-prompt.
    subroutine comint_snapshot_last_prompt()
        type(silent_modifications_type) :: silent
        type(status_type) :: st
        integer :: k

        k = entry_of(cur)
        if (k == 0) return
        if (.not. comint_buffers(k)%has_last_prompt) return
        call silent_modifications_begin(silent)
        call font_lock_append_text_property(marker_pos(comint_buffers(k)%last_prompt_start), &
                                            marker_pos(comint_buffers(k)%last_prompt_end), &
                                            sym("font_lock_face"), sym("comint_highlight_prompt"), st)
        call silent_modifications_end(silent)
        ! Reset comint-last-prompt so later on comint-output-filter does
        ! not remove the font-lock-face text property of the previous
        ! (this) prompt.
        call forget_last_prompt(comint_buffers(k))
    end subroutine comint_snapshot_last_prompt

    !> Default function for sending to PROC input STRING.
    !> This just sends STRING plus a newline.  To override this, set the
    !> hook `comint_input_sender'.  Port of comint.el:comint-simple-send.
    recursive subroutine comint_simple_send(process, string, status)
        type(handle_type), intent(in) :: process
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status

        if (comint_input_sender_no_newline) then
            call comint_send_string(process, string, status)
        else
            ! Sending as two separate strings does not work on Windows, so
            ! concat the \n before sending.
            call comint_send_string(process, string//achar(10), status)
        end if

        if (.not. status%ok()) return
        if (comint_input_sender_no_newline .and. len(string) > 0) call process_send_eof(status)
    end subroutine comint_simple_send

    !> The `comint_input_sender' comint-skip-input binds: send nothing.
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
    recursive subroutine comint_send_string(process, string, status)
        type(handle_type), intent(in) :: process
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status

        call snapshot_in_buffer_of(process)
        call process_send_string(string, status, process)
    end subroutine comint_send_string

    !> Like `process_send_region', but also does extra bookkeeping for
    !> Comint mode.  Port of comint.el:comint-send-region.
    subroutine comint_send_region(process, start, end, status)
        type(handle_type), intent(in) :: process
        integer(int64), intent(in) :: start, end
        type(status_type), intent(out) :: status

        call snapshot_in_buffer_of(process)
        call process_send_region(start, end, status, process)
    end subroutine comint_send_region

    !> comint-snapshot-last-prompt in the buffer of PROCESS.  Not a port.
    subroutine snapshot_in_buffer_of(process)
        type(handle_type), intent(in) :: process
        type(handle_type) :: old, buf
        type(status_type) :: st

        buf = process_buffer(process)
        if (.not. buffer_live_p(buf)) return
        old = current_buffer()
        call set_buffer(buf, st)
        call comint_snapshot_last_prompt()
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
    recursive subroutine comint_carriage_motion(start, end)
        integer(int64), intent(in) :: start, end
        type(value_type) :: v, inhibit
        type(handle_type) :: delete_end, saved_point
        type(status_type) :: st
        integer(int64) :: opoint, lbeg, de, moved
        logical :: changed
        integer(int32) :: ch
        logical :: have_delete_end

        call copy_marker_at(point(), saved_point, .false., st)
        call goto_char(start)
        call inhibit_read_only_begin(inhibit)
        lbeg = line_beginning_position()
        have_delete_end = .false.

        ! If the preceding text is marked as "must-overwrite", record it in
        ! delete-end.
        if (start > point_min()) then
            call get_text_property(start - 1, sym("comint_must_overwrite"), v, st)

            if (.not. nilp(v)) then
                call copy_marker_at(point(), delete_end, .false., st)
                have_delete_end = .true.
                call remove_text_properties(lbeg, start, list(sym("comint_must_overwrite"), nil()), changed, st)
            end if
        end if

        call narrow_to_region(lbeg, end, st)

        ! Handle BS, LF, and CR specially.
        do
            moved = skip_chars_forward("^"//achar(8)//achar(10)//achar(13), st)
            if (eobp()) exit
            ch = following_char()

            if (ch == 8) then
                ! CH = BS
                call delete_char(1_int64, st)
                if (point() > lbeg) call delete_char(-1_int64, st)
            else if (ch == 10) then
                ! CH = LF
                if (have_delete_end) then
                    call marker_position(delete_end, de, st)
                    if (de < point()) call delete_region(lbeg, de, st)
                    call free_marker(delete_end, st)
                    have_delete_end = .false.
                end if

                call forward_char(st, 1_int64)
                lbeg = point()
            else
                ! CH = CR
                call delete_char(1_int64, st)

                if (have_delete_end) then
                    call marker_position(delete_end, de, st)

                    if (de < point()) then
                        call set_marker(delete_end, point(), st)
                    end if
                else
                    call copy_marker_at(point(), delete_end, .false., st)
                    have_delete_end = .true.
                end if
            end if
        end do

        if (have_delete_end) then
            call marker_position(delete_end, de, st)

            if (de < point()) then
                ! As there's a text after the last CR, make the current
                ! line contain only that text.
                call delete_region(lbeg, de, st)
            else
                ! Remember that the process output ends with CR, and thus
                ! the next output should overwrite the current line.
                call put_text_property(lbeg, de, sym("comint_must_overwrite"), t_value(), st)
            end if

            call free_marker(delete_end, st)
        end if

        call widen()
        ! (save-excursion restores point.)
        call marker_position(saved_point, opoint, st)
        call goto_char(opoint)
        call free_marker(saved_point, st)
        call inhibit_read_only_end(inhibit)
    end subroutine comint_carriage_motion

    !> The filter of Comint processes: insert STRING (after the
    !> `comint_preoutput_filter_functions') at the process mark, with
    !> carriage motion, run `comint_output_filter_functions', mark the text
    !> as output and the last line as the prompt.  Port of
    !> comint.el:comint-output-filter.
    recursive subroutine comint_output_filter(process, string, status)
        type(handle_type), intent(in) :: process
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        type(handle_type) :: oprocbuf, saved_point, rbeg, rend, pmark
        type(value_type) :: inhibit
        type(silent_modifications_type) :: silent
        type(status_type) :: st
        character(len=:), allocatable :: text
        integer(int64) :: start, prompt_start, b, e, shortage
        logical :: narrowed, changed
        type(value_type) :: v
        integer :: k

        status = status_type()
        oprocbuf = process_buffer(process)
        ! First check for killed buffer or no input.
        if (.not. buffer_live_p(oprocbuf)) return
        call set_buffer(oprocbuf, status)
        if (.not. status%ok()) return
        k = local_entry()
        ! Run preoutput filters.
        call comint_preoutput_filter_functions%run(string, text, status)
        if (.not. status%ok()) return
        ! Insert STRING.
        call inhibit_read_only_begin(inhibit)
        ! The point should float after any insertion we do.
        call copy_marker_at(point(), saved_point, .true., st)
        ! We temporarily remove any buffer narrowing, in case the process
        ! mark is outside of the restriction.
        narrowed = buffers(cur)%begv > 1 .or. buffers(cur)%zv < buffers(cur)%text%z
        call copy_marker_at(point_min(), rbeg, .false., st)
        call copy_marker_at(point_max(), rend, .true., st)
        call widen()
        pmark = process_mark(process)
        call goto_char(marker_pos(pmark))
        k = local_entry()
        call set_marker(comint_buffers(k)%last_output_start, point(), st)
        start = point()
        ! insert-before-markers is a bad thing.  (The text is marked as
        ! output once it is inserted, the property being on the string
        ! upstream.)
        call insert(text, status)

        if (status%ok()) then
            call put_text_property(start, point(), sym("field"), sym("output"), st)
            ! Advance process-mark.
            call set_marker(pmark, point(), st)

            if (.not. comint_inhibit_carriage_motion%value) then
                ! Interpret any carriage motion characters (newline,
                ! backspace).
                call comint_carriage_motion(marker_pos(comint_buffers(k)%last_output_start), point())
            end if

            ! Run these hooks with point where the user had it.
            call goto_char(marker_pos(saved_point))
            call comint_output_filter_functions%run(text, status)
            call set_marker(saved_point, point(), st)
            ! In case a filter moved it.
            call goto_char(marker_pos(pmark))
            k = local_entry()

            if (.not. comint_use_prompt_regexp%value) then
                call comint_mark_as_output(marker_pos(comint_buffers(k)%last_output_start), point())
            end if

            ! Highlight the prompt, where we define `prompt' to mean the
            ! most recent output that doesn't end with a newline.
            b = point()
            shortage = forward_line(0_int64)
            prompt_start = point()
            call goto_char(b)

            if (comint_prompt_read_only%value) then
                call silent_modifications_begin(silent)

                if (point_min() /= prompt_start) then
                    call get_text_property(prompt_start - 1, sym("read_only"), v, st)
                    if (nilp(v)) call put_text_property(prompt_start - 1, prompt_start, sym("read_only"), &
                                                        sym("fence"), st)
                end if

                call add_text_properties(prompt_start, point(), list(sym("read_only"), t_value(), sym("front_sticky"), &
                                                                     list(sym("read_only"))), changed, st)
                call silent_modifications_end(silent)
            end if

            k = local_entry()

            if (comint_buffers(k)%has_last_prompt) then
                ! There might be some keywords here waiting for
                ! fontification, so no `with-silent-modifications'.
                call font_lock_remove_face_from_text_property(marker_pos(comint_buffers(k)%last_prompt_start), &
                                                              marker_pos(comint_buffers(k)%last_prompt_end), &
                                                              sym("font_lock_face"), sym("comint_highlight_prompt"), st)
                call forget_last_prompt(comint_buffers(k))
            end if

            call copy_marker_at(prompt_start, comint_buffers(k)%last_prompt_start, .false., st)
            call copy_marker_at(point(), comint_buffers(k)%last_prompt_end, .false., st)
            comint_buffers(k)%has_last_prompt = .true.
            e = point()
            call font_lock_append_text_property(prompt_start, e, sym("font_lock_face"), sym("comint_highlight_prompt"), st)
            call add_text_properties(prompt_start, e, rear_nonsticky_plist(), changed, st)
        end if

        ! save-restriction.
        if (narrowed) then
            call narrow_to_region(marker_pos(rbeg), marker_pos(rend), st)
        end if

        call free_marker(rbeg, st)
        call free_marker(rend, st)
        call goto_char(marker_pos(saved_point))
        call free_marker(saved_point, st)
        call inhibit_read_only_end(inhibit)
    end subroutine comint_output_filter

    !> Mark the text from BEG to END as process output.  Port of
    !> comint.el:comint--mark-as-output.
    subroutine comint_mark_as_output(beg, end)
        integer(int64), intent(in) :: beg, end
        type(silent_modifications_type) :: silent
        type(status_type) :: st
        logical :: changed

        call silent_modifications_begin(silent)
        call add_text_properties(beg, end, rear_nonsticky_plist(), changed, st)
        call add_text_properties(beg, end, list(sym("front_sticky"), &
                                                list(sym("field"), sym("inhibit_line_move_field_capture")), &
                                                sym("field"), sym("output"), &
                                                sym("inhibit_line_move_field_capture"), t_value()), changed, st)
        ! Text inserted by a user in the middle of process output should be
        ! marked as output.  (Text property hooks are not ported: the
        ! property is set as upstream sets it.)
        call add_text_properties(beg, end, list(sym("insert_in_front_hooks"), &
                                                list(sym("comint__mark_as_output"), &
                                                     sym("comint__mark_yanked_as_output"))), changed, st)
        call silent_modifications_end(silent)
    end subroutine comint_mark_as_output

    !> Go to the end of buffer in some or all windows showing it.
    !> Do not scroll if the current line is the last line in the buffer.
    !> Depends on the value of `comint_move_point_for_output' and
    !> `comint_scroll_show_maximum_output'.
    !>
    !> This function should be in the hook `comint_output_filter_functions'.
    !> Port of comint.el:comint-postoutput-scroll-to-bottom.
    recursive subroutine comint_postoutput_scroll_to_bottom(string, status)
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
            call comint_adjust_window_point(leaves(i), process)

            ! Optionally scroll to the bottom of the window.
            if (comint_scroll_show_maximum_output%value) then
                if (window_point_of(leaves(i)) == point_max()) then
                    call select_window(leaves(i), st, norecord=.true.)
                    call recenter(make_int(-1_int64), .false., st)
                    call select_window(selected, st, norecord=.true.)
                end if
            end if

            call set_buffer(current, st)
        end do
    end subroutine comint_postoutput_scroll_to_bottom

    ! Not a port.
    function make_int(n) result(v)
        use lisp_fm, only: make_fixnum
        integer(int64), intent(in) :: n
        type(value_type) :: v

        v = make_fixnum(n)
    end function make_int

    !> The point of window W: point, for the selected window showing the
    !> current buffer.  Not a port.
    integer(int64) function window_point_of(w) result(pos)
        type(handle_type), intent(in) :: w

        if (same_handle(w, selected_frame%selected_window) .and. same_handle(windows(w%index)%buffer, current_buffer())) then
            pos = point()
        else
            pos = window_point(windows(w%index))
        end if
    end function window_point_of

    !> Move point in WINDOW based on Comint settings.
    !> For point adjustment use the process-mark of PROCESS.  Port of
    !> comint.el:comint-adjust-window-point.
    subroutine comint_adjust_window_point(window, process)
        type(handle_type), intent(in) :: window, process
        type(status_type) :: st
        integer(int64) :: wp, pm, los
        logical :: move
        integer :: k

        wp = window_point_of(window)
        pm = pmark_pos(process)
        if (.not. (wp < pm)) return

        select case (comint_move_point_for_output%value)
        case (COMINT_MOVE_T, COMINT_MOVE_ALL)
            move = .true.
        case (COMINT_MOVE_THIS)
            ! Maybe user wants point to jump to end.
            move = same_handle(window, selected_frame%selected_window)
        case (COMINT_MOVE_OTHERS)
            move = .not. same_handle(window, selected_frame%selected_window)
        case default
            move = .false.
        end select

        if (.not. move) then
            ! If point was at the end, keep it at end.
            k = entry_of(cur)
            los = 0
            if (k /= 0) los = marker_pos(comint_buffers(k)%last_output_start)
            move = los /= 0 .and. wp >= los
        end if

        if (.not. move) return

        if (same_handle(window, selected_frame%selected_window) .and. &
            same_handle(windows(window%index)%buffer, current_buffer())) then
            call goto_char(pm)
        else
            call set_window_point(windows(window%index), pm, st)
        end if
    end subroutine comint_adjust_window_point

    !> Truncate the buffer to `comint_buffer_maximum_size'.
    !> This function could be on `comint_output_filter_functions' or bound
    !> to a key (STRING is not used).  Port of comint.el:comint-truncate-buffer.
    recursive subroutine comint_truncate_buffer(string, status)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status

        status = status_type()
        if (len(string) < 0) return
        call truncate_to(comint_buffer_maximum_size%value, status)
    end subroutine comint_truncate_buffer

    !> The command.  Truncate the buffer to `comint_buffer_maximum_size'.
    recursive subroutine cmd_comint_truncate_buffer(status)
        type(status_type), intent(out) :: status

        call comint_truncate_buffer("", status)
    end subroutine cmd_comint_truncate_buffer

    !> comint-truncate-buffer with comint-buffer-maximum-size MAXIMUM.
    !> Port of comint.el:comint-truncate-buffer (its body).
    subroutine truncate_to(maximum, status)
        integer(int64), intent(in) :: maximum
        type(status_type), intent(out) :: status
        type(handle_type) :: proc
        type(value_type) :: inhibit
        integer(int64) :: opoint, shortage

        call current_process(proc, status)
        if (.not. status%ok()) return
        opoint = point()
        call goto_char(pmark_pos(proc))
        shortage = forward_line(-maximum)
        call beginning_of_line()
        call inhibit_read_only_begin(inhibit)
        call delete_region(point_min(), point(), status)
        call inhibit_read_only_end(inhibit)
        call goto_char(min(opoint, point_max()))
    end subroutine truncate_to

    !> Strip trailing `^M' characters from the current output group.
    !> This function could be on `comint_output_filter_functions' or bound
    !> to a key (STRING is not used).  Port of comint.el:comint-strip-ctrl-m.
    subroutine comint_strip_ctrl_m(string, status)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status

        status = status_type()
        if (len(string) < 0) return
        call strip_ctrl_m(status, .false.)
    end subroutine comint_strip_ctrl_m

    !> comint-strip-ctrl-m, called interactively if INTERACTIVE.
    !> Port of comint.el:comint-strip-ctrl-m (its body).
    subroutine strip_ctrl_m(status, interactive)
        use search_fm, only: re_search_forward, replace_match
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: interactive
        type(handle_type) :: proc
        type(status_type) :: st
        integer(int64) :: opoint, found, pm, from
        logical :: inter
        integer :: k

        status = status_type()
        inter = .false.
        if (present(interactive)) inter = interactive
        proc = get_buffer_process(current_buffer())

        if (proc%index == 0) then
            ! This function may be used in `comint-output-filter-functions',
            ! and in that case, if there's no process, then we should do
            ! nothing.  If interactive, report an error.
            if (inter) call set_error(status, FM_GENERIC_ERROR, "No process in the current buffer")
            return
        end if

        pm = pmark_pos(proc)
        opoint = point()
        k = entry_of(cur)
        from = 0

        if (k /= 0) then
            if (inter) then
                from = marker_pos(comint_buffers(k)%last_input_end)
            else
                from = marker_pos(comint_buffers(k)%last_output_start)
            end if
        end if

        if (from > 0) call goto_char(from)

        do
            call re_search_forward(achar(13)//"+$", found, st, bound=pm, noerror=SEARCH_NOERROR)
            if (found == 0) exit
            call replace_match("", st, fixedcase=.true., literal=.true.)
        end do

        call goto_char(opoint)
    end subroutine strip_ctrl_m

    !> The command.  Strip trailing `^M' characters from the current
    !> output group.
    subroutine cmd_comint_strip_ctrl_m(status)
        type(status_type), intent(out) :: status

        call strip_ctrl_m(status, .true.)
    end subroutine cmd_comint_strip_ctrl_m

    !> Put the end of the buffer at the bottom of the window.  Port of
    !> comint.el:comint-show-maximum-output.
    subroutine comint_show_maximum_output(status)
        type(status_type), intent(out) :: status

        call goto_char(point_max())
        call recenter(make_int(-1_int64), .false., status)
    end subroutine comint_show_maximum_output

    ! ------------------------------------------------------------------
    ! Old input
    ! ------------------------------------------------------------------

    !> Default for `comint_get_old_input'.
    !> If `comint_use_prompt_regexp' is false, then either return the
    !> current input field, if point is on an input field, or the current
    !> line, if point is on an output field.  If `comint_use_prompt_regexp'
    !> is true, then return the current line with any initial string
    !> matching the regexp `comint_prompt_regexp' removed.  Port of
    !> comint.el:comint-get-old-input-default.
    recursive subroutine comint_get_old_input_default(input, status)
        character(len=:), allocatable, intent(out) :: input
        type(status_type), intent(out) :: status
        type(value_type) :: field_prop
        integer(int64) :: bof, start, end

        status = status_type()
        field_prop = nil()

        if (.not. comint_use_prompt_regexp%value) then
            bof = field_beginning(point(), .false.)
            field_prop = field_at(bof)

            ! Make sure we're in an input rather than output field.
            if (nilp(field_prop)) then
                input = field_string(bof)
                return
            end if
        end if

        call comint_bol(.false.)
        start = point()

        if (comint_use_prompt_regexp%value .or. symbol_is(field_prop, "output")) then
            end = line_end_position()
        else
            end = field_end(point(), .false.)
        end if

        call buffer_substring(start, end, input, status)
    end subroutine comint_get_old_input_default

    !> Insert after prompt old input at point as new input to be edited.
    !> Calls `comint_get_old_input' to get old input.  Port of
    !> comint.el:comint-copy-old-input.
    subroutine comint_copy_old_input(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: proc
        character(len=:), allocatable :: input

        call call_get_old_input(input, status)
        if (.not. status%ok()) return
        call current_process(proc, status)
        if (.not. status%ok()) return
        call goto_char(pmark_pos(proc))
        call insert(input, status)
    end subroutine comint_copy_old_input

    !> Skip past the text matching regexp `comint_prompt_regexp'.
    !> If this takes us past the end of the current line, don't skip at
    !> all.  Port of comint.el:comint-skip-prompt.
    subroutine comint_skip_prompt()
        type(status_type) :: st
        integer(int64) :: e

        if (looking_at(comint_prompt_regexp%value, st)) then
            e = match_end(0, st)
            if (e <= line_end_position()) call goto_char(e)
        end if
    end subroutine comint_skip_prompt

    !> Return true if point is after the process output marker.  Port of
    !> comint.el:comint-after-pmark-p.
    logical function comint_after_pmark_p()
        type(handle_type) :: proc

        comint_after_pmark_p = .false.
        proc = get_buffer_process(current_buffer())
        if (proc%index /= 0) comint_after_pmark_p = pmark_pos(proc) <= point()
    end function comint_after_pmark_p

    !> Return the buffer position of the beginning of the line, after any
    !> prompt.  If `comint_use_prompt_regexp' is true, then the prompt
    !> skip is done by skipping text matching the regular expression
    !> `comint_prompt_regexp', a buffer local variable.  Port of
    !> comint.el:comint-line-beginning-position.
    integer(int64) function comint_line_beginning_position() result(pos)
        integer(int64) :: opoint, bol

        if (comint_use_prompt_regexp%value) then
            ! Use comint-prompt-regexp.
            opoint = point()
            call beginning_of_line()
            call comint_skip_prompt()
            pos = point()
            call goto_char(opoint)
        else
            ! Use input fields.  Note that, unlike the behavior of
            ! `line-beginning-position' inside a field, this function will
            ! return the position of the end of a prompt, even if the point
            ! is already inside the prompt.  In order to do this, it assumes
            ! that if there are two fields on a line, then the first one is
            ! the prompt, and the second one is an input field, and is
            ! front-sticky (as input fields should be).
            if (symbol_is(field_at_pos(point()), "output")) then
                bol = line_beginning_position()
            else
                bol = field_beginning(point(), .false.)
            end if

            pos = constrain_to_field(bol, line_end_position(), .false., .false.)
        end if
    end function comint_line_beginning_position

    !> The field at POS, as editfns.c:Ffield_at_pos gives it (the field
    !> of the text before POS unless the field after it is front-sticky).
    !> Not a port.
    function field_at_pos(pos) result(v)
        use editfns_fm, only: get_pos_property
        integer(int64), intent(in) :: pos
        type(value_type) :: v

        v = get_pos_property(pos, sym("field"))
    end function field_at_pos

    !> Go to the beginning of line, then skip past the prompt, if any.
    !> If prefix argument is given (\[universal_argument]) the prompt is
    !> not skipped.  If `comint_use_prompt_regexp' is true, then the prompt
    !> skip is done by skipping text matching the regular expression
    !> `comint_prompt_regexp', a buffer local variable.  Port of
    !> comint.el:comint-bol.
    subroutine comint_bol(arg)
        logical, intent(in) :: arg
        integer(int64) :: shortage

        if (arg) then
            ! Unlike `beginning-of-line', forward-line ignores field
            ! boundaries.
            shortage = forward_line(0_int64)
        else
            call goto_char(comint_line_beginning_position())
        end if
    end subroutine comint_bol

    ! ------------------------------------------------------------------
    ! Output commands
    ! ------------------------------------------------------------------

    !> Delete all output from interpreter since last input.
    !> If KILL (interactively, the prefix), save the killed text in the
    !> kill ring.
    !>
    !> This command does not delete the prompt.  Port of
    !> comint.el:comint-delete-output.
    recursive subroutine comint_delete_output(status, kill)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: kill
        type(handle_type) :: proc
        type(value_type) :: inhibit
        type(status_type) :: st
        character(len=:), allocatable :: replacement, prompt
        integer(int64) :: opoint, pm, lie, shortage
        integer :: k

        call current_process(proc, status)
        if (.not. status%ok()) return
        k = local_entry()
        call inhibit_read_only_begin(inhibit)
        opoint = point()
        call goto_char(pmark_pos(proc))
        shortage = forward_line(0_int64)
        pm = point()
        lie = marker_pos(comint_buffers(k)%last_input_end)

        if (present(kill)) then
            if (kill) call copy_region_as_kill(lie, pm, st)
        end if

        call delete_region(lie, pm, st)
        pm = lie
        call goto_char(pmark_pos(proc))
        call buffer_substring(pm, point(), prompt, st)
        replacement = "*** output flushed ***"//achar(10)//prompt
        call delete_region(pm, point(), st)

        if (opoint <= point_max()) then
            call goto_char(opoint)
        else
            call goto_char(point_max())
        end if

        call inhibit_read_only_end(inhibit)
        ! Output message and put back prompt.
        call comint_output_filter(proc, replacement, status)
    end subroutine comint_delete_output

    !> The command: with a prefix argument, kill the output.
    !>
    !> Delete all output from interpreter since last input.
    recursive subroutine cmd_comint_delete_output(status)
        type(status_type), intent(out) :: status

        call comint_delete_output(status,.not. nilp(current_prefix_arg))
    end subroutine cmd_comint_delete_output

    !> Write output from interpreter since last input to FILENAME.
    !> Any prompt at the end of the output is not written.
    !>
    !> If the optional argument APPEND (the prefix argument when
    !> interactive) is true, the output is appended to the file instead
    !> (Deferred: write_region has no APPEND yet, an error says so).  Port of
    !> comint.el:comint-write-output (MUSTBENEW is Deferred).
    subroutine comint_write_output(filename, status, append)
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: append
        type(handle_type) :: proc
        integer(int64) :: opoint, shortage, e
        character(len=:), allocatable :: msg
        integer :: k

        if (present(append)) then
            if (append) then
                call set_error(status, FM_GENERIC_ERROR, "Appending output to a file is not supported yet")
                return
            end if
        end if

        call current_process(proc, status)
        if (.not. status%ok()) return
        k = local_entry()
        opoint = point()
        call goto_char(pmark_pos(proc))
        shortage = forward_line(0_int64)
        e = point()
        call goto_char(opoint)
        call write_region(filename, .false., status, start=marker_pos(comint_buffers(k)%last_input_end), end=e, &
                          message=msg)
        if (status%ok() .and. allocated(msg)) call message(msg)
    end subroutine comint_write_output

    !> The command: read the file name.
    !>
    !> Write output from interpreter since last input to FILENAME.
    subroutine cmd_comint_write_output(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        logical :: append

        append = .not. nilp(current_prefix_arg)

        if (append) then
            call read_file_name("Append output to file: ", file, status)
        else
            call read_file_name("Write output to file: ", file, status)
        end if

        if (.not. status%ok()) return
        call comint_write_output(file, status, append)
    end subroutine cmd_comint_write_output

    !> Append output from interpreter since last input to FILENAME.
    !> Any prompt at the end of the output is not written.  Port of
    !> comint.el:comint-append-output-to-file.
    subroutine comint_append_output_to_file(filename, status)
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status

        call comint_write_output(filename, status, .true.)
    end subroutine comint_append_output_to_file

    !> Display start of this batch of interpreter output at top of window.
    !> Sets mark to the value of point when this command is run.  Port of
    !> comint.el:comint-show-output.
    subroutine comint_show_output(status)
        type(status_type), intent(out) :: status
        type(status_type) :: st
        integer(int64) :: pos
        integer :: k

        status = status_type()
        call push_mark()
        pos = 0
        k = entry_of(cur)
        if (k /= 0) pos = marker_pos(comint_buffers(k)%last_input_end)
        if (pos == 0) pos = point_max()

        if (comint_use_prompt_regexp%value) then
            call goto_char(pos)
            call beginning_of_line(0_int64)
            call set_window_start(windows(selected_frame%selected_window%index), point(), st)
            call comint_skip_prompt()
        else
            call goto_char(field_beginning(pos, .false.))
            call set_window_start(windows(selected_frame%selected_window%index), point(), st)
        end if
    end subroutine comint_show_output

    !> Clear the comint buffer.  Port of comint.el:comint-clear-buffer.
    subroutine comint_clear_buffer(status)
        type(status_type), intent(out) :: status

        call truncate_to(0_int64, status)
    end subroutine comint_clear_buffer

    !> Interrupt the current subjob.  Port of
    !> comint.el:comint-interrupt-subjob.
    recursive subroutine comint_interrupt_subjob(status)
        type(status_type), intent(out) :: status

        call comint_skip_input(status)
        if (.not. status%ok()) return
        call interrupt_process(status, current_group=comint_ptyp())
    end subroutine comint_interrupt_subjob

    !> Send kill signal to the current subjob.  Port of
    !> comint.el:comint-kill-subjob.
    recursive subroutine comint_kill_subjob(status)
        type(status_type), intent(out) :: status

        call comint_skip_input(status)
        if (.not. status%ok()) return
        call kill_process(status, current_group=comint_ptyp())
    end subroutine comint_kill_subjob

    !> Send quit signal to the current subjob.  Port of
    !> comint.el:comint-quit-subjob.
    recursive subroutine comint_quit_subjob(status)
        type(status_type), intent(out) :: status

        call comint_skip_input(status)
        if (.not. status%ok()) return
        call quit_process(status, current_group=comint_ptyp())
    end subroutine comint_quit_subjob

    !> Skip all pending input, from last stuff output by interpreter to
    !> point.  This means mark it as if it had been sent as input, without
    !> sending it.  The command keys used to trigger the command that
    !> called this function are inserted into the buffer.  Port of
    !> comint.el:comint-skip-input.
    recursive subroutine comint_skip_input(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: proc, marker
        type(value_type) :: inhibit
        type(status_type) :: st
        integer(int64) :: pos
        character(len=:), allocatable :: keys

        call comint_send_input(status, .true., .true., ignore_input, .false.)
        if (.not. status%ok()) return
        call end_of_line()
        pos = point()
        call current_process(proc, status)
        if (.not. status%ok()) return
        marker = process_mark(proc)
        call inhibit_read_only_begin(inhibit)
        keys = ""
        if (allocated(this_command_keys)) keys = key_description(this_command_keys)
        call insert("  "//keys, st)
        if (marker_pos(marker) == pos) call set_marker(marker, point(), st)
        call inhibit_read_only_end(inhibit)
    end subroutine comint_skip_input

    !> Kill all text from last stuff output by interpreter to point.  Port of
    !> comint.el:comint-kill-input.
    subroutine comint_kill_input(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: proc
        integer(int64) :: pm

        call current_process(proc, status)
        if (.not. status%ok()) return
        pm = pmark_pos(proc)
        if (point() > pm) call kill_region(pm, point(), status)
    end subroutine comint_kill_input

    !> Behave like the global binding or send an EOF to subprocess.
    !> Sends an EOF only if point is at the end of the buffer and there is
    !> no input.  Port of comint.el:comint-delchar-or-maybe-eof.
    recursive subroutine comint_delchar_or_maybe_eof(status)
        use keymap_fm, only: keymapp
        use lisp_fm, only: make_keymap_ref
        type(status_type), intent(out) :: status
        type(handle_type) :: proc
        type(value_type) :: cmd, remap
        logical :: eof

        proc = get_buffer_process(current_buffer())
        eof = .false.
        if (eobp() .and. proc%index /= 0) eof = point() == pmark_pos(proc)

        if (eof) then
            call comint_send_eof(status)
        else
            call lookup_key(make_keymap_ref(current_global_map()), this_command_keys, cmd, status)
            if (.not. status%ok()) return
            call command_remapping(cmd, remap)
            if (.not. nilp(remap)) cmd = remap
            call call_interactively(cmd, status)
        end if
    end subroutine comint_delchar_or_maybe_eof

    !> Send an EOF to the current buffer's process.  Port of
    !> comint.el:comint-send-eof.
    recursive subroutine comint_send_eof(status)
        type(status_type), intent(out) :: status

        call comint_send_input(status, .true., .true.)
        if (.not. status%ok()) return
        call process_send_eof(status)
    end subroutine comint_send_eof

    !> Search backward through buffer for input fields that match REGEXP.
    !> If `comint_use_prompt_regexp' is true, then input fields are
    !> identified by lines that match `comint_prompt_regexp'.
    !>
    !> With prefix argument N, search for Nth previous match.
    !> If N is negative, find the next or Nth next match.  Port of
    !> comint.el:comint-backward-matching-input.
    subroutine comint_backward_matching_input(regexp, n_in, status)
        character(len=*), intent(in) :: regexp
        integer, intent(in) :: n_in
        type(status_type), intent(out) :: status
        integer(int64) :: opoint, found, pos
        integer :: n, dir
        type(value_type) :: f

        status = status_type()
        n = n_in
        opoint = point()

        if (comint_use_prompt_regexp%value) then
            ! Use comint-prompt-regexp.
            if (n > 0) then
                call end_of_line(0_int64)
            else
                call end_of_line(1_int64)
            end if

            call re_search_backward(comint_prompt_regexp%value//".*"//regexp, found, status, noerror=SEARCH_NOERROR, &
                                    count=int(n, int64))
            pos = point()
            call goto_char(opoint)

            if (found == 0) then
                call message("Not found")
                call ding(status=status)
            else
                call goto_char(pos)
                call comint_bol(.false.)
            end if
        else
            ! Use input fields.
            dir = merge(-1, 1, n < 0)

            do while (n /= 0)
                call re_search_backward(regexp, found, status, noerror=SEARCH_NOERROR, count=int(dir, int64))

                if (found == 0) then
                    call goto_char(opoint)
                    call set_error(status, FM_USER_ERROR, "Not found")
                    return
                end if

                f = field_at(point())
                if (nilp(f)) n = n - dir
            end do

            pos = field_beginning(point(), .false.)
            call goto_char(pos)
        end if
    end subroutine comint_backward_matching_input

    !> Search forward through buffer for input fields that match REGEXP.
    !> If `comint_use_prompt_regexp' is true, then input fields are
    !> identified by lines that match `comint_prompt_regexp'.
    !>
    !> With prefix argument N, search for Nth following match.
    !> If N is negative, find the previous or Nth previous match.  Port of
    !> comint.el:comint-forward-matching-input.
    subroutine comint_forward_matching_input(regexp, n, status)
        character(len=*), intent(in) :: regexp
        integer, intent(in) :: n
        type(status_type), intent(out) :: status

        call comint_backward_matching_input(regexp, -n, status)
    end subroutine comint_forward_matching_input

    !> Move to end of Nth next prompt in the buffer.
    !> If `comint_use_prompt_regexp' is false, then this means the
    !> beginning of the Nth next `input' field, otherwise, it means the Nth
    !> occurrence of text matching `comint_prompt_regexp'.  Port of
    !> comint.el:comint-next-prompt.
    subroutine comint_next_prompt(n_in)
        integer, intent(in) :: n_in
        character(len=:), allocatable :: saved
        integer(int64) :: pos, input_pos, prev_pos, left
        logical :: have_saved, have_input
        integer :: n

        n = n_in

        if (comint_use_prompt_regexp%value) then
            ! Use comint-prompt-regexp.
            have_saved = allocated(paragraph_start%value)
            if (have_saved) saved = paragraph_start%value
            paragraph_start%value = comint_prompt_regexp%value

            if (n > 0) then
                call end_of_line(1_int64)
            else
                call end_of_line(0_int64)
            end if

            left = forward_paragraph(int(n, int64))
            call comint_skip_prompt()

            if (have_saved) then
                paragraph_start%value = saved
            else
                deallocate (paragraph_start%value)
            end if
        else
            ! Use input fields.
            pos = point()
            have_input = .false.
            input_pos = 0

            do while (n /= 0)
                prev_pos = pos

                if (n > 0) then
                    pos = next_single_char_property_change(pos, sym("field"))
                else
                    pos = previous_single_char_property_change(pos, sym("field"))
                end if

                if (pos == prev_pos) then
                    ! Ran off the end of the buffer.
                    if (n > 0) then
                        ! There's always an input field at the end of the
                        ! buffer, but it has a `field' property of nil.
                        input_pos = point_max()
                        have_input = .true.
                    end if

                    ! Stop iterating.
                    n = 0
                else if (nilp(field_at(pos))) then
                    if (n < 0) then
                        n = n + 1
                    else
                        n = n - 1
                    end if

                    input_pos = pos
                    have_input = .true.
                end if
            end do

            if (have_input) call goto_char(input_pos)
        end if
    end subroutine comint_next_prompt

    !> Move to end of Nth previous prompt in the buffer.
    !> If `comint_use_prompt_regexp' is false, then this means the
    !> beginning of the Nth previous `input' field, otherwise, it means the
    !> Nth occurrence of text matching `comint_prompt_regexp'.  Port of
    !> comint.el:comint-previous-prompt.
    subroutine comint_previous_prompt(n)
        integer, intent(in) :: n

        call comint_next_prompt(-n)
    end subroutine comint_previous_prompt

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
    subroutine comint_update_fence()
        type(silent_modifications_type) :: silent
        type(status_type) :: st
        type(value_type) :: lst, ro, prev_ro
        integer(int64) :: pt
        logical :: sticky, changed

        pt = point()
        if (.not. bolp() .or. bobp()) return
        call get_text_property(pt, sym("front_sticky"), lst, st)
        call get_text_property(pt, sym("read_only"), ro, st)
        call silent_modifications_begin(silent)
        sticky = .false.
        if (.not. nilp(ro)) sticky = front_sticky_read_only(lst)
        call get_text_property(pt - 1, sym("read_only"), prev_ro, st)

        if (sticky) then
            if (nilp(prev_ro)) call put_text_property(pt - 1, pt, sym("read_only"), sym("fence"), st)
        else if (symbol_is(prev_ro, "fence")) then
            call remove_list_of_text_properties(pt - 1, pt, list(sym("read_only")), changed, st)
        end if

        call silent_modifications_end(silent)
    end subroutine comint_update_fence

    !> Whether a front-sticky value LST covers `read_only': (if (listp lst)
    !> (memq 'read-only lst) t).  Not a port.
    logical function front_sticky_read_only(lst) result(yes)
        type(value_type), intent(in) :: lst
        type(value_type), allocatable :: elts(:)
        integer :: i

        if (nilp(lst)) then
            yes = .false.
        else if (consp(lst)) then
            yes = .false.
            call list_elements(lst, elts)

            do i = 1, size(elts)
                if (symbol_is(elts(i), "read_only")) yes = .true.
            end do
        else
            yes = .true.
        end if
    end function front_sticky_read_only

    !> Kill current line, ignoring read-only and field properties.
    !> With prefix arg COUNT, kill that many lines starting from the
    !> current line.  If COUNT is negative, kill backward.  Also kill the
    !> preceding newline, instead of the trailing one.
    !> If COUNT is zero, kill current line but exclude the trailing
    !> newline.  The read-only status of newlines is updated with
    !> `comint_update_fence', if necessary.  Port of
    !> comint.el:comint-kill-whole-line.
    subroutine comint_kill_whole_line(count, status)
        integer(int64), intent(in) :: count
        type(status_type), intent(out) :: status
        type(value_type) :: inhibit
        logical :: saved

        call inhibit_read_only_begin(inhibit)
        saved = inhibit_field_text_motion
        inhibit_field_text_motion = .true.
        call kill_whole_line(count, status)
        if (count >= 0) call comint_update_fence()
        inhibit_field_text_motion = saved
        call inhibit_read_only_end(inhibit)
    end subroutine comint_kill_whole_line

    !> Like `kill_region', but ignore read-only properties, if safe.
    !> This command assumes that the buffer contains read-only "prompts"
    !> which are regions with front-sticky read-only properties at the
    !> beginning of a line, with the preceding newline being read-only to
    !> protect the prompt.  This is true of the comint prompts if
    !> `comint_prompt_read_only' is true.  This command will not delete the
    !> region if this would create mutilated or out of place prompts.  That
    !> is, if any part of a prompt is deleted, the entire prompt must be
    !> deleted and all remaining prompts should stay at the beginning of a
    !> line.  If this is not the case, this command just calls
    !> `kill_region' with all read-only properties intact.  The read-only
    !> status of newlines is updated using `comint_update_fence', if
    !> necessary.  Port of comint.el:comint-kill-region.
    subroutine comint_kill_region(beg, end, status)
        integer(int64), intent(in) :: beg, end
        type(status_type), intent(out) :: status
        type(value_type) :: inhibit, lst, ro
        type(status_type) :: st
        integer(int64) :: opoint, true_beg, true_end
        logical :: beg_bolp, end_bolp, beg_bad, end_bad

        opoint = point()
        true_beg = min(beg, end)
        true_end = max(beg, end)
        call goto_char(true_beg)
        beg_bolp = bolp()
        call get_text_property(true_beg, sym("front_sticky"), lst, st)
        call get_text_property(true_beg, sym("read_only"), ro, st)
        beg_bad = .false.
        if (.not. nilp(ro)) beg_bad = front_sticky_read_only(lst)
        call goto_char(true_end)
        end_bolp = bolp()
        call get_text_property(true_end, sym("front_sticky"), lst, st)
        call get_text_property(true_end, sym("read_only"), ro, st)
        end_bad = .false.
        if (.not. nilp(ro)) end_bad = front_sticky_read_only(lst)
        call goto_char(opoint)

        if ((.not. beg_bolp .and. (beg_bad .or. end_bad)) .or. (.not. end_bolp .and. end_bad)) then
            call kill_region(beg, end, status)
        else
            call inhibit_read_only_begin(inhibit)
            call kill_region(beg, end, status)
            opoint = point()
            call comint_update_fence()
            call goto_char(opoint)
            call inhibit_read_only_end(inhibit)
        end if
    end subroutine comint_kill_region

    !> After fetching a line from input history, this fetches the following
    !> line.  In other words, this recalls the input line after the line
    !> you recalled last.  You can use this to repeat a sequence of input
    !> lines.  Port of comint.el:comint-get-next-from-history.
    subroutine comint_get_next_from_history(status)
        type(status_type), intent(out) :: status
        integer :: k

        status = status_type()
        k = entry_of(cur)

        if (k /= 0) then
            if (comint_buffers(k)%save_input_ring_index >= 0) then
                comint_buffers(k)%input_ring_index = comint_buffers(k)%save_input_ring_index + 1
                call comint_next_input(1, status)
                return
            end if
        end if

        call message("No previous history command")
    end subroutine comint_get_next_from_history

    !> Accumulate a line to send as input along with more lines.
    !> This inserts a newline so that you can enter more text to be sent
    !> along with this line.  Use \[comint_send_input] to send all the
    !> accumulated input, at once.  The entire accumulated text becomes one
    !> item in the input history when you send it.  Port of
    !> comint.el:comint-accumulate.
    subroutine comint_accumulate(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: proc
        type(value_type) :: inhibit
        type(status_type) :: st
        character(len=:), allocatable :: text
        integer(int64) :: pm, am
        integer :: k

        status = status_type()
        k = local_entry()
        proc = get_buffer_process(current_buffer())

        if (proc%index /= 0) then
            pm = pmark_pos(proc)
            am = marker_pos(comint_buffers(k)%accum_marker)

            if (am == 0) then
                call set_marker(comint_buffers(k)%accum_marker, pm, st)
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
        k = local_entry()
        call set_marker(comint_buffers(k)%accum_marker, point(), st)

        if (comint_buffers(k)%input_ring_index >= 0) then
            comint_buffers(k)%save_input_ring_index = comint_buffers(k)%input_ring_index - 1
        end if
    end subroutine comint_accumulate

    !> Move point to the process mark.
    !> The process mark separates output, and input already sent, from
    !> input that has not yet been sent.  Port of
    !> comint.el:comint-goto-process-mark.
    subroutine comint_goto_process_mark(status, interactive)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: interactive
        type(handle_type) :: proc

        call current_process(proc, status)
        if (.not. status%ok()) return
        call goto_char(pmark_pos(proc))

        if (present(interactive)) then
            if (interactive) call message("Point is now at the process mark")
        end if
    end subroutine comint_goto_process_mark

    !> The command.  Move point to the process mark.
    subroutine cmd_comint_goto_process_mark(status)
        type(status_type), intent(out) :: status

        call comint_goto_process_mark(status, .true.)
    end subroutine cmd_comint_goto_process_mark

    !> Move point to beginning of line (after prompt) or to the process
    !> mark.  The first time you use this command, it moves to the
    !> beginning of the line (but after the prompt, if any).  If you repeat
    !> it again immediately, it moves point to the process mark.
    !>
    !> The process mark separates the process output, along with input
    !> already sent, from input that has not yet been sent.  Ordinarily,
    !> the process mark is at the beginning of the current input line; but
    !> if you have used \[comint_accumulate] to send multiple lines at once,
    !> the process mark is at the beginning of the accumulated input.  Port of
    !> comint.el:comint-bol-or-process-mark.
    subroutine comint_bol_or_process_mark(status)
        type(status_type), intent(out) :: status

        status = status_type()

        if (.not. symbol_is(last_command, "comint_bol_or_process_mark")) then
            call comint_bol(.false.)
        else
            call comint_goto_process_mark(status)
        end if
    end subroutine comint_bol_or_process_mark

    !> Set the process mark at point.  Port of
    !> comint.el:comint-set-process-mark.
    subroutine comint_set_process_mark(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: proc
        type(status_type) :: st

        call current_process(proc, status)
        if (.not. status%ok()) return
        call set_marker(process_mark(proc), point(), st)
        call message("Process mark set")
    end subroutine comint_set_process_mark

    ! ------------------------------------------------------------------
    ! The commands' interactive forms
    ! ------------------------------------------------------------------

    !> The count of the prefix argument.  Not a port.
    integer function prefix_count()
        prefix_count = int(prefix_numeric_value(current_prefix_arg))
    end function prefix_count

    !> Cycle backwards through input history, saving input.
    subroutine cmd_comint_previous_input(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call comint_previous_input(prefix_count(), status)
    end subroutine cmd_comint_previous_input

    !> Cycle forwards through input history.
    subroutine cmd_comint_next_input(status)
        type(status_type), intent(out) :: status

        call barf_if_buffer_read_only(status)
        if (status%ok()) call comint_next_input(prefix_count(), status)
    end subroutine cmd_comint_next_input

    !> Search backwards through input history for match for REGEXP.
    subroutine cmd_comint_previous_matching_input(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp
        integer :: n

        call comint_regexp_arg("Previous input matching (regexp): ", regexp, n, status)
        if (status%ok()) call comint_previous_matching_input(regexp, n, status)
    end subroutine cmd_comint_previous_matching_input

    !> Search forwards through input history for match for REGEXP.
    subroutine cmd_comint_next_matching_input(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp
        integer :: n

        call comint_regexp_arg("Next input matching (regexp): ", regexp, n, status)
        if (status%ok()) call comint_next_matching_input(regexp, n, status)
    end subroutine cmd_comint_next_matching_input

    !> Search backwards through input history for match for current input.
    subroutine cmd_comint_previous_matching_input_from_input(status)
        type(status_type), intent(out) :: status

        call comint_previous_matching_input_from_input(prefix_count(), status)
    end subroutine cmd_comint_previous_matching_input_from_input

    !> Search forwards through input history for match for current input.
    subroutine cmd_comint_next_matching_input_from_input(status)
        type(status_type), intent(out) :: status

        call comint_next_matching_input_from_input(prefix_count(), status)
    end subroutine cmd_comint_next_matching_input_from_input

    !> Search backward through buffer for input fields that match REGEXP.
    subroutine cmd_comint_backward_matching_input(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp
        integer :: n

        call comint_regexp_arg("Backward input matching (regexp): ", regexp, n, status)
        if (status%ok()) call comint_backward_matching_input(regexp, n, status)
    end subroutine cmd_comint_backward_matching_input

    !> Search forward through buffer for input fields that match REGEXP.
    subroutine cmd_comint_forward_matching_input(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regexp
        integer :: n

        call comint_regexp_arg("Forward input matching (regexp): ", regexp, n, status)
        if (status%ok()) call comint_forward_matching_input(regexp, n, status)
    end subroutine cmd_comint_forward_matching_input

    !> Move to end of Nth next prompt in the buffer.
    subroutine cmd_comint_next_prompt(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call comint_next_prompt(prefix_count())
    end subroutine cmd_comint_next_prompt

    !> Move to end of Nth previous prompt in the buffer.
    subroutine cmd_comint_previous_prompt(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call comint_previous_prompt(prefix_count())
    end subroutine cmd_comint_previous_prompt

    !> Go to the beginning of line, then skip past the prompt, if any.
    subroutine cmd_comint_bol(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call comint_bol(.not. nilp(current_prefix_arg))
    end subroutine cmd_comint_bol

    !> Kill current line, ignoring read-only and field properties.
    subroutine cmd_comint_kill_whole_line(status)
        type(status_type), intent(out) :: status

        call comint_kill_whole_line(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_comint_kill_whole_line

    !> Like `kill_region', but ignore read-only properties, if safe.
    subroutine cmd_comint_kill_region(status)
        type(status_type), intent(out) :: status
        integer(int64) :: b, e

        call interactive_region(b, e, status)
        if (.not. status%ok()) return
        call comint_kill_region(b, e, status)
    end subroutine cmd_comint_kill_region

    !> Append output from interpreter since last input to FILENAME.
    subroutine cmd_comint_append_output_to_file(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file

        call read_file_name("Append output to file: ", file, status)
        if (status%ok()) call comint_append_output_to_file(file, status)
    end subroutine cmd_comint_append_output_to_file

    ! ------------------------------------------------------------------
    ! History expansion
    ! ------------------------------------------------------------------

    !> Return the string ARG places along the input ring.
    !> Moves relative to `comint_input_ring_index'.  Port of
    !> comint.el:comint-previous-input-string.
    function comint_previous_input_string(arg) result(item)
        integer, intent(in) :: arg
        character(len=:), allocatable :: item
        type(status_type) :: st
        integer :: k

        item = ""
        k = entry_of(cur)
        if (k == 0) return
        if (.not. comint_buffers(k)%has_ring) return

        if (comint_buffers(k)%input_ring_index >= 0) then
            call comint_buffers(k)%input_ring%ref(arg + comint_buffers(k)%input_ring_index, item, st)
        else
            call comint_buffers(k)%input_ring%ref(arg, item, st)
        end if
    end function comint_previous_input_string

    ! The length of the input ring (0 without one).  Not a port.
    integer function ring_length()
        integer :: k

        ring_length = 0
        k = entry_of(cur)
        if (k == 0) return
        if (comint_buffers(k)%has_ring) ring_length = comint_buffers(k)%input_ring%count()
    end function ring_length

    ! (setq comint-input-ring-index INDEX), -1 for nil.  Not a port.
    subroutine set_input_ring_index(index)
        integer, intent(in) :: index
        integer :: k

        k = local_entry()
        comint_buffers(k)%input_ring_index = index
    end subroutine set_input_ring_index

    ! comint-input-ring-index, -1 for nil.  Not a port.
    integer function input_ring_index()
        integer :: k

        input_ring_index = -1
        k = entry_of(cur)
        if (k > 0) input_ring_index = comint_buffers(k)%input_ring_index
    end function input_ring_index

    !> Return true if the number of quotes between BEG and END is odd.
    !> Quotes are single and double.  Port of
    !> comint.el:comint-within-quotes.
    logical function comint_within_quotes(beg, end)
        integer(int64), intent(in) :: beg, end
        integer :: countsq, countdq

        countsq = comint_how_many_region("\(^\|[^\]\)'", beg, end)
        countdq = comint_how_many_region("\(^\|[^\]\)""", beg, end)
        comint_within_quotes = modulo(countsq, 2) == 1 .or. modulo(countdq, 2) == 1
    end function comint_within_quotes

    !> Return number of matches for REGEXP from BEG to END.  Port of
    !> comint.el:comint-how-many-region.
    integer function comint_how_many_region(regexp, beg, end) result(count)
        use search_fm, only: re_search_forward, match_data, set_match_data
        character(len=*), intent(in) :: regexp
        integer(int64), intent(in) :: beg, end
        integer(int64), allocatable :: saved(:)
        type(status_type) :: st
        integer(int64) :: opoint, pos

        count = 0
        opoint = point()
        saved = match_data()
        call goto_char(beg)

        do
            call re_search_forward(regexp, pos, st, bound=end, noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. pos == 0) exit
            count = count + 1
        end do

        call set_match_data(saved)
        call goto_char(opoint)
    end function comint_how_many_region

    ! From STRING, return the args depending on the range specified in the
    ! text from BEGIN to END.  If BEGIN is 0, assume all args.  Ignore
    ! leading `:'.  Range can be x-y, x-, -y, where x/y can be [0-9], *,
    ! ^, $.  Port of comint.el:comint-args.
    function comint_args(string, begin, end) result(args)
        use search_fm, only: match_data, set_match_data
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: begin, end
        character(len=:), allocatable :: args
        character(len=:), allocatable :: range
        integer(int64), allocatable :: saved(:)
        type(status_type) :: st
        integer :: nth, mth, d
        logical :: has_nth, has_mth
        integer(int64) :: b

        saved = match_data()

        if (begin == 0) then
            args = comint_arguments(string, 0)
        else
            b = begin
            if (char_after_is(begin, ":")) b = begin + 1
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
                nth = string_to_int(range)
            end if

            has_mth = .true.

            if (smatch("[-*$]$", range)) then
                has_mth = .false.
                mth = 0
            else if (index(range, "-") > 0) then
                d = index(range, "-")
                mth = string_to_int(range(d + 1:))
            else
                has_mth = has_nth
                mth = nth
            end if

            if (has_nth .and. has_mth) then
                args = comint_arguments(string, nth, mth)
            else if (has_nth) then
                args = comint_arguments(string, nth=nth)
            else if (has_mth) then
                args = comint_arguments(string, mth=mth)
            else
                args = comint_arguments(string)
            end if
        end if

        call set_match_data(saved)

    contains

        ! (string-to-number S): the leading integer, 0 if none.
        integer function string_to_int(s)
            character(len=*), intent(in) :: s
            integer :: j, ios

            j = 0

            do while (j < len(s))
                if (verify(s(j + 1:j + 1), "0123456789") /= 0) exit
                j = j + 1
            end do

            string_to_int = 0

            if (j > 0) then
                read (s(1:j), *, iostat=ios) string_to_int
                if (ios /= 0) string_to_int = 0
            end if
        end function string_to_int
    end function comint_args

    ! Whether the character at POS is C.  Not a port.
    logical function char_after_is(pos, c)
        integer(int64), intent(in) :: pos
        character(len=1), intent(in) :: c
        character(len=:), allocatable :: s
        type(status_type) :: st

        char_after_is = .false.
        if (pos < point_min() .or. pos >= point_max()) return
        call buffer_substring(pos, pos + 1, s, st)
        char_after_is = s == c
    end function char_after_is

    ! Whether REGEXP matches STR (without changing the match data).
    ! Not a port.
    logical function smatch(regexp, str)
        character(len=*), intent(in) :: regexp, str
        type(status_type) :: st

        smatch = string_match(regexp, str, st, inhibit_modify=.true.) > 0
    end function smatch

    !> Expand input command history references before point.
    !> Expansion is dependent on the value of `comint_input_autoexpand'.
    !>
    !> This function depends on the buffer's idea of the input history,
    !> which may not match the command interpreter's idea, assuming it has
    !> one.
    !>
    !> Assumes history syntax is like typical Un*x shells'.  However,
    !> since Emacs cannot know the interpreter's idea of input line
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
    !> See `comint_magic_space' and
    !> `comint_replace_by_expanded_history_before_point'.
    !>
    !> Returns true if successful.  Port of
    !> comint.el:comint-replace-by-expanded-history.
    recursive logical function comint_replace_by_expanded_history(status, silent, start) result(done)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: silent
        integer(int64), intent(in), optional :: start
        integer(int64) :: tick

        status = status_type()
        done = .false.
        if (.not. history_references_p(silent, start)) return
        ! Looks like there might be history references in the command.
        tick = buffers(cur)%text%modiff
        call comint_replace_by_expanded_history_before_point(silent, status, start)
        done = tick /= buffers(cur)%text%modiff
    end function comint_replace_by_expanded_history

    ! Whether there are input command history references at point to
    ! expand (comint-c-a-p-replace-by-expanded-history returns the
    ! function doing the expansion then).  Port of
    ! comint.el:comint-c-a-p-replace-by-expanded-history, in part.
    recursive logical function history_references_p(silent, start) result(applies)
        logical, intent(in), optional :: silent
        integer(int64), intent(in), optional :: start
        type(value_type) :: field
        type(status_type) :: st
        integer(int64) :: opoint
        integer :: kind

        applies = .false.
        if (comint_input_autoexpand%value == COMINT_AUTOEXPAND_NIL) return

        if (comint_use_prompt_regexp%value) then
            ! Use comint-prompt-regexp
            opoint = point()
            call beginning_of_line()
            applies = looking_at(comint_prompt_regexp%value//"!\|\^", st)
            call goto_char(opoint)
        else
            ! Use input fields.  User input that hasn't been entered yet,
            ! at the end of the buffer, has a nil `field' property.
            field = get_char_property(point(), sym("field"))
            if (nilp(field)) applies = smatch("!\|^\^", field_string(point()))
        end if

        if (.not. applies) return
        call comint_replace_by_expanded_history_before_point(silent, st, start, dry_run=.true., kind=kind)
        applies = kind /= DRY_RUN_NONE
    end function history_references_p

    !> Expand directory stack reference before point.
    !> See `comint_replace_by_expanded_history'.
    !>
    !> If the optional argument START is present, that specifies the start
    !> of the text to scan for history references, rather than the logical
    !> beginning of line.
    !>
    !> If DRY_RUN is true, return in KIND what the first reference would
    !> do before performing any actual side-effect (upstream returns it to
    !> DRY-RUN).  Port of
    !> comint.el:comint-replace-by-expanded-history-before-point.
    recursive subroutine comint_replace_by_expanded_history_before_point(silent, status, start, dry_run, kind)
        logical, intent(in), optional :: silent
        type(status_type), intent(out) :: status
        integer(int64), intent(in), optional :: start
        logical, intent(in), optional :: dry_run
        integer, intent(out), optional :: kind
        character(len=:), allocatable :: exp, pref, old, new, input
        type(status_type) :: st
        type(handle_type) :: opoint
        integer(int64) :: toend, beg, moved, mb1, me1, mb2, me2, mb4, me4, pos, mb0, me0
        integer :: number, hpos
        logical :: quiet, dry, matched, found

        status = status_type()
        quiet = .false.
        if (present(silent)) quiet = silent
        dry = .false.
        if (present(dry_run)) dry = dry_run
        if (present(kind)) kind = DRY_RUN_NONE
        call copy_marker_at(point(), opoint, .false., st)
        toend = line_end_position() - point()

        if (present(start)) then
            beg = start
        else
            beg = comint_line_beginning_position()
        end if

        call goto_char(beg)

        do
            moved = skip_chars_forward("^!^", st, line_end_position() - toend)
            if (point() >= line_end_position() - toend) exit
            ! This seems a bit complex.  We look for references such as !!,
            ! !-num, !foo, !?foo, !{bar}, !?{bar}, ^oh, ^my^, ^god^it,
            ! ^never^ends^.  If that wasn't enough, the plings can be
            ! suffixed with argument range specifiers.  Argument ranges
            ! are complex too, so we hive off the input line, referenced
            ! with plings, with the range string to `comint-args'.
            call set_input_ring_index(-1)

            if (char_after_is(point() - 1, "\") .or. comint_within_quotes(beg, point())) then
                ! The history is quoted, or we're in quotes.
                call goto_char(point() + 1)
            else if (looking_at("![0-9]+\($\|[^-]\)", st)) then
                ! We cannot know the interpreter's idea of input line
                ! numbers.
                if (dry) then
                    if (present(kind)) kind = DRY_RUN_MESSAGE
                    exit
                end if

                call goto_char(match_end(0, st))
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
                    call replace_match(comint_args(comint_previous_input_string(number), mb2, me2), st, &
                                       fixedcase=.true., literal=.true.)
                    call set_input_ring_index(number)
                    call message("History item: "//int_str(number + 1))
                else
                    call goto_char(match_end(0, st))
                    call message("Relative reference exceeds input history size")
                end if
            else if (looking_at_either("!!?:?\([0-9^$*-]+\)", "!!")) then
                ! Just a number of args from the previous input line.
                if (dry) then
                    if (present(kind)) kind = DRY_RUN_EXPAND
                    exit
                end if

                call replace_match(comint_args(comint_previous_input_string(0), match_beginning(1, st), &
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

                call comint_previous_matching_input_string_position(pref//regexp_quote(exp), 1, hpos, st)
                ! (restore the match data the search changed)
                matched = looking_at("!\??\({\(.+\)}\|\(\sw+\)\):?\([0-9^$*-]+\)?", st)

                if (hpos < 0) then
                    call goto_char(me0)

                    if (.not. quiet) then
                        call message("Not found")
                        call ding()
                    end if
                else
                    call set_input_ring_index(hpos)
                    call comint_input_ring_ref(hpos, input)
                    call replace_match(comint_args(input, mb4, me4), st, fixedcase=.true., literal=.true.)
                    call message("History item: "//int_str(hpos + 1))
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
                call replace_match(comint_previous_input_string(0), st, fixedcase=.true., literal=.true.)
                pos = point()
                call goto_char(mb0)
                call search_forward(old, found, st, bound=pos)

                if (.not. found) then
                    if (.not. quiet) then
                        call set_error(status, FM_USER_ERROR, "Not found")
                        exit
                    end if
                else
                    call replace_match(new, st, fixedcase=.true., literal=.true.)
                    call message("History item: substituted")
                end if
            else
                call goto_char(point() + 1)
            end if
        end do

        call goto_char(marker_pos(opoint))
        call free_marker(opoint, st)

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
    end subroutine comint_replace_by_expanded_history_before_point

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
        use search_fm, only: search_forward_fm => search_forward
        character(len=*), intent(in) :: string
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        integer(int64), intent(in) :: bound
        integer(int64) :: pos

        call search_forward_fm(string, pos, status, bound=bound, noerror=SEARCH_NOERROR)
        found = status%ok() .and. pos > 0
    end subroutine search_forward

    ! N as decimal digits.  Not a port.
    function int_str(n) result(s)
        integer, intent(in) :: n
        character(len=:), allocatable :: s
        character(len=16) :: buf

        write (buf, "(i0)") n
        s = trim(buf)
    end function int_str

    !> Expand input history references before point and insert ARG
    !> spaces.  A useful command to bind to SPC.  See
    !> `comint_replace_by_expanded_history'.  Port of
    !> comint.el:comint-magic-space.
    recursive subroutine comint_magic_space(arg, status)
        integer(int64), intent(in) :: arg
        type(status_type), intent(out) :: status
        logical :: done

        done = comint_replace_by_expanded_history(status)
        if (.not. status%ok()) return
        call self_insert_command(arg, int(iachar(" "), int64), status)
    end subroutine comint_magic_space

    recursive subroutine cmd_comint_magic_space(status)
        type(status_type), intent(out) :: status

        call comint_magic_space(prefix_numeric_value(current_prefix_arg), status)
    end subroutine cmd_comint_magic_space

    recursive subroutine cmd_comint_replace_by_expanded_history(status)
        type(status_type), intent(out) :: status
        logical :: done

        done = comint_replace_by_expanded_history(status)
    end subroutine cmd_comint_replace_by_expanded_history

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
    recursive subroutine comint_insert_previous_argument(index, has_index, status)
        integer, intent(in) :: index
        logical, intent(in) :: has_index
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: input_string
        type(status_type) :: st
        integer :: k, idx
        logical :: has

        status = status_type()
        k = local_entry()
        idx = index
        has = has_index

        if (value_eq(last_command, this_command) .and. .not. nilp(this_command)) then
            ! Delete last input inserted by this command.
            call delete_region(marker_pos(comint_buffers(k)%insert_previous_argument_last_start_pos), point(), st)
            idx = comint_buffers(k)%insert_previous_argument_last_index
            has = comint_buffers(k)%has_insert_previous_argument_last_index
        else
            ! This is a non-repeat invocation, so initialize state.
            if (has .and. comint_insert_previous_argument_from_end%value) idx = -idx
            comint_buffers(k)%input_ring_index = -1
            comint_buffers(k)%insert_previous_argument_last_index = idx
            comint_buffers(k)%has_insert_previous_argument_last_index = has

            ! First usage; initialize to a marker
            if (comint_buffers(k)%insert_previous_argument_last_start_pos%index == 0) then
                call make_marker(comint_buffers(k)%insert_previous_argument_last_start_pos)
            end if
        end if

        ! Make sure we're not in the prompt, and add a beginning space if
        ! necessary.
        if (point() <= comint_line_beginning_position()) then
            call comint_bol(.false.)
        else
            call just_one_space(1_int64, st)
        end if

        ! Remember the beginning of what we insert, so we can delete it if
        ! the command is repeated.
        call set_marker(comint_buffers(k)%insert_previous_argument_last_start_pos, point(), st)
        ! Insert the argument.
        input_string = comint_previous_input_string(0)

        if (has) then
            call insert(comint_arguments(input_string, idx, idx), st)
        else
            call insert(comint_arguments(input_string), st)
        end if

        ! Make next invocation return arg from previous input
        k = local_entry()
        comint_buffers(k)%input_ring_index = max(comint_buffers(k)%input_ring_index, 0) + 1
        ! Add a terminating space if necessary.
        if (.not. eolp()) call just_one_space(1_int64, st)
    end subroutine comint_insert_previous_argument

    recursive subroutine cmd_comint_insert_previous_argument(status)
        type(status_type), intent(out) :: status

        if (nilp(current_prefix_arg)) then
            call comint_insert_previous_argument(0, .false., status)
        else
            call comint_insert_previous_argument(int(prefix_numeric_value(current_prefix_arg)), .true., status)
        end if
    end subroutine cmd_comint_insert_previous_argument

    ! ------------------------------------------------------------------
    ! Isearch in comint input history
    ! ------------------------------------------------------------------

    !> Search for a string backward in input history using Isearch.  Port of
    !> comint.el:comint-history-isearch-backward.
    recursive subroutine comint_history_isearch_backward(status)
        type(status_type), intent(out) :: status

        status = status_type()
        comint_force_history_isearch = .true.
        call isearch_mode(.false., .false.)
    end subroutine comint_history_isearch_backward

    !> Search for a regular expression backward in input history using
    !> Isearch.  Port of comint.el:comint-history-isearch-backward-regexp.
    recursive subroutine comint_history_isearch_backward_regexp(status)
        type(status_type), intent(out) :: status

        status = status_type()
        comint_force_history_isearch = .true.
        call isearch_mode(.false., .true.)
    end subroutine comint_history_isearch_backward_regexp

    ! The beginning of the line of the prompt: (save-excursion (goto-char
    ! (comint-line-beginning-position)) (forward-line 0) (point)).  Not a port.
    integer(int64) function prompt_line_start()
        integer(int64) :: opoint

        opoint = point()
        call goto_char(comint_line_beginning_position())
        call beginning_of_line()
        prompt_line_start = point()
        call goto_char(opoint)
    end function prompt_line_start

    !> Set up a comint for using Isearch to search the input history.
    !> Intended to be added to `isearch_mode_hook' in `comint_mode'.  Port of
    !> comint.el:comint-history-isearch-setup (no lazy count).
    recursive subroutine comint_history_isearch_setup(status)
        type(status_type), intent(out) :: status

        status = status_type()
        ! Prompt is not empty like in Async Shell Command buffers or in
        ! finished shell buffers
        if (prompt_line_start() == comint_line_beginning_position()) return
        if (.not. (comint_force_history_isearch .or. comint_history_isearch%value == COMINT_HISTORY_ISEARCH_T .or. &
                   (comint_history_isearch%value == COMINT_HISTORY_ISEARCH_DWIM .and. comint_after_pmark_p()))) return
        isearch_message_prefix_add = "history "
        call isearch_search_fun_function%set_local(comint_history_isearch_search)
        call isearch_message_function%set_local(comint_history_isearch_message)
        call isearch_wrap_function%set_local(comint_history_isearch_wrap)
        call isearch_push_state_function%set_local(comint_history_isearch_push_state)
        call isearch_mode_end_hook%add(comint_history_isearch_end, local=.true.)
    end subroutine comint_history_isearch_setup

    !> Clean up the comint after terminating Isearch in comint.  Port of
    !> comint.el:comint-history-isearch-end.
    recursive subroutine comint_history_isearch_end(status)
        type(status_type), intent(out) :: status
        type(status_type) :: st
        integer :: k

        status = status_type()
        k = local_entry()

        if (overlayp(comint_buffers(k)%history_isearch_message_overlay)) then
            call delete_overlay(comint_buffers(k)%history_isearch_message_overlay)
        end if

        if (allocated(isearch_message_prefix_add)) deallocate (isearch_message_prefix_add)
        call isearch_search_fun_function%clear_local()
        call isearch_message_function%clear_local()
        call isearch_wrap_function%clear_local()
        call isearch_push_state_function%clear_local()
        ! Force isearch to not change mark.
        call set_isearch_opoint(point())
        call isearch_mode_end_hook%remove(comint_history_isearch_end, local=.true.)
        ! (isearch-suspended is not ported: it is always nil here.)
        comint_force_history_isearch = .false.
    end subroutine comint_history_isearch_end

    !> Put input history item of the absolute history position POS (-1
    !> for none).  Port of comint.el:comint-goto-input.
    recursive subroutine comint_goto_input(pos, status)
        integer, intent(in) :: pos
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: old, item
        integer :: k

        status = status_type()
        k = local_entry()

        ! If leaving the edit line, save partial unfinished input.
        if (comint_buffers(k)%input_ring_index < 0) then
            call call_get_old_input(old, status)
            if (.not. status%ok()) return
            k = local_entry()
            comint_buffers(k)%stored_incomplete_input = old
        end if

        comint_buffers(k)%input_ring_index = pos
        call comint_delete_input(status)
        if (.not. status%ok()) return
        k = local_entry()

        if (pos >= 0 .and. ring_length() > 0) then
            call comint_input_ring_ref(pos, item)
            call insert(item, status)
        else if (allocated(comint_buffers(k)%stored_incomplete_input)) then
            ! Restore partial unfinished input.
            if (len(comint_buffers(k)%stored_incomplete_input) > 0) then
                call insert(comint_buffers(k)%stored_incomplete_input, status)
            end if
        end if
    end subroutine comint_goto_input

    !> Return the proper search function, for Isearch in input history.
    !> Port of comint.el:comint-history-isearch-search.
    recursive subroutine comint_history_isearch_search(fn)
        procedure(isearch_search_proc), pointer, intent(out) :: fn

        fn => history_search
    end subroutine comint_history_isearch_search

    ! The search function comint-history-isearch-search returns.
    ! Port of comint.el:comint-history-isearch-search (the function it returns).
    recursive subroutine history_search(string, bound, noerror, pos, status)
        character(len=*), intent(in) :: string
        integer(int64), intent(in) :: bound
        logical, intent(in) :: noerror
        integer(int64), intent(out) :: pos
        type(status_type), intent(out) :: status
        procedure(isearch_search_proc), pointer :: search_fun
        type(handle_type) :: proc
        type(status_type) :: st
        logical :: found

        ! Use standard functions to search within comint text
        search_fun => isearch_search_fun_default()
        status = status_type()

        ! Avoid lazy-highlighting matches in the comint prompt and in the
        ! output when searching forward.  Lazy-highlight calls this lambda
        ! with the bound arg, so skip the prompt and the output.
        if (bound > 0 .and. isearch_forward .and. .not. comint_after_pmark_p()) then
            call current_process(proc, st)
            if (st%ok()) call goto_char(pmark_pos(proc))
        end if

        ! 1. First try searching in the initial comint text
        if (isearch_forward) then
            call search_fun(string, bound, noerror, pos, status)
        else
            call search_fun(string, comint_line_beginning_position(), noerror, pos, status)
        end if

        if (.not. status%ok() .or. pos > 0) return
        ! 2. If the above search fails, start putting next/prev history
        ! elements in the comint successively, and search the string in
        ! them.  Do this only when bound is nil (i.e. not while
        ! lazy-highlighting search strings in the current comint text).
        if (bound > 0) return
        found = .false.

        do while (.not. found)
            if (isearch_forward) then
                ! Signal an error here explicitly, because
                ! `comint-next-input' doesn't signal an error.
                if (input_ring_index() < 0) return
                call comint_next_input(1, st)
                if (.not. st%ok()) return
                call goto_char(comint_line_beginning_position())
            else
                ! Signal an error here explicitly, because
                ! `comint-previous-input' doesn't signal an error.
                if (input_ring_index() == ring_length() - 1) return
                call comint_previous_input(1, st)
                if (.not. st%ok()) return
                call goto_char(point_max())
            end if

            call set_isearch_barrier(point())
            call set_isearch_opoint(point())

            ! After putting the next/prev history element, search the
            ! string in them again, until comint-next-input or
            ! comint-previous-input raises an error at the beginning/end
            ! of history.
            if (isearch_forward) then
                call search_fun(string, 0_int64, noerror, pos, st)
            else
                ! For backward search, don't search in the comint prompt
                call search_fun(string, comint_line_beginning_position(), noerror, pos, st)
            end if

            if (.not. st%ok()) return
            found = pos > 0
        end do

        ! Return point of the new search result
        pos = point()
    end subroutine history_search

    !> Display the input history search prompt.
    !> If there are no search errors, this function displays an overlay
    !> with the Isearch prompt which replaces the original comint prompt.
    !> Otherwise, it displays the standard Isearch message returned from
    !> the function `isearch_message'.  Port of
    !> comint.el:comint-history-isearch-message.
    recursive subroutine comint_history_isearch_message(c_q_hack, ellipsis)
        logical, intent(in) :: c_q_hack, ellipsis
        type(status_type) :: st
        integer :: k

        if (.not. (isearch_success .and. .not. allocated(isearch_error))) then
            ! Use standard function `isearch-message' when not in comint
            ! prompt, or search fails, or has an error (like incomplete
            ! regexp).  This function displays isearch message in the echo
            ! area, so it's possible to see what is wrong in the search
            ! string.
            call isearch_message_display(c_q_hack, ellipsis)
            return
        end if

        ! Otherwise, put the overlay with the standard isearch prompt over
        ! the initial comint prompt.
        k = local_entry()

        if (overlayp(comint_buffers(k)%history_isearch_message_overlay)) then
            call move_overlay(comint_buffers(k)%history_isearch_message_overlay, prompt_line_start(), &
                              comint_line_beginning_position(), st)
        else
            call make_overlay(comint_buffers(k)%history_isearch_message_overlay, prompt_line_start(), &
                              comint_line_beginning_position(), st)
            call overlay_put(comint_buffers(k)%history_isearch_message_overlay, "evaporate", t_value(), st)
        end if

        call overlay_put(comint_buffers(k)%history_isearch_message_overlay, "display", &
                         make_str(isearch_message_prefix(isearch_nonincremental, ellipsis)), st)

        if (comint_buffers(k)%input_ring_index >= 0 .and. .not. ellipsis) then
            ! Display the current history index.
            call message("History item: "//int_str(comint_buffers(k)%input_ring_index + 1))
        else
            ! Or clear a previous isearch message.
            call message("")
        end if
    end subroutine comint_history_isearch_message

    !> Wrap the input history search when search fails.
    !> Move point to the first history element for a forward search, or
    !> to the last history element for a backward search.  Port of
    !> comint.el:comint-history-isearch-wrap.
    recursive subroutine comint_history_isearch_wrap()
        type(status_type) :: st

        ! When `comint-history-isearch-search' fails on reaching the
        ! beginning/end of the history, wrap the search to the first/last
        ! input history element.
        if (isearch_forward) then
            call comint_goto_input(ring_length() - 1, st)
            call goto_char(comint_line_beginning_position())
        else
            call comint_goto_input(-1, st)
            call goto_char(point_max())
        end if
    end subroutine comint_history_isearch_wrap

    !> Save a function restoring the state of input history search.
    !> Save `comint_input_ring_index' to the additional state parameter in
    !> the search status stack.  Port of
    !> comint.el:comint-history-isearch-push-state.
    recursive subroutine comint_history_isearch_push_state(data, pop)
        character(len=:), allocatable, intent(out) :: data
        procedure(isearch_pop_proc), pointer, intent(out) :: pop

        data = int_str(input_ring_index())
        pop => comint_history_isearch_pop_state
    end subroutine comint_history_isearch_push_state

    !> Restore the input history search state.
    !> Go to the history element by the absolute history position
    !> HIST_POS.  Port of comint.el:comint-history-isearch-pop-state.
    recursive subroutine comint_history_isearch_pop_state(hist_pos)
        character(len=*), intent(in) :: hist_pos
        type(status_type) :: st
        integer :: n, ios

        read (hist_pos, *, iostat=ios) n
        if (ios /= 0) n = -1
        call comint_goto_input(n, st)
    end subroutine comint_history_isearch_pop_state

    ! ------------------------------------------------------------------
    ! Scrolling on input, stopped subjobs
    ! ------------------------------------------------------------------

    !> Go to the end of buffer in all windows showing it.
    !> Movement occurs if point in the selected window is not after the
    !> process mark, and `this_command' is an insertion command.
    !> Insertion commands recognized are `self_insert_command',
    !> `comint_magic_space', `yank', and `hilit_yank'.  Depends on the
    !> value of `comint_scroll_to_bottom_on_input'.
    !>
    !> This function should be a pre-command hook.  Port of
    !> comint.el:comint-preinput-scroll-to-bottom.
    recursive subroutine comint_preinput_scroll_to_bottom(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: process, current
        type(handle_type), allocatable :: leaves(:)
        integer :: scroll, i

        status = status_type()
        scroll = int(comint_scroll_to_bottom_on_input%value)
        if (scroll == COMINT_SCROLL_ON_INPUT_NIL) return
        if (.not. (symbol_is(this_command, "self_insert_command") .or. symbol_is(this_command, "comint_magic_space") &
                   .or. symbol_is(this_command, "yank") .or. symbol_is(this_command, "hilit_yank"))) return
        current = current_buffer()
        process = get_buffer_process(current)
        if (process%index == 0) return
        if (point() >= pmark_pos(process)) return

        if (scroll == COMINT_SCROLL_ON_INPUT_THIS) then
            call goto_char(point_max())
        else
            call window_leaves(selected_frame%root_window, leaves)

            do i = 1, size(leaves)
                if (same_handle(windows(leaves(i)%index)%buffer, current)) then
                    call set_window_point(windows(leaves(i)%index), point_max(), status)
                end if
            end do

            call goto_char(point_max())
        end if
    end subroutine comint_preinput_scroll_to_bottom

    !> Stop the current subjob.
    !>
    !> WARNING: if there is no current subjob, you can end up suspending
    !> the top-level process running in the buffer.  If you accidentally
    !> do this, use \[comint_continue_subjob] to resume the process.
    !> (This is not a problem with most shells, since they ignore this
    !> signal.)  Port of comint.el:comint-stop-subjob.
    recursive subroutine comint_stop_subjob(status)
        type(status_type), intent(out) :: status

        call comint_skip_input(status)
        if (.not. status%ok()) return
        call stop_process(status, current_group=comint_ptyp())
    end subroutine comint_stop_subjob

    !> Send CONT signal to process buffer's process group.
    !> Useful if you accidentally suspend the top-level process.  Port of
    !> comint.el:comint-continue-subjob.
    subroutine comint_continue_subjob(status)
        type(status_type), intent(out) :: status

        call continue_process(status, current_group=comint_ptyp())
    end subroutine comint_continue_subjob

    ! ------------------------------------------------------------------
    ! Passwords
    ! ------------------------------------------------------------------

    !> The default value of `comint_password_prompt_regexp', made from
    !> `password_word_equivalents' and `password_colon_equivalents'.
    !> Port of the value of comint.el comint-password-prompt-regexp.
    function comint_password_prompt_regexp_default() result(re)
        character(len=:), allocatable :: re
        character(len=MAX_MULTIBYTE_LENGTH) :: buf
        character(len=:), allocatable :: colons
        type(status_type) :: st
        integer(int32) :: nbytes
        integer :: i

        colons = ""

        do i = 1, size(password_colon_equivalents)
            call char_string(password_colon_equivalents(i), buf, nbytes, st)
            colons = colons//buf(1:nbytes)
        end do

        re = "\(^ *\|"// &
             regexp_opt([character(len=24) :: "Enter", "enter", "Enter same", "enter same", "Enter the", "enter the", &
                         "Current", &
                         ! Ansible.  (Bug#78442)
                         "Vault", "SSH", "BECOME", &
                         "Enter Auth", "enter auth", "Old", "old", "New", "new", "login", &
                         "Kerberos", "CVS", "UNIX", " SMB", "LDAP", "PEM", "SUDO", &
                         "[sudo]", "[sudo: authenticate]", "[su: authenticate]", &
                         "doas", "Repeat", "Bad", "Retype", "Verify"], REGEXP_OPT_T)// &
             ! Allow for user name to precede password equivalent
             ! (Bug#31075).
             " +.*\)"// &
             "\(?:"//regexp_opt(password_word_equivalents)//"\|Response\)"// &
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
    end function comint_password_prompt_regexp_default

    !> Read a password with PROMPT (obsolete: use `read_passwd').  Port of
    !> comint.el:comint-read-noecho.
    recursive subroutine comint_read_noecho(prompt, result, status)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status

        call read_passwd(prompt, result, status)
    end subroutine comint_read_noecho

    !> Read a string without echoing.
    !> Then send it to the process running in the current buffer.
    !> The string is sent using `comint_input_sender'.
    !> Security bug: your string can still be temporarily recovered with
    !> \[view_lossage]; `clear_this_command_keys' can fix that.  Port of
    !> comint.el:comint-send-invisible.
    recursive subroutine comint_send_invisible(status, prompt)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: prompt
        type(handle_type) :: proc, wbuf
        character(len=:), allocatable :: prefix, prefix_prompt, str
        integer :: k
        logical :: found

        status = status_type()
        wbuf = windows(selected_frame%selected_window%index)%buffer

        if (same_handle(wbuf, current_buffer())) then
            prefix = ""
        else
            prefix = "(In buffer "//buffers(cur)%name//") "
        end if

        proc = get_buffer_process(current_buffer())

        if (proc%index == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Buffer "//buffers(cur)%name//" has no process")
            return
        end if

        if (present(prompt)) then
            prefix_prompt = prefix//prompt
        else
            prefix_prompt = prefix//"Non-echoed text: "
        end if

        found = .false.
        k = local_entry()

        if (associated(comint_buffers(k)%password_function)) then
            call comint_buffers(k)%password_function(prefix_prompt, str, found)
        end if

        if (.not. found) then
            call read_passwd(prefix_prompt, str, status)
            found = status%ok()
        end if

        if (found) then
            call comint_snapshot_last_prompt()
            k = local_entry()
            call comint_buffers(k)%input_sender(proc, str, status)
        else
            call message("Warning: text will be echoed")
            status = status_type()
        end if
    end subroutine comint_send_invisible

    recursive subroutine cmd_comint_send_invisible(status)
        type(status_type), intent(out) :: status

        call comint_send_invisible(status)
    end subroutine cmd_comint_send_invisible

    !> Prompt in the minibuffer for password and send without echoing.
    !> Looks for a match to `comint_password_prompt_regexp' in order to
    !> detect the need to (prompt and) send a password.  Ignores any
    !> carriage returns (\r) in STRING.
    !>
    !> This function could be in the list
    !> `comint_output_filter_functions'.  Port of
    !> comint.el:comint-watch-for-password-prompt.
    recursive subroutine comint_watch_for_password_prompt(string, status)
        character(len=*), intent(in) :: string
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: text, content, prompt
        type(pending_prompt_type), allocatable :: grown(:)
        type(handle_type) :: timer
        logical :: saved_fold, matched
        integer :: n

        status = status_type()
        text = string_limit_end(without_cr(string), comint_password_prompt_max_length)
        saved_fold = case_fold_search%value
        case_fold_search = .true.
        matched = string_match(comint_password_prompt_regexp, text, status) > 0
        case_fold_search = saved_fold

        if (.not. status%ok() .or. .not. matched) then
            status = status_type()
            return
        end if

        content = string_trim(match_string(0, text))
        prompt = content

        if (len(content) > 0) then
            if (content(len(content):len(content)) /= " ") prompt = content//" "
        end if

        ! Use `run-at-time' in order not to pause execution of the process
        ! filter with a minibuffer
        if (.not. allocated(pending_prompts)) allocate (pending_prompts(0))
        n = size(pending_prompts)
        allocate (grown(n + 1))
        grown(1:n) = pending_prompts
        grown(n + 1)%buffer = current_buffer()
        grown(n + 1)%prompt = prompt
        call move_alloc(grown, pending_prompts)
        call run_at_time(timer, 0.0_real64, send_pending_password, status)

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

        ! (string-trim S "[ \n\r\t\v\f\b\a]+" "\n+")
        function string_trim(s) result(r)
            character(len=*), intent(in) :: s
            character(len=:), allocatable :: r
            integer :: b, e

            b = 1

            do while (b <= len(s))
                if (index(" "//achar(10)//achar(13)//achar(9)//achar(11)//achar(12)//achar(8)//achar(7), s(b:b)) == 0) exit
                b = b + 1
            end do

            e = len(s)

            do while (e >= b)
                if (s(e:e) /= achar(10)) exit
                e = e - 1
            end do

            r = s(b:e)
        end function string_trim
    end subroutine comint_watch_for_password_prompt

    ! The timer function of `comint_watch_for_password_prompt': send the
    ! oldest pending password prompt's password.  Not a port.
    recursive subroutine send_pending_password(status)
        type(status_type), intent(out) :: status
        type(pending_prompt_type) :: p
        type(handle_type) :: old, proc
        type(status_type) :: st

        status = status_type()
        if (.not. allocated(pending_prompts)) return
        if (size(pending_prompts) == 0) return
        p = pending_prompts(1)
        pending_prompts = pending_prompts(2:)
        if (.not. buffer_live_p(p%buffer)) return
        old = current_buffer()
        call set_buffer(p%buffer, st)
        comint_prompt_recursion_depth = comint_prompt_recursion_depth + 1

        if (comint_prompt_recursion_depth > 10) then
            call message("Password prompt recursion too deep")
        else
            proc = get_buffer_process(current_buffer())
            if (proc%index /= 0) call comint_send_invisible(status, p%prompt)
        end if

        comint_prompt_recursion_depth = comint_prompt_recursion_depth - 1
        if (buffer_live_p(old)) call set_buffer(old, st)
    end subroutine send_pending_password

    ! ------------------------------------------------------------------
    ! Completion
    ! ------------------------------------------------------------------

    !> Complete a list of recent inputs entered into the current buffer.
    !> Like `minibuffer_complete_history' but completes on comint inputs.
    !> This function makes `comint_dynamic_list_input_ring' obsolete.
    !> Port of comint.el:comint-complete-input-ring.
    recursive subroutine comint_complete_input_ring(status)
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: elems(:)
        type(completion_table_type) :: table
        procedure(cir_predicate_proc), pointer :: saved_pred
        character(len=:), allocatable :: item
        type(status_type) :: st
        integer :: k, i, n
        logical :: res

        status = status_type()
        k = entry_of(cur)
        n = 0

        if (k /= 0) then
            if (comint_buffers(k)%has_ring) n = comint_buffers(k)%input_ring%count()
        end if

        if (n == 0) then
            call set_error(status, FM_USER_ERROR, "No history available")
            return
        end if

        allocate (elems(n))

        do i = 1, n
            call comint_buffers(k)%input_ring%ref(i - 1, item, st)
            elems(i) = make_string(item)
        end do

        table = completion_list_table(make_list(elems), "comint_input")
        table%unsorted = .true.
        saved_pred => completion_in_region_mode_predicate
        completion_in_region_mode_predicate => completions_shown
        res = completion_in_region(comint_line_beginning_position(), point_max(), table)
        completion_in_region_mode_predicate => saved_pred
    end subroutine comint_complete_input_ring

    ! Whether *Completions* is shown (minibuffer--completions-visible as
    ! the `completion_in_region_mode_predicate' of
    ! `comint_complete_input_ring').  Port of
    ! minibuffer.el:minibuffer--completions-visible.
    recursive logical function completions_shown()
        type(handle_type) :: window

        window = completions_visible()
        completions_shown = window%index /= 0
    end function completions_shown

    !> Return expanded DIRECTORY, with `comint_file_name_prefix' if
    !> absolute: EXPANDED.  Port of comint.el:comint-directory.
    recursive subroutine comint_directory(directory, expanded, status)
        character(len=*), intent(in) :: directory
        character(len=:), allocatable, intent(out) :: expanded
        type(status_type), intent(out) :: status

        if (file_name_absolute_p(directory)) then
            call expand_file_name(comint_file_name_prefix%value//directory, expanded, status)
        else
            call expand_file_name(directory, expanded, status)
        end if
    end subroutine comint_directory

    !> Return the word of WORD_CHARS at point as WORD, FOUND false if none
    !> is found.  Word constituents are considered to be those in
    !> WORD_CHARS, which is like the inside of a "[...]" (see
    !> `skip_chars_forward'), plus all non-ASCII characters.  The match
    !> data is set to match the word.  Port of comint.el:comint-word.
    recursive subroutine comint_word(word_chars, word, found)
        character(len=*), intent(in) :: word_chars
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
            moved = skip_chars_backward("\\"//word_chars, st)
            call char_after(c, have, point() - 2)

            if (have .and. c == iachar("\")) then
                call char_after(c, have, point() - 1)

                if (have) then
                    if (any(quote_chars() == c)) call goto_char(point() - 2)
                end if
            end if

            ! FIXME: This isn't consistent with Bash, at least -- not all
            ! non-ASCII chars should be word constituents.
            if (.not. bobp()) then
                if (preceding_char() >= 128) call goto_char(point() - 1)
            end if

            giveup = point() == startpoint
        end do

        ! Set match-data to match the entire string.
        if (point() < here) then
            call set_match_data([point(), here])
            call buffer_substring(point(), here, word, st)
            found = .true.
        end if

        call goto_char(here)
    end subroutine comint_word

    ! The characters of `comint_file_name_quote_list'.  Not a port.
    function quote_chars() result(chars)
        integer(int32), allocatable :: chars(:)
        integer(int32) :: c, n
        integer(int64) :: i

        allocate (chars(0))
        if (.not. allocated(comint_file_name_quote_list%value)) return
        i = 1

        do while (i <= len(comint_file_name_quote_list%value))
            call string_char_and_length(comint_file_name_quote_list%value, i, c, n)
            chars = [chars, c]
            i = i + n
        end do
    end function quote_chars

    ! Return the filename at point as-is as FILENAME, FOUND false if none
    ! is found.  See `comint_word'.  Port of
    ! comint.el:comint--match-partial-filename.
    recursive subroutine match_partial_filename(filename, found)
        character(len=:), allocatable, intent(out) :: filename
        logical, intent(out) :: found

        call comint_word(comint_file_name_chars%value, filename, found)
    end subroutine match_partial_filename

    ! QSTR unquoted as USTR, and QUPOS the position in QSTR that
    ! corresponds to UPOS in USTR (-1 for none).  Port of
    ! comint.el:comint--unquote&requote-argument.
    recursive subroutine unquote_requote_argument(qstr, upos_in, ustr, qupos)
        character(len=*), intent(in) :: qstr
        integer(int64), intent(in) :: upos_in
        character(len=:), allocatable, intent(out) :: ustr
        integer(int64), intent(out) :: qupos
        character(len=:), allocatable :: re, value
        integer(int64) :: upos, qpos, match, mend
        type(status_type) :: st
        logical :: found

        upos = upos_in
        qpos = 0
        ustr = ""
        qupos = -1
        re = "\$\(?:\([[:alpha:]][[:alnum:]]*\)\|{\(?1:[^{}]+\)}\)"
        if (size(quote_chars()) > 0) re = re//"\|\\\(.\)"

        do
            match = string_match(re, qstr, st, start=qpos + 1)
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

        call push(substring(qstr, qpos), schars(qstr))

    contains

        subroutine push(str, end)
            character(len=*), intent(in) :: str
            integer(int64), intent(in) :: end

            ustr = ustr//str
            upos = upos - schars(str)

            if (qupos < 0 .and. upos <= 0) then
                if (end < 0) then
                    qupos = -end
                else
                    qupos = upos + end
                end if
            end if
        end subroutine push
    end subroutine unquote_requote_argument

    !> STR with environment variables substituted and quoting `\'
    !> removed, as USTR (the default `comint_unquote_function').  Port of
    !> comint.el:comint--unquote-argument.
    recursive subroutine comint_unquote_argument(str, ustr)
        character(len=*), intent(in) :: str
        character(len=:), allocatable, intent(out) :: ustr
        integer(int64) :: qupos

        call unquote_requote_argument(str, 0_int64, ustr, qupos)
    end subroutine comint_unquote_argument

    !> The default `comint_requote_function'.  See
    !> `completion_table_with_quoting'.  Port of
    !> comint.el:comint--requote-argument.
    recursive subroutine comint_requote_argument(upos, qstr, qpos, qfun)
        integer(int64), intent(in) :: upos
        character(len=*), intent(in) :: qstr
        integer(int64), intent(out) :: qpos
        procedure(completion_quote_proc), pointer, intent(out) :: qfun
        character(len=:), allocatable :: ustr

        call unquote_requote_argument(qstr, upos, ustr, qpos)
        qfun => comint_quote_filename
    end subroutine comint_requote_argument

    !> Return the unquoted&expanded filename at point as FILENAME, FOUND
    !> false if none is found.  Environment variables are substituted.
    !> See `comint_word'.  Port of comint.el:comint-match-partial-filename.
    recursive subroutine comint_match_partial_filename(filename, found)
        character(len=:), allocatable, intent(out) :: filename
        logical, intent(out) :: found
        character(len=:), allocatable :: unquoted

        call match_partial_filename(filename, found)

        if (found) then
            unquoted = filename
            call comint_unquote_argument(unquoted, filename)
        end if
    end subroutine comint_match_partial_filename

    !> Return FILENAME with magic characters quoted, as QUOTED.
    !> Magic characters are those in `comint_file_name_quote_list'.
    !> Port of comint.el:comint-quote-filename.
    recursive subroutine comint_quote_filename(filename, quoted)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable, intent(out) :: quoted
        character(len=:), allocatable :: regexp, replaced
        integer(int64) :: i, mend
        integer(int64), allocatable :: saved(:)
        type(status_type) :: st

        quoted = filename
        if (size(quote_chars()) == 0) return
        regexp = regexp_opt_charset(quote_chars())
        saved = match_data()
        i = 1

        do while (string_match(regexp, quoted, st, start=i) > 0)
            mend = match_end(0, st)
            call replace_match("\\\&", st, string=quoted, result=replaced)
            quoted = replaced
            i = mend + 1
        end do

        call set_match_data(saved)
    end subroutine comint_quote_filename

    !> The file name completion table of comint: `completion_file_name_table'
    !> with `comint_file_name_prefix' before absolute file names.  Port of
    !> comint.el:comint-completion-file-name-table.
    recursive subroutine comint_completion_file_name_table(string, action, result, status, pred, suffix)
        character(len=*), intent(in) :: string
        integer, intent(in) :: action
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: pred
        character(len=*), intent(in), optional :: suffix
        type(status_type) :: st

        if (.not. file_name_absolute_p(string)) then
            call completion_file_name_table(string, action, result, status, pred, suffix)
            return
        end if

        select case (action)
        case (COMPLETE_ALL, COMPLETE_TEST)
            call completion_file_name_table(comint_file_name_prefix%value//string, action, result, status, pred, suffix)
        case (COMPLETE_TRY)
            call completion_file_name_table(comint_file_name_prefix%value//string, action, result, status, pred, suffix)

            if (stringp(result)) then
                if (string_match("\`"//regexp_quote(comint_file_name_prefix%value), xstring(result), st) > 0) then
                    result = make_string(substring(xstring(result), match_end(0, st) - 1))
                end if
            end if
        case default
            call completion_file_name_table(string, action, result, status, pred, suffix)
        end select
    end subroutine comint_completion_file_name_table

    ! The table of `comint_file_name_completion_data': file names quoted
    ! with `comint_unquote_function' and `comint_requote_function',
    ! completed with `read_file_name_completion_ignore_case' and
    ! `comint_completion_fignore' (the lambda of
    ! comint--complete-file-name-data).  Port of
    ! comint.el:comint--complete-file-name-data, in part.
    recursive subroutine file_name_table(string, action, result, status, pred, suffix)
        character(len=*), intent(in) :: string
        integer, intent(in) :: action
        type(value_type), intent(out) :: result
        type(status_type), intent(out) :: status
        procedure(completion_predicate), optional :: pred
        character(len=*), intent(in), optional :: suffix
        type(completion_table_type) :: table
        character(len=len(completion_ignored_extensions)) :: saved_extensions(size(completion_ignored_extensions))
        procedure(completion_unquote_proc), pointer :: unquote
        procedure(completion_requote_proc), pointer :: requote
        character(len=8), allocatable :: fignore(:)
        logical :: saved_case
        integer :: k, n

        call quote_functions(unquote, requote)
        table = completion_table_with_quoting(comint_completion_file_name_table, unquote, requote)
        saved_case = completion_ignore_case%value
        saved_extensions = completion_ignored_extensions
        completion_ignore_case%value = read_file_name_completion_ignore_case
        completion_ignored_extensions = ""
        k = entry_of(cur)

        if (k /= 0) then
            if (allocated(comint_buffers(k)%completion_fignore)) fignore = comint_buffers(k)%completion_fignore
        end if

        if (.not. allocated(fignore) .and. allocated(comint_completion_fignore)) fignore = comint_completion_fignore

        if (allocated(fignore)) then
            n = min(size(fignore), size(completion_ignored_extensions))
            completion_ignored_extensions(1:n) = fignore(1:n)
        end if

        call quoting_table_call(table, string, action, result, status, pred, suffix)
        completion_ignore_case%value = saved_case
        completion_ignored_extensions = saved_extensions
    end subroutine file_name_table

    ! The current buffer's `comint_unquote_function' and
    ! `comint_requote_function'.  Not a port.
    subroutine quote_functions(unquote, requote)
        procedure(completion_unquote_proc), pointer, intent(out) :: unquote
        procedure(completion_requote_proc), pointer, intent(out) :: requote
        integer :: k

        unquote => comint_unquote_function
        requote => comint_requote_function
        k = entry_of(cur)
        if (k == 0) return
        if (associated(comint_buffers(k)%unquote)) unquote => comint_buffers(k)%unquote
        if (associated(comint_buffers(k)%requote)) requote => comint_buffers(k)%requote
    end subroutine quote_functions

    !> Give the current buffer its own `comint_unquote_function' and
    !> `comint_requote_function' (set_local), UNQUOTE and REQUOTE.
    !> Not a port.
    subroutine set_comint_quote_functions(unquote, requote)
        procedure(completion_unquote_proc) :: unquote
        procedure(completion_requote_proc) :: requote
        integer :: k

        k = local_entry()
        comint_buffers(k)%unquote => unquote
        comint_buffers(k)%requote => requote
    end subroutine set_comint_quote_functions

    !> Give the current buffer its own `comint_completion_fignore',
    !> SUFFIXES (set_local).  Not a port.
    subroutine set_comint_completion_fignore(suffixes)
        character(len=*), intent(in) :: suffixes(:)
        integer :: k, i

        k = local_entry()
        if (allocated(comint_buffers(k)%completion_fignore)) deallocate (comint_buffers(k)%completion_fignore)
        allocate (comint_buffers(k)%completion_fignore(size(suffixes)))

        do i = 1, size(suffixes)
            comint_buffers(k)%completion_fignore(i) = suffixes(i)
        end do
    end subroutine set_comint_completion_fignore

    ! The FILESUFFIX of `comint_completion_addsuffix'.  Not a port.
    function file_suffix() result(suffix)
        character(len=:), allocatable :: suffix

        suffix = ""
        if (allocated(comint_completion_addsuffix%value)) suffix = comint_completion_addsuffix%value
    end function file_suffix

    ! The `:exit_function' of `comint_file_name_completion_data': add
    ! the FILESUFFIX of `comint_completion_addsuffix' (or move over it)
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

        if (looking_at(regexp_quote(suffix), st)) then
            call goto_char(match_end(0, st))
        else
            call insert(suffix, st)
        end if
    end subroutine file_suffix_exit

    !> Return the completion data for file name at point.  Port of
    !> comint.el:comint--complete-file-name-data.
    recursive subroutine comint_file_name_completion_data(data)
        type(capf_data_type), intent(out) :: data
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
    end subroutine comint_file_name_completion_data

    !> Return completion data for filename at point, if any.  Port of
    !> comint.el:comint-filename-completion.
    recursive subroutine comint_filename_completion(data, status)
        type(capf_data_type), intent(out) :: data
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: filename
        logical :: found

        status = status_type()
        data = capf_data_type()
        call match_partial_filename(filename, found)
        if (found) call comint_file_name_completion_data(data)
    end subroutine comint_filename_completion

    !> Expand input command history at point.
    !> For use on `completion_at_point_functions': DATA gives the function
    !> performing the expansion when there are history references.
    !> Port of comint.el:comint-c-a-p-replace-by-expanded-history (SILENT
    !> and START false).
    recursive subroutine comint_c_a_p_replace_by_expanded_history(data, status)
        type(capf_data_type), intent(out) :: data
        type(status_type), intent(out) :: status

        status = status_type()
        data = capf_data_type()
        if (.not. history_references_p()) return
        data%found = .true.
        data%perform => perform_expand_history
    end subroutine comint_c_a_p_replace_by_expanded_history

    ! The function comint-c-a-p-replace-by-expanded-history returns.
    ! Port of comint.el:comint-c-a-p-replace-by-expanded-history, in part.
    recursive logical function perform_expand_history() result(changed)
        integer(int64) :: tick
        type(status_type) :: st

        ! Looks like there might be history references in the command.
        tick = buffers(cur)%text%modiff
        call comint_replace_by_expanded_history_before_point(status=st)
        changed = tick /= buffers(cur)%text%modiff
    end function perform_expand_history

    !> Run `comint_dynamic_complete_functions' until one returns
    !> completion data (for `completion_at_point_functions').  Port of
    !> comint.el:comint-completion-at-point.
    recursive subroutine comint_completion_at_point(data, status)
        type(capf_data_type), intent(out) :: data
        type(status_type), intent(out) :: status
        type(capf_entry_type), allocatable :: fns(:)
        integer :: k

        status = status_type()
        data = capf_data_type()
        fns = comint_dynamic_complete_functions%functions()

        do k = 1, size(fns)
            call fns(k)%fn(data, status)
            if (.not. status%ok()) return
            if (data%found) return
        end do
    end subroutine comint_completion_at_point

    !> Dynamically complete the filename at point.
    !> Completes if after a filename.
    !> This function is similar to `comint_replace_by_expanded_filename',
    !> except that it won't change parts of the filename already entered
    !> in the buffer; it just adds completion characters to the end of the
    !> filename.  A completions listing may be shown in a separate buffer
    !> if completion is ambiguous.
    !>
    !> Completion is dependent on the value of
    !> `comint_completion_addsuffix', `comint_completion_recexact' and
    !> `comint_completion_fignore', and the timing of completions listing
    !> is dependent on the value of `comint_completion_autolist'.
    !>
    !> Returns true if successful.  Port of
    !> comint.el:comint-dynamic-complete-filename.
    recursive logical function comint_dynamic_complete_filename(status) result(done)
        type(status_type), intent(out) :: status
        type(capf_data_type) :: data
        character(len=:), allocatable :: filename
        logical :: found

        status = status_type()
        done = .false.
        call match_partial_filename(filename, found)
        if (.not. found) return
        if (.not. selected_frame%minibuffer_selected) call message("Completing file name...")
        call comint_file_name_completion_data(data)
        done = completion_in_region(data%start, data%end, data%table)
    end function comint_dynamic_complete_filename

    subroutine cmd_comint_dynamic_complete_filename(status)
        type(status_type), intent(out) :: status
        logical :: done

        done = comint_dynamic_complete_filename(status)
    end subroutine cmd_comint_dynamic_complete_filename

    !> Dynamically expand and complete the filename at point.
    !> Replace the filename with an expanded, canonicalized and completed
    !> replacement, i.e. substituting environment variables (e.g. $HOME),
    !> `~'s, `..', and `.', and making the filename absolute.  For
    !> expansion see `expand_file_name' and `substitute_in_file_name'.
    !> For completion see `comint_dynamic_complete_filename'.  Port of
    !> comint.el:comint-replace-by-expanded-filename.
    recursive subroutine comint_replace_by_expanded_filename(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: filename, expanded
        logical :: found, done

        status = status_type()
        call comint_match_partial_filename(filename, found)
        if (.not. found) return
        call expand_file_name(filename, expanded, status)
        if (.not. status%ok()) return
        call replace_match(expanded, status, fixedcase=.true., literal=.true.)
        if (.not. status%ok()) return
        done = comint_dynamic_complete_filename(status)
    end subroutine comint_replace_by_expanded_filename

    !> Display a list of possible completions for the filename at point.
    !> Port of comint.el:comint-dynamic-list-filename-completions.
    recursive subroutine comint_dynamic_list_filename_completions(status)
        type(status_type), intent(out) :: status
        type(capf_data_type) :: data
        type(completion_table_type) :: saved_table
        procedure(completion_predicate), pointer :: saved_pred
        type(handle_type) :: ol

        status = status_type()
        call comint_file_name_completion_data(data)
        saved_table = minibuffer_completion_table
        saved_pred => minibuffer_completion_predicate
        minibuffer_completion_table = data%table
        minibuffer_completion_predicate => null()
        call make_overlay(ol, data%start, data%end, status, rear_advance=.true.)

        if (status%ok()) then
            call overlay_put(ol, "field", sym("completion"), status)
            call minibuffer_completion_help()
            call free_overlay(ol)
        end if

        minibuffer_completion_table = saved_table
        minibuffer_completion_predicate => saved_pred
    end subroutine comint_dynamic_list_filename_completions

    ! ------------------------------------------------------------------
    ! Setup
    ! ------------------------------------------------------------------

    !> Define Comint mode, its map, faces and settings, and register the
    !> commands.  Not a port.
    subroutine syms_of_comint()
        type(status_type) :: st

        if (associated(major_modes%comint_mode%run)) return
        call major_modes%comint_mode%define("comint_mode", "Comint", comint_mode, comint_mode_hook)
        major_modes%comint_mode%special = .true.
        comint_mode_map = major_modes%comint_mode%map
        call define_faces()
        if (.not. allocated(comint_prompt_regexp%value)) comint_prompt_regexp = "^"
        if (.not. allocated(comint_delimiter_argument_list%value)) comint_delimiter_argument_list = ""
        if (.not. allocated(comint_input_ring_separator%value)) comint_input_ring_separator = achar(10)
        if (.not. allocated(comint_input_history_ignore%value)) comint_input_history_ignore = "^#"
        if (.not. allocated(comint_terminfo_terminal%value)) comint_terminfo_terminal = "dumb"
        call forward_setting(comint_prompt_regexp)
        call forward_setting(comint_delimiter_argument_list)
        call forward_setting(comint_prompt_read_only)
        call forward_setting(comint_highlight_input)
        call forward_setting(comint_input_ignoredups)
        call forward_setting(comint_input_ring_file_name)
        call forward_setting(comint_pager)
        call forward_setting(comint_input_ring_file_prefix)
        call forward_setting(comint_move_point_for_output)
        call forward_setting(comint_move_point_for_matching_input)
        call forward_setting(comint_scroll_show_maximum_output)
        call forward_setting(comint_buffer_maximum_size)
        call forward_setting(comint_input_ring_size)
        call forward_setting(comint_input_ring_separator)
        call forward_setting(comint_input_history_ignore)
        call forward_setting(comint_process_echoes)
        call forward_setting(comint_eol_on_send)
        call forward_setting(comint_use_prompt_regexp)
        call forward_setting(comint_terminfo_terminal)
        call forward_setting(comint_inhibit_carriage_motion)
        if (.not. allocated(comint_completion_addsuffix%value)) comint_completion_addsuffix = " "
        if (.not. allocated(comint_file_name_prefix%value)) comint_file_name_prefix = ""
        if (.not. allocated(comint_file_name_chars%value)) comint_file_name_chars = "[]~/A-Za-z0-9+@:_.$#%,={}-"
        if (.not. allocated(comint_file_name_quote_list%value)) comint_file_name_quote_list = ""
        call forward_setting(comint_file_name_prefix)
        call forward_setting(comint_file_name_chars)
        call forward_setting(comint_file_name_quote_list)
        call comint_dynamic_complete_functions%add(comint_filename_completion)
        call comint_dynamic_complete_functions%add(comint_c_a_p_replace_by_expanded_history)
        ! (The default value lists comint-postoutput-scroll-to-bottom
        ! before comint-watch-for-password-prompt; `add' puts a function
        ! first.)
        call comint_output_filter_functions%add(comint_watch_for_password_prompt)
        call comint_output_filter_functions%add(comint_postoutput_scroll_to_bottom)
        call syms_of_mule_conf()

        if (.not. allocated(comint_password_prompt_regexp)) then
            comint_password_prompt_regexp = comint_password_prompt_regexp_default()
        end if

        commands%comint_send_invisible%run => cmd_comint_send_invisible
        call add_local_variables_killer(kill_comint_locals)
        ! Keys:
        call keymap_set(comint_mode_map, "M-p", "comint_previous_input", st)
        call keymap_set(comint_mode_map, "M-n", "comint_next_input", st)
        call keymap_set(comint_mode_map, "C-<up>", "comint_previous_input", st)
        call keymap_set(comint_mode_map, "C-<down>", "comint_next_input", st)
        call keymap_set(comint_mode_map, "C-x <up>", "comint_complete_input_ring", st)
        call keymap_set(comint_mode_map, "M-r", "comint_history_isearch_backward_regexp", st)
        call keymap_set(comint_mode_map, "C-c M-r", "comint_previous_matching_input_from_input", st)
        call keymap_set(comint_mode_map, "C-c M-s", "comint_next_matching_input_from_input", st)
        call keymap_set(comint_mode_map, "C-M-l", "comint_show_output", st)
        call keymap_set(comint_mode_map, "RET", "comint_send_input", st)
        call keymap_set(comint_mode_map, "C-d", "comint_delchar_or_maybe_eof", st)
        ! The following two are standardly bound to delete-forward-char, but
        ! they should never do EOF, just delete.
        call keymap_set(comint_mode_map, "<delete>", "delete_forward_char", st)
        call keymap_set(comint_mode_map, "<kp-delete>", "delete_forward_char", st)
        call keymap_set(comint_mode_map, "C-c SPC", "comint_accumulate", st)
        call keymap_set(comint_mode_map, "C-c C-x", "comint_get_next_from_history", st)
        call keymap_set(comint_mode_map, "C-c C-a", "comint_bol_or_process_mark", st)
        call keymap_set(comint_mode_map, "C-c C-u", "comint_kill_input", st)
        call keymap_set(comint_mode_map, "C-c C-w", "backward_kill_word", st)
        call keymap_set(comint_mode_map, "C-c C-c", "comint_interrupt_subjob", st)
        call keymap_set(comint_mode_map, "C-c C-z", "comint_stop_subjob", st)
        call keymap_set(comint_mode_map, "C-c C-\", "comint_quit_subjob", st)
        call keymap_set(comint_mode_map, "C-c RET", "comint_copy_old_input", st)
        call keymap_set(comint_mode_map, "C-c C-o", "comint_delete_output", st)
        call keymap_set(comint_mode_map, "C-c M-o", "comint_clear_buffer", st)
        call keymap_set(comint_mode_map, "C-c C-r", "comint_show_output", st)
        call keymap_set(comint_mode_map, "C-c C-e", "comint_show_maximum_output", st)
        call keymap_set(comint_mode_map, "C-c C-n", "comint_next_prompt", st)
        call keymap_set(comint_mode_map, "C-c C-p", "comint_previous_prompt", st)

        if (.not. keymap_live_p(comint_repeat_map)) then
            call make_sparse_keymap(comint_repeat_map)
            call keymap_set(comint_repeat_map, "C-n", "comint_next_prompt", st)
            call keymap_set(comint_repeat_map, "C-p", "comint_previous_prompt", st)
            call set_repeat_map(comint_repeat_map)
        end if

        call keymap_set(comint_mode_map, "C-c C-d", "comint_send_eof", st)
        call keymap_set(comint_mode_map, "C-c C-s", "comint_write_output", st)
        call keymap_set(comint_mode_map, "C-c .", "comint_insert_previous_argument", st)
        commands%comint_mode%run => comint_mode
        commands%comint_run%run => cmd_comint_run
        commands%comint_send_input%run => cmd_comint_send_input
        commands%comint_previous_input%run => cmd_comint_previous_input
        commands%comint_next_input%run => cmd_comint_next_input
        commands%comint_previous_matching_input%run => cmd_comint_previous_matching_input
        commands%comint_next_matching_input%run => cmd_comint_next_matching_input
        commands%comint_previous_matching_input_from_input%run => cmd_comint_previous_matching_input_from_input
        commands%comint_next_matching_input_from_input%run => cmd_comint_next_matching_input_from_input
        commands%comint_restore_input%run => comint_restore_input
        commands%comint_backward_matching_input%run => cmd_comint_backward_matching_input
        commands%comint_forward_matching_input%run => cmd_comint_forward_matching_input
        commands%comint_show_output%run => comint_show_output
        commands%comint_show_maximum_output%run => comint_show_maximum_output
        commands%comint_delchar_or_maybe_eof%run => comint_delchar_or_maybe_eof
        commands%comint_send_eof%run => comint_send_eof
        commands%comint_accumulate%run => comint_accumulate
        commands%comint_get_next_from_history%run => comint_get_next_from_history
        commands%comint_bol_or_process_mark%run => comint_bol_or_process_mark
        commands%comint_bol%run => cmd_comint_bol
        commands%comint_goto_process_mark%run => cmd_comint_goto_process_mark
        commands%comint_set_process_mark%run => comint_set_process_mark
        commands%comint_kill_input%run => comint_kill_input
        commands%comint_interrupt_subjob%run => comint_interrupt_subjob
        commands%comint_kill_subjob%run => comint_kill_subjob
        commands%comint_quit_subjob%run => comint_quit_subjob
        commands%comint_stop_subjob%run => comint_stop_subjob
        commands%comint_continue_subjob%run => comint_continue_subjob
        commands%comint_replace_by_expanded_history%run => cmd_comint_replace_by_expanded_history
        commands%comint_magic_space%run => cmd_comint_magic_space
        commands%comint_insert_previous_argument%run => cmd_comint_insert_previous_argument
        commands%comint_history_isearch_backward%run => comint_history_isearch_backward
        commands%comint_history_isearch_backward_regexp%run => comint_history_isearch_backward_regexp
        commands%comint_copy_old_input%run => comint_copy_old_input
        commands%comint_delete_output%run => cmd_comint_delete_output
        commands%comint_write_output%run => cmd_comint_write_output
        commands%comint_append_output_to_file%run => cmd_comint_append_output_to_file
        commands%comint_clear_buffer%run => comint_clear_buffer
        commands%comint_truncate_buffer%run => cmd_comint_truncate_buffer
        commands%comint_strip_ctrl_m%run => cmd_comint_strip_ctrl_m
        commands%comint_next_prompt%run => cmd_comint_next_prompt
        commands%comint_previous_prompt%run => cmd_comint_previous_prompt
        commands%comint_kill_whole_line%run => cmd_comint_kill_whole_line
        commands%comint_kill_region%run => cmd_comint_kill_region
        commands%comint_complete_input_ring%run => comint_complete_input_ring
        commands%comint_dynamic_complete_filename%run => cmd_comint_dynamic_complete_filename
        commands%comint_replace_by_expanded_filename%run => comint_replace_by_expanded_filename
        commands%comint_dynamic_list_filename_completions%run => comint_dynamic_list_filename_completions
    end subroutine syms_of_comint

    !> The faces of comint.el.  Not a port.
    subroutine define_faces()
        type(named_face_type), pointer :: f

        f => comint_highlight_input_face
        call f%define("comint_highlight_input", "Face to use to highlight user input.")
        call f%spec_entry(display_when(), face_attrs(weight="bold"))
        f => comint_highlight_prompt
        call f%define("comint_highlight_prompt", "Face to use to highlight prompts.")
        call f%spec_entry(display_when(), face_attrs(inherit="minibuffer_prompt"))
    end subroutine define_faces

    !> Add S at the end of LIST.  Not a port.
    subroutine append_arg(list, s)
        type(process_arg_type), allocatable, intent(inout) :: list(:)
        character(len=*), intent(in) :: s
        type(process_arg_type), allocatable :: grown(:)
        integer :: k, n

        if (.not. allocated(list)) allocate (list(0))
        n = size(list)
        allocate (grown(n + 1))

        do k = 1, n
            if (allocated(list(k)%s)) call move_alloc(list(k)%s, grown(k)%s)
        end do

        grown(n + 1)%s = s
        call move_alloc(grown, list)
    end subroutine append_arg

    !> Add S at the start of LIST.  Not a port.
    subroutine prepend_arg(list, s)
        type(process_arg_type), allocatable, intent(inout) :: list(:)
        character(len=*), intent(in) :: s
        type(process_arg_type), allocatable :: grown(:)
        integer :: k, n

        if (.not. allocated(list)) allocate (list(0))
        n = size(list)
        allocate (grown(n + 1))
        grown(1)%s = s

        do k = 1, n
            if (allocated(list(k)%s)) call move_alloc(list(k)%s, grown(k + 1)%s)
        end do

        call move_alloc(grown, list)
    end subroutine prepend_arg
end module comint_fm
