! bindings_mod.f90 -- the default global key bindings.
! Ported from GNU Emacs 31.1 lisp/bindings.el, with the global keymap
! tree from lisp/subr.el and the self-inserting range from
! lisp/international/mule-conf.el.
!
! Copyright (C) 1985-2026 Free Software Foundation, Inc.
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

!> `initialize_bindings' registers the ported commands and builds global-map,
!> esc-map, ctl-x-map and function-key-map with the bindings upstream
!> gives them, as far as their commands exist.  Bindings to commands not
!> ported yet are left out rather than failing with `void_function'.
module bindings_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use execute_interactively_mod, only: PREFIX_NONE
    use commands_mod, only: commands
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE
    use handle_mod, only: handle_type
    use value_mod, only: value_type, make_keymap_reference, list
    use character_mod, only: MAXIMUM_CHARACTER, CHARACTER_CONTROL, CHARACTER_META
    use keymap_mod, only: make_keymap, make_sparse_keymap, define_key, &
                          define_key_range, use_global_map, set_keymap_parent, &
                          key_parse, is_keymap_live, destroy_keymap, set_repeat_map
    use execute_interactively_mod, only: current_prefix_argument, prefix_numeric_value, intern_command_names
    use page_mod, only: initialize_page
    use button_mod, only: initialize_button
    use keyboard_mod, only: local_function_key_map, initialize_macros_commands, initialize_keyboard_suspend_commands, help_events
    use keyboard_macros_mod, only: initialize_keyboard_macro, keyboard_macro_keymap
    use editing_commands_mod, only: initialize_commands, forward_character, backward_character
    use display_update_mod, only: initialize_display_commands
    use simple_mod, only: initialize_simple, backward_word, region_bounds, is_region_noncontiguous
    use syntax_mod, only: initialize_syntax, forward_word
    use syntax_propertize_mod, only: initialize_syntax_propertize
    use simple_minded_indentation_engine_mod, only: initialize_simple_minded_indentation_engine
    use executable_mod, only: initialize_executable
    use shell_script_mod, only: initialize_shell_script
    use paragraphs_mod, only: initialize_paragraphs
    use expression_motion_mod, only: initialize_expression_motion
    use search_mod, only: initialize_search, begin_running_asynchronous_code, end_running_asynchronous_code
    use replace_mod, only: initialize_replace, initialize_occur
    use casefiddle_mod, only: initialize_casefiddle, casefiddle_region_bounds => region_bounds, &
                              casefiddle_is_region_noncontiguous => is_region_noncontiguous
    use novice_mod, only: initialize_novice
    use utility_functions_mod, only: initialize_functions
    use indent_mod, only: initialize_indent
    use fill_mod, only: initialize_fill
    use incremental_search_mod, only: initialize_incremental_search
    use window_mod, only: initialize_window_commands
    use frame_mod, only: initialize_frame_commands
    use buffer_mod, only: initialize_buffer_commands
    use files_mod, only: initialize_files
    use version_control_hooks_mod, only: initialize_version_control_hooks
    use version_control_mod, only: initialize_version_control
    use version_control_subversion_mod, only: initialize_version_control_subversion
    use version_control_git_mod, only: initialize_version_control_git
    use version_control_mercurial_mod, only: initialize_version_control_mercurial
    use log_view_mod, only: initialize_log_view
    use version_control_directory_mod, only: initialize_version_control_directory
    use project_mod, only: initialize_project
    use version_control_annotate_mod, only: initialize_version_control_annotate
    use autorevert_mod, only: initialize_autorevert
    use repeat_mod, only: initialize_repeat
    use help_mod, only: initialize_help, register_keymap_name, help_map
    use multilingual_environment_mod, only: initialize_multilingual_environment
    use incremental_search_mod, only: incremental_search_mode_map
    use text_mode_mod, only: text_mode_map
    use programming_mode_mod, only: programming_mode_map
    use simple_mod, only: special_mode_map
    use minibuffer_mod, only: minibuffer_local_completion_map, minibuffer_local_must_match_map, &
                              minibuffer_local_filename_completion_map, minibuffer_local_filename_must_match_map
    use help_functions_mod, only: initialize_help_functions
    use apropos_mod, only: initialize_apropos
    use minibuffer_reading_mod, only: initialize_minibuf, minibuffer_local_map
    use minibuffer_mod, only: initialize_minibuffer, completion_at_point
    use basic_subprograms_mod, only: initialize_basic_subprograms
    use text_mode_mod, only: initialize_text_mode
    use configuration_mode_mod, only: initialize_configuration_mode
    use make_mode_mod, only: initialize_make_mode
    use python_mod, only: initialize_python
    use css_mode_mod, only: initialize_css_mode
    use javascript_mod, only: initialize_javascript
    use skeleton_mod, only: initialize_skeleton
    use sgml_mode_mod, only: initialize_sgml_mode
    use f90_mod, only: initialize_f90
    use fortran_completion_mod, only: initialize_fortran_completion
    use programming_mode_mod, only: initialize_programming_mode
    use font_core_mod, only: initialize_font_core
    use font_lock_mod, only: initialize_font_lock
    use just_in_time_lock_mod, only: initialize_just_in_time_lock
    use directory_editor_mode_mod, only: initialize_directory_editor_mode, directory_editor_mode_map
    use view_mod, only: initialize_view
    use follow_mod, only: initialize_follow
    use process_mod, only: initialize_process, begin_asynchronous_code, end_asynchronous_code
    use compile_mod, only: initialize_compile, compilation_mode_map
    use grep_mod, only: initialize_grep
    use cross_reference_mod, only: initialize_cross_reference
    use fortmacs_tags_mod, only: initialize_fortmacs_tags
    use diff_mode_mod, only: initialize_diff_mode
    use simple_merge_mode_mod, only: initialize_simple_merge_mode
    use add_log_mod, only: initialize_add_log
    use diff_mod, only: initialize_diff
    use fortmacs_diff_mod, only: initialize_fortmacs_diff
    use command_interpreter_mod, only: initialize_command_interpreter, command_interpreter_mode_map
    use programmable_completion_mod, only: initialize_programmable_completion
    use authentication_source_mod, only: initialize_authentication_source
    use ansi_color_mod, only: initialize_ansi_color
    use shell_mod, only: initialize_shell, shell_mode_map
    use rectangle_mod, only: initialize_rectangle
    use save_history_mod, only: initialize_save_history
    use winner_mod, only: initialize_winner
    use tab_bar_mod, only: initialize_tab_bar
    use tab_line_mod, only: initialize_tab_line
    use window_move_mod, only: initialize_windmove
    use saveplace_mod, only: initialize_saveplace
    use recent_files_mod, only: initialize_recent_files
    use tabulated_list_mod, only: initialize_tabulated_list
    use simple_mod, only: initialize_simple_process
    use bookmark_mod, only: initialize_bookmark, bookmark_map, bookmark_minibuffer_read_name_map
    use info_mod, only: initialize_info
    use info_lookup_mod, only: initialize_info_look
    use buffer_menu_mod, only: initialize_buffer_menu
    use calendar_mod, only: initialize_calendar
    use holidays_mod, only: initialize_holidays
    use diary_library_mod, only: initialize_diary_library
    use ibuffer_mod, only: initialize_ibuffer
    use completing_read_multiple_mod, only: initialize_completing_read_multiple
    use register_mod, only: initialize_register
    use abbreviation_mod, only: initialize_abbreviation, edit_abbreviations_mode_map
    use dynamic_abbreviation_mod, only: initialize_dynamic_abbreviation
    use spellcheck_mod, only: initialize_spellcheck
    use spellcheck_mode_mod, only: initialize_spellcheck_mode
    use parenthesis_mod, only: initialize_parenthesis
    use index_menu_mod, only: initialize_index_menu
    use which_function_mod, only: initialize_which_function
    use electric_mod, only: initialize_electric
    use electric_pair_mod, only: initialize_electric_pair
    use newcomment_mod, only: initialize_newcomment
    use mail_utilities_mod, only: initialize_mail_utilities
    use quoted_printable_mod, only: initialize_quoted_printable
    use rfc2047_mod, only: initialize_rfc2047
    use mail_extract_mod, only: initialize_mail_extract
    use send_mail_mod, only: initialize_sendmail, mail_mode_map
    use smtpmail_mod, only: initialize_smtpmail
    use rmail_mod, only: initialize_rmail
    use rmail_keywords_mod, only: initialize_rmail_keywords
    use rmail_edit_mod, only: initialize_rmail_edit
    use rmail_output_mod, only: initialize_rmail_output
    use rmail_summary_mod, only: initialize_rmail_summary
    use rmail_mime_mod, only: initialize_rmail_mime
    use message_mod, only: initialize_message
    use mail_abbreviations_mod, only: initialize_mailabbrev
    use simple_mod, only: initialize_simple_mail
    use modes_mod, only: minor_modes
    implicit none(type, external)
    private

    public :: initialize_bindings

    integer(int64), parameter :: M_CONTROL = int(CHARACTER_CONTROL, int64), &
                                 M_META = int(CHARACTER_META, int64)

    !> subr.el:global-map, esc-map, ctl-x-map; keymap.c:Vfunction_key_map;
    !> bindings.el:ctl-x-x-map (keymap for subcommands of `C-x x');
    !> bindings.el:ctl-x-r-map (keymap for subcommands of `C-x r');
    !> subr.el:ctl-x-4-map (keymap for subcommands of `C-x 4');
    !> subr.el:ctl-x-5-map (keymap for frame commands, `C-x 5').
    type(handle_type), public :: global_map, escape_map, control_x_map, &
                                 function_key_map, control_x_x_map, control_x_r_map, control_x_4_map, control_x_5_map
    !> subr.el:tab-prefix-map (keymap for tab-bar related commands, `C-x t').
    type(handle_type), public :: tab_prefix_map
    !> bindings.el:goto-map, keymap for navigation commands (M-g).
    type(handle_type), public :: goto_map
    !> bindings.el:narrow-map, keymap for narrowing commands (C-x n).
    type(handle_type), public :: narrow_map
    !> bindings.el:search-map, keymap for search related commands (M-s).
    type(handle_type), public :: search_map

    !> bindings.el:abbrev-map (keymap for abbrev commands).
    type(handle_type), public :: abbreviation_map

    !> The repeat maps (keymaps to repeat commands in `repeat_mode'):
    !> bindings.el:undo-repeat-map (`undo'), buffer-navigation-repeat-map
    !> (`next_buffer', `previous_buffer'), next-error-repeat-map
    !> (`next_error', `previous_error'), page-navigation-repeat-map
    !> (`forward_page', `backward_page'); window.el:other-window-repeat-map
    !> (`other_window', `other_window_backward').
    type(handle_type), public :: undo_repeat_map, buffer_navigation_repeat_map, next_error_repeat_map, &
                                 page_navigation_repeat_map, other_window_repeat_map

contains

    !> Perform completion on the text around point.
    !> The completion method is determined by `completion_at_point_functions'.
    !>
    !> With a prefix argument, this command does completion within
    !> the collection of names listed in the index of the manual for the
    !> language you are using (info_complete_name, not ported yet).
    !> Port of bindings.el:complete-symbol.
    subroutine execute_complete_name(status)
        type(status_type), intent(out) :: status

        if (current_prefix_argument%kind /= PREFIX_NONE) then
            call set_error(status, GENERIC_ERROR_CODE, "info_complete_name is not ported yet")
            return
        end if

        call completion_at_point(status)
    end subroutine execute_complete_name

    !> Move point N characters to the right (to the left if N is negative).
    !> On reaching beginning or end of buffer, stop, and an error condition occurs.
    !>
    !> In left-to-right text this moves forward in the buffer, like
    !> \[forward_character].  Port of bindings.el:right-char (left-to-right
    !> paragraphs only).
    subroutine execute_right_character(status)
        type(status_type), intent(out) :: status

        call forward_character(status, prefix_numeric_value(current_prefix_argument))
    end subroutine execute_right_character

    !> Move point N characters to the left (to the right if N is negative).
    !> On reaching beginning or end of buffer, stop, and an error condition occurs.
    !>
    !> In left-to-right text this moves backward in the buffer, like
    !> \[backward_character].  Port of bindings.el:left-char (left-to-right
    !> paragraphs only).
    subroutine execute_left_character(status)
        type(status_type), intent(out) :: status

        call backward_character(status, prefix_numeric_value(current_prefix_argument))
    end subroutine execute_left_character

    !> Move point N words to the right (to the left if N is negative).
    !>
    !> In left-to-right text this moves forward in the buffer, like
    !> \[forward_word].
    !>
    !> The word boundaries are normally determined by the buffer's syntax
    !> table and character script (according to `character_script_table').
    !>
    !> If an edge of the buffer or a field boundary is reached, point is left there.
    !> Field boundaries are not noticed if `inhibit_field_text_motion' is
    !> true.  Port of bindings.el:right-word (left-to-right paragraphs
    !> only).
    subroutine execute_right_word(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call forward_word(prefix_numeric_value(current_prefix_argument))
    end subroutine execute_right_word

    !> Move point N words to the left (to the right if N is negative).
    !>
    !> In left-to-right text this moves backward in the buffer, like
    !> \[backward_word].
    !>
    !> The word boundaries are normally determined by the buffer's syntax
    !> table and character script (according to `character_script_table').
    !>
    !> If an edge of the buffer or a field boundary is reached, point is left there.
    !> Field boundaries are not noticed if `inhibit_field_text_motion' is
    !> true.  Port of bindings.el:left-word (left-to-right paragraphs
    !> only).
    subroutine execute_left_word(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call backward_word(prefix_numeric_value(current_prefix_argument))
    end subroutine execute_left_word

    !> Bind `keys' (kbd syntax) to command `command' in `map'.  Not a port.
    subroutine bind(map, keys, command)
        type(handle_type), intent(in) :: map
        character(len=*), intent(in) :: keys, command
        type(value_type), allocatable :: key(:)
        type(status_type) :: status

        call key_parse(keys, key, status)
        call define_key(map, key, value_type(name=command), status)
    end subroutine bind

    !> Bind the single event `c' to command `command' in `map'.  Not a port.
    subroutine bind_character(map, c, command)
        type(handle_type), intent(in) :: map
        integer(int64), intent(in) :: c
        character(len=*), intent(in) :: command
        type(value_type) :: key(1)
        type(status_type) :: status

        key(1) = value_type(c)
        call define_key(map, key, value_type(name=command), status)
    end subroutine bind_character

    !> In `map', translate `from' (kbd syntax) to the event `to'.
    !> Not a port.
    subroutine translate(map, from, to)
        type(handle_type), intent(in) :: map
        character(len=*), intent(in) :: from
        type(value_type), intent(in) :: to
        type(value_type), allocatable :: key(:)
        type(status_type) :: status

        call key_parse(from, key, status)
        call define_key(map, key, list(to), status)
    end subroutine translate

    !> Register the commands and install the default keymaps.  Safe to
    !> call again (the maps are rebuilt).
    !> Port of the top-level key bindings of bindings.el (in part).
    subroutine initialize_bindings()
        type(status_type) :: status
        type(value_type) :: key(1)
        integer(int64) :: c

        call intern_command_names()
        ! bindings.el's help-event-list.
        help_events = [character(len=16) :: "<help>", "<f1>", "?"]
        ! Rebuilding: drop the old maps.
        if (is_keymap_live(global_map)) call destroy_keymap(global_map, status)
        if (is_keymap_live(escape_map)) call destroy_keymap(escape_map, status)
        if (is_keymap_live(control_x_map)) call destroy_keymap(control_x_map, status)
        if (is_keymap_live(control_x_x_map)) call destroy_keymap(control_x_x_map, status)
        if (is_keymap_live(control_x_4_map)) call destroy_keymap(control_x_4_map, status)
        if (is_keymap_live(control_x_5_map)) call destroy_keymap(control_x_5_map, status)
        if (is_keymap_live(tab_prefix_map)) call destroy_keymap(tab_prefix_map, status)
        if (is_keymap_live(control_x_r_map)) call destroy_keymap(control_x_r_map, status)
        if (is_keymap_live(function_key_map)) call destroy_keymap(function_key_map, status)
        if (is_keymap_live(goto_map)) call destroy_keymap(goto_map, status)
        if (is_keymap_live(narrow_map)) call destroy_keymap(narrow_map, status)
        if (is_keymap_live(search_map)) call destroy_keymap(search_map, status)
        if (is_keymap_live(abbreviation_map)) call destroy_keymap(abbreviation_map, status)
        if (is_keymap_live(undo_repeat_map)) call destroy_keymap(undo_repeat_map, status)
        if (is_keymap_live(buffer_navigation_repeat_map)) call destroy_keymap(buffer_navigation_repeat_map, status)
        if (is_keymap_live(next_error_repeat_map)) call destroy_keymap(next_error_repeat_map, status)
        if (is_keymap_live(page_navigation_repeat_map)) call destroy_keymap(page_navigation_repeat_map, status)
        if (is_keymap_live(other_window_repeat_map)) call destroy_keymap(other_window_repeat_map, status)
        ! First: bindings.el's minor-mode-alist starts with abbrev-mode.
        call initialize_abbreviation()
        call initialize_commands()
        call initialize_display_commands()
        call initialize_simple()
        call initialize_window_commands()
        call initialize_buffer_commands()
        commands%right_character%execute => execute_right_character
        commands%complete_name%execute => execute_complete_name
        commands%left_character%execute => execute_left_character
        commands%right_word%execute => execute_right_word
        commands%left_word%execute => execute_left_word
        call initialize_syntax()
        call initialize_syntax_propertize()
        call initialize_simple_minded_indentation_engine()
        call initialize_paragraphs()
        call initialize_expression_motion()
        call initialize_search()
        ! A process filter or sentinel keeps the match data (search.c).
        begin_asynchronous_code => begin_running_asynchronous_code
        end_asynchronous_code => end_running_asynchronous_code
        call initialize_replace()
        call initialize_casefiddle()
        casefiddle_region_bounds => region_bounds
        casefiddle_is_region_noncontiguous => is_region_noncontiguous
        call initialize_functions()
        call initialize_indent()
        call initialize_fill()
        call initialize_newcomment()
        call initialize_novice()
        call initialize_text_mode()
        call initialize_index_menu()
        call initialize_configuration_mode()
        call initialize_make_mode()
        call initialize_python()
        call initialize_css_mode()
        call initialize_javascript()
        call initialize_skeleton()
        call initialize_sgml_mode()
        call initialize_f90()
        call initialize_fortran_completion()
        call initialize_programming_mode()
        call initialize_just_in_time_lock()
        call initialize_font_core()
        call initialize_font_lock()
        call initialize_executable()
        call initialize_shell_script()
        call initialize_process()
        call initialize_command_interpreter()
        call initialize_programmable_completion()
        call initialize_ansi_color()
        call initialize_shell()
        call initialize_compile()
        call initialize_occur()
        call initialize_diff_mode()
        call initialize_diff()
        call initialize_fortmacs_diff()
        call initialize_rectangle()
        call initialize_save_history()
        call initialize_winner()
        call initialize_windmove()
        call initialize_saveplace()
        call initialize_recent_files()
        call initialize_tabulated_list()
        call initialize_simple_process()
        call initialize_bookmark()
        call initialize_buffer_menu()
        call initialize_calendar()
        call initialize_holidays()
        call initialize_diary_library()
        call initialize_ibuffer()
        call initialize_info()
        call initialize_info_look()
        call initialize_register()
        call initialize_macros_commands()
        call initialize_keyboard_suspend_commands()
        call initialize_keyboard_macro()
        call initialize_dynamic_abbreviation()
        call initialize_spellcheck()
        call initialize_spellcheck_mode()
        ! subr.el: esc-map, ctl-x-map, global-map.
        call make_keymap(escape_map)
        call make_keymap(control_x_map)
        call make_keymap(global_map)
        key(1) = value_type(27_int64)
        call define_key(global_map, key, make_keymap_reference(escape_map), status)
        key(1) = value_type(24_int64)
        call define_key(global_map, key, make_keymap_reference(control_x_map), status)
        ! bindings.el: narrow-map.
        call make_sparse_keymap(narrow_map)
        call bind(narrow_map, "n", "narrow_to_region")
        call bind(narrow_map, "w", "widen")
        call bind(narrow_map, "g", "goto_line_relative")
        key(1) = value_type(int(iachar("n"), int64))
        call define_key(control_x_map, key, make_keymap_reference(narrow_map), status)
        call bind_character(global_map, 9_int64, "self_insert_command")

        do c = 32, 126
            call bind_character(global_map, c, "self_insert_command")
        end do

        ! mule-conf.el: all non-ASCII characters self-insert (this covers
        ! subr.el's 0240..0377 loop too).
        call define_key_range(global_map, 128_int64, int(MAXIMUM_CHARACTER, int64), &
                              value_type(name="self_insert_command"), status)
        call bind(global_map, "C-a", "move_beginning_of_line")
        call bind(global_map, "C-b", "backward_character")
        call bind(global_map, "C-e", "move_end_of_line")
        call bind(global_map, "C-f", "forward_character")
        call bind(global_map, "C-M-c", "exit_recursive_edit")
        call bind(global_map, "C-]", "abort_recursive_edit")
        ! subr.el binds suspend-emacs; bindings.el substitutes suspend-frame.
        call bind(global_map, "C-z", "suspend_frame")
        call bind(control_x_map, "C-z", "suspend_frame")
        call bind(global_map, "C-n", "next_line")
        call bind(global_map, "C-p", "previous_line")
        ! bindings.el
        call bind(global_map, "C-g", "keyboard_quit")
        call bind(global_map, "C-m", "newline")
        call bind(global_map, "C-o", "open_line")
        call bind(global_map, "C-q", "quoted_insert")
        call bind(global_map, "C-u", "universal_argument")

        do c = 0, 9
            call bind_character(escape_map, iachar("0") + c, "digit_argument")
            call bind_character(global_map, M_CONTROL + iachar("0") + c, "digit_argument")
            call bind_character(global_map, M_CONTROL + M_META + iachar("0") + c, "digit_argument")
        end do

        call bind(escape_map, "-", "negative_argument")
        call bind_character(global_map, M_CONTROL + iachar("-"), "negative_argument")
        call bind_character(global_map, M_CONTROL + M_META + iachar("-"), "negative_argument")
        call bind(global_map, "DEL", "delete_backward_character")
        call bind(global_map, "C-d", "delete_character")
        call bind(global_map, "<home>", "move_beginning_of_line")
        call bind(global_map, "<end>", "move_end_of_line")
        call bind(global_map, "<left>", "left_character")
        call bind(global_map, "<right>", "right_character")
        call bind(global_map, "<down>", "next_line")
        call bind(global_map, "<up>", "previous_line")
        call bind(global_map, "<deletechar>", "delete_forward_character")
        call bind(global_map, "<menu>", "execute_extended_command")
        call bind(global_map, "<find>", "search_forward")
        call bind(global_map, "<begin>", "beginning_of_buffer")
        call bind(global_map, "<execute>", "execute_extended_command")
        call bind(global_map, "C-<insert>", "copy_region")
        call bind(global_map, "S-<insert>", "paste")
        call bind(global_map, "C-<insertchar>", "copy_region")
        call bind(global_map, "S-<insertchar>", "paste")
        call bind(global_map, "<copy>", "copy_region")
        call bind(global_map, "<paste>", "paste")
        call bind(global_map, "<cut>", "cut_region")
        call bind(global_map, "<undo>", "undo")
        call bind(global_map, "<open>", "find_file")
        call bind(global_map, "<insertline>", "open_line")
        call bind(global_map, "<deleteline>", "cut_line")
        call bind(global_map, "S-<delete>", "cut_region")
        call bind(escape_map, "x", "execute_extended_command")
        call bind(escape_map, "TAB", "complete_name")
        call bind(global_map, "<next>", "scroll_up_command")
        call bind(global_map, "<prior>", "scroll_down_command")
        call bind(global_map, "C-v", "scroll_up_command")
        call bind(escape_map, "v", "scroll_down_command")
        call bind(escape_map, "C-v", "scroll_other_window")
        call bind(global_map, "M-<next>", "scroll_other_window")
        call bind(escape_map, "<next>", "scroll_other_window")
        call bind(global_map, "M-<prior>", "scroll_other_window_down")
        call bind(escape_map, "<prior>", "scroll_other_window_down")
        call bind(escape_map, "C-S-v", "scroll_other_window_down")
        ! window.el
        call bind(global_map, "C-l", "recenter_top_bottom")
        call bind(escape_map, "r", "move_to_window_line_top_bottom")
        ! Undo (bindings.el).
        commands%undo%advertised_binding = "C-x u"
        call bind(global_map, "C-_", "undo")
        call bind(global_map, "C-/", "undo")
        call bind(control_x_map, "u", "undo")
        call make_sparse_keymap(undo_repeat_map)
        call bind(undo_repeat_map, "u", "undo")
        call set_repeat_map(undo_repeat_map)
        ! bindings.el: ctl-x-x-map.
        call make_sparse_keymap(control_x_x_map)
        call bind(control_x_x_map, "f", "font_lock_update")
        key(1) = value_type(int(iachar("x"), int64))
        call define_key(control_x_map, key, make_keymap_reference(control_x_x_map), status)
        ! bindings.el: ctl-x-r-map (rect.el, register.el and bookmark.el;
        ! frameset-to-register on f is not ported).
        call bind(control_x_map, "SPC", "rectangle_mark_mode")
        call make_sparse_keymap(control_x_r_map)
        call bind(control_x_r_map, "c", "clear_rectangle")
        call bind(control_x_r_map, "k", "cut_rectangle")
        call bind(control_x_r_map, "d", "delete_rectangle")
        call bind(control_x_r_map, "y", "paste_rectangle")
        call bind(control_x_r_map, "o", "open_rectangle")
        call bind(control_x_r_map, "t", "string_rectangle")
        call bind(control_x_r_map, "N", "rectangle_number_lines")
        call bind(control_x_r_map, "M-w", "copy_rectangle")
        call bind(control_x_r_map, "C-@", "point_to_register")
        call bind(control_x_r_map, "C-SPC", "point_to_register")
        call bind(control_x_r_map, "SPC", "point_to_register")
        call bind(control_x_r_map, "j", "jump_to_register")
        call bind(control_x_r_map, "s", "copy_to_register")
        call bind(control_x_r_map, "x", "copy_to_register")
        call bind(control_x_r_map, "i", "insert_register")
        call bind(control_x_r_map, "g", "insert_register")
        call bind(control_x_r_map, "r", "copy_rectangle_to_register")
        call bind(control_x_r_map, "n", "number_to_register")
        call bind(control_x_r_map, "+", "increment_register")
        call bind(control_x_r_map, "w", "window_configuration_to_register")
        call bind(control_x_r_map, "F", "file_to_register")
        call bind(control_x_r_map, "B", "buffer_to_register")
        ! bookmark.el (autoloaded bindings).
        call bind(control_x_r_map, "b", "bookmark_jump")
        call bind(control_x_r_map, "m", "bookmark_set")
        call bind(control_x_r_map, "M", "bookmark_set_no_overwrite")
        call bind(control_x_r_map, "l", "bookmark_bookmark_menu_list")
        key(1) = value_type(int(iachar("r"), int64))
        call define_key(control_x_map, key, make_keymap_reference(control_x_r_map), status)
        ! kmacro.el (autoloads): keyboard macros.
        call bind(control_x_map, "(", "keyboard_macro_start_macro")
        call bind(control_x_map, ")", "keyboard_macro_end_macro")
        call bind(control_x_map, "e", "keyboard_macro_end_and_execute_macro")
        call bind(global_map, "<f3>", "keyboard_macro_start_macro_or_insert_counter")
        call bind(global_map, "<f4>", "keyboard_macro_end_or_execute_macro")
        key(1) = value_type(11_int64)
        call define_key(control_x_map, key, make_keymap_reference(keyboard_macro_keymap), status)
        ! bindings.el: abbrev-map, and the abbrev keys.
        call make_sparse_keymap(abbreviation_map)
        call bind(abbreviation_map, "l", "add_mode_abbreviation")
        call bind(abbreviation_map, "C-a", "add_mode_abbreviation")
        call bind(abbreviation_map, "g", "add_global_abbreviation")
        call bind(abbreviation_map, "+", "add_mode_abbreviation")
        call bind(abbreviation_map, "i g", "inverse_add_global_abbreviation")
        call bind(abbreviation_map, "i l", "inverse_add_mode_abbreviation")
        call bind(abbreviation_map, "-", "inverse_add_global_abbreviation")
        call bind(abbreviation_map, "e", "expand_abbreviation")
        call bind(abbreviation_map, "'", "expand_abbreviation")
        key(1) = value_type(int(iachar("a"), int64))
        call define_key(control_x_map, key, make_keymap_reference(abbreviation_map), status)
        call bind(escape_map, "'", "abbreviation_prefix_mark")
        call bind(control_x_map, "'", "expand_abbreviation")
        ! dabbrev.el (autoloads).
        call bind(escape_map, "/", "dynamic_abbreviation_expand")
        call bind(global_map, "C-M-/", "dynamic_abbreviation_completion")
        ! ispell.el (autoloads).
        call bind(escape_map, "$", "spellcheck_word")
        call bind(global_map, "C-?", "undo_redo")
        call bind(global_map, "C-M-_", "undo_redo")
        ! The mark and killing (bindings.el).
        commands%set_mark_command%advertised_binding = "C-SPC"
        call bind(global_map, "C-@", "set_mark_command")
        call bind(global_map, "C-SPC", "set_mark_command")
        call bind(control_x_map, "C-x", "exchange_point_and_mark")
        call bind(control_x_map, "h", "mark_whole_buffer")
        ! Pages (bindings.el).
        call initialize_page()
        call bind(control_x_map, "[", "backward_page")
        call bind(control_x_map, "]", "forward_page")
        call bind(control_x_map, "C-p", "mark_page")
        call bind(control_x_map, "l", "count_lines_page")
        call bind(control_x_map, "n p", "narrow_to_page")
        call make_sparse_keymap(page_navigation_repeat_map)
        call bind(page_navigation_repeat_map, "]", "forward_page")
        call bind(page_navigation_repeat_map, "[", "backward_page")
        call set_repeat_map(page_navigation_repeat_map)
        ! Windows (window.el) and buffers (subr.el ctl-x-map).
        call bind(control_x_map, "0", "delete_window")
        call bind(control_x_map, "1", "delete_other_windows")
        call bind(control_x_map, "2", "split_window_below")
        call bind(control_x_map, "3", "split_window_right")
        call bind(control_x_map, "o", "other_window")
        call make_sparse_keymap(other_window_repeat_map)
        call bind(other_window_repeat_map, "o", "other_window")
        call bind(other_window_repeat_map, "O", "other_window_backward")
        call set_repeat_map(other_window_repeat_map)
        call bind(control_x_map, "b", "switch_to_buffer")
        call bind(control_x_map, "<right>", "next_buffer")
        call bind(control_x_map, "C-<right>", "next_buffer")
        call bind(global_map, "<XF86Forward>", "next_buffer")
        call bind(control_x_map, "<left>", "previous_buffer")
        call bind(control_x_map, "C-<left>", "previous_buffer")
        call bind(global_map, "<XF86Back>", "previous_buffer")
        call make_sparse_keymap(buffer_navigation_repeat_map)
        call bind(buffer_navigation_repeat_map, "<right>", "next_buffer")
        call bind(buffer_navigation_repeat_map, "<left>", "previous_buffer")
        call set_repeat_map(buffer_navigation_repeat_map)
        call bind(control_x_map, "k", "close_buffer")
        call bind(global_map, "C-w", "cut_region")
        call bind(escape_map, "w", "copy_region")
        call bind(global_map, "C-k", "cut_line")
        call bind(global_map, "C-y", "paste")
        call bind(escape_map, "y", "paste_previous")
        call bind(global_map, "<C-S-backspace>", "cut_whole_line")
        ! Words and paragraphs (bindings.el).
        call bind(escape_map, "@", "mark_word")
        call bind(escape_map, "f", "forward_word")
        call bind(escape_map, "b", "backward_word")
        call bind(escape_map, "d", "cut_word")
        call bind(escape_map, "DEL", "backward_cut_word")
        call bind(global_map, "M-<right>", "right_word")
        call bind(global_map, "M-;", "comment_do_what_i_mean")
        call bind(global_map, "C-x ;", "comment_set_column")
        call bind(global_map, "C-x C-;", "comment_line")
        call bind(escape_map, "<right>", "forward_word")
        call bind(global_map, "M-<left>", "left_word")
        call bind(escape_map, "<left>", "backward_word")
        call bind(global_map, "C-<right>", "right_word")
        call bind(global_map, "C-<left>", "left_word")
        call bind(global_map, "C-<delete>", "cut_word")
        call bind(global_map, "C-<backspace>", "backward_cut_word")
        call bind(global_map, "C-<up>", "backward_paragraph")
        call bind(global_map, "C-<down>", "forward_paragraph")
        call bind(escape_map, "{", "backward_paragraph")
        call bind(escape_map, "}", "forward_paragraph")
        call bind(escape_map, "h", "mark_paragraph")
        call bind(escape_map, "a", "backward_sentence")
        call bind(escape_map, "e", "forward_sentence")
        call bind(escape_map, "k", "cut_sentence")
        call bind(control_x_map, "DEL", "backward_cut_sentence")
        ! Balanced expressions (bindings.el).
        call bind(global_map, "C-M-<left>", "backward_expression")
        call bind(escape_map, "C-<left>", "backward_expression")
        call bind(global_map, "C-M-<right>", "forward_expression")
        call bind(escape_map, "C-<right>", "forward_expression")
        call bind(global_map, "C-M-<up>", "backward_up_group")
        call bind(escape_map, "C-<up>", "backward_up_group")
        call bind(global_map, "C-M-<down>", "down_group")
        call bind(escape_map, "C-<down>", "down_group")
        call bind(escape_map, "C-f", "forward_expression")
        call bind(escape_map, "C-b", "backward_expression")
        call bind(escape_map, "C-u", "backward_up_group")
        call bind(escape_map, "C-@", "mark_expression")
        call bind(escape_map, "C-SPC", "mark_expression")
        call bind(escape_map, "C-d", "down_group")
        call bind(escape_map, "C-k", "cut_expression")
        call bind(global_map, "C-M-<delete>", "backward_cut_expression")
        call bind(global_map, "C-M-<backspace>", "backward_cut_expression")
        call bind(escape_map, "C-<delete>", "backward_cut_expression")
        call bind(escape_map, "C-<backspace>", "backward_cut_expression")
        call bind(global_map, "C-M-<home>", "beginning_of_subprogram")
        call bind(escape_map, "C-<home>", "beginning_of_subprogram")
        call bind(global_map, "C-M-<end>", "end_of_subprogram")
        call bind(escape_map, "C-<end>", "end_of_subprogram")
        call bind(escape_map, "C-a", "beginning_of_subprogram")
        call bind(escape_map, "C-e", "end_of_subprogram")
        call bind(escape_map, "C-h", "mark_subprogram")
        call bind(control_x_map, "n d", "narrow_to_subprogram")
        call bind(escape_map, "(", "insert_parentheses")
        call bind(escape_map, "C-n", "forward_group")
        call bind(escape_map, "C-p", "backward_group")
        call bind(escape_map, "%", "query_replace")
        call bind(escape_map, "C-%", "query_replace_regular_expression")
        call bind(escape_map, "!", "shell_command")
        call bind(escape_map, "|", "shell_command_on_region")
        call bind(escape_map, "&", "asynchronous_shell_command")
        call bind(control_x_map, "`", "next_error")
        call make_sparse_keymap(next_error_repeat_map)
        call bind(next_error_repeat_map, "n", "next_error")
        call bind(next_error_repeat_map, "M-n", "next_error")
        call bind(next_error_repeat_map, "p", "previous_error")
        call bind(next_error_repeat_map, "M-p", "previous_error")
        call set_repeat_map(next_error_repeat_map)
        ! bindings.el: (define-key ctl-x-map "z" 'repeat)
        call bind(control_x_map, "z", "repeat")
        ! bindings.el: goto-map (the keys of ported commands).
        call make_sparse_keymap(goto_map)
        call bind(goto_map, "n", "next_error")
        call bind(goto_map, "M-n", "next_error")
        call bind(goto_map, "p", "previous_error")
        call bind(goto_map, "M-p", "previous_error")
        call bind(goto_map, "c", "goto_character")
        call bind(goto_map, "g", "goto_line")
        call bind(goto_map, "M-g", "goto_line")
        call bind(goto_map, "TAB", "move_to_column")
        call bind(goto_map, "i", "index_menu")
        key(1) = value_type(int(iachar("g"), int64))
        call define_key(escape_map, key, make_keymap_reference(goto_map), status)
        ! bindings.el: search-map (the keys of ported commands).
        call make_sparse_keymap(search_map)
        call bind(search_map, "o", "occur")
        key(1) = value_type(int(iachar("s"), int64))
        call define_key(escape_map, key, make_keymap_reference(search_map), status)
        call bind(global_map, "C-t", "transpose_characters")
        call bind(escape_map, "t", "transpose_words")
        call bind(control_x_map, "C-b", "list_buffers")
        call bind(escape_map, "=", "count_words_region")
        call bind(control_x_map, "C-t", "transpose_lines")
        call bind(escape_map, "SPC", "cycle_spacing")
        call bind(escape_map, "\", "delete_horizontal_space")
        call bind(control_x_map, "C-o", "delete_blank_lines")
        call bind(escape_map, "^", "delete_indentation")
        call bind(escape_map, "u", "upcase_word")
        call bind(escape_map, "l", "downcase_word")
        call bind(escape_map, "c", "capitalize_word")
        call bind(control_x_map, "C-u", "upcase_region")
        call bind(control_x_map, "C-l", "downcase_region")
        call bind(global_map, "TAB", "indent_for_tab_command")
        call bind(escape_map, "C-\", "indent_region")
        call bind(control_x_map, "TAB", "indent_rigidly")
        call bind(escape_map, "i", "tab_to_tab_stop")
        call bind(escape_map, "m", "back_to_indentation")
        call bind(escape_map, "q", "fill_paragraph")
        call bind(control_x_map, "f", "set_fill_column")
        call bind(control_x_map, ".", "set_fill_prefix")
        call bind(escape_map, "j", "default_indent_new_line")
        call bind(escape_map, "C-j", "default_indent_new_line")
        call bind(escape_map, "<", "beginning_of_buffer")
        call bind(escape_map, ">", "end_of_buffer")
        call bind(global_map, "C-<home>", "beginning_of_buffer")
        call bind(global_map, "C-<end>", "end_of_buffer")
        call use_global_map(global_map, status)
        ! subr.el: (define-key ctl-x-map "4" 'ctl-x-4-prefix)
        call make_sparse_keymap(control_x_4_map)
        key(1) = value_type(int(iachar("4"), int64))
        call define_key(control_x_map, key, make_keymap_reference(control_x_4_map), status)
        call initialize_files(control_x_map, control_x_4_map)
        ! subr.el: (define-key ctl-x-map "5" 'ctl-x-5-prefix), then frame.el's
        ! keys.
        call make_sparse_keymap(control_x_5_map)
        key(1) = value_type(int(iachar("5"), int64))
        call define_key(control_x_map, key, make_keymap_reference(control_x_5_map), status)
        call initialize_frame_commands()
        call bind(control_x_5_map, "2", "make_frame_command")
        call bind(control_x_5_map, "1", "delete_other_frames")
        call bind(control_x_5_map, "0", "delete_frame")
        call bind(control_x_5_map, "o", "other_frame")
        ! files.el
        call bind(control_x_5_map, "b", "switch_to_buffer_other_frame")
        call bind(control_x_5_map, "f", "find_file_other_frame")
        call bind(control_x_5_map, "C-f", "find_file_other_frame")
        call bind(control_x_5_map, "r", "find_file_read_only_other_frame")
        call bind(control_x_5_map, "C-o", "display_buffer_other_frame")
        ! subr.el: (define-key ctl-x-map "t" tab-prefix-map), then tab-bar.el's
        ! keys.
        call make_sparse_keymap(tab_prefix_map)
        key(1) = value_type(int(iachar("t"), int64))
        call define_key(control_x_map, key, make_keymap_reference(tab_prefix_map), status)
        call initialize_tab_bar(tab_prefix_map)
        call initialize_tab_line(control_x_map)
        call initialize_version_control_hooks(control_x_map)
        ! (After version_control_hooks: `grep_find_ignored_directories' defaults to
        ! `version_control_directory_exclusions'.)
        call initialize_grep()
        call initialize_cross_reference(escape_map, control_x_4_map)
        call initialize_fortmacs_tags()
        call initialize_version_control()
        call initialize_log_view()
        call initialize_version_control_directory()
        call initialize_project(control_x_map)
        call initialize_autorevert()
        call initialize_repeat()
        call initialize_version_control_subversion()
        call initialize_simple_merge_mode()
        call initialize_version_control_git()
        call initialize_version_control_mercurial()
        call initialize_version_control_annotate()
        call initialize_view()
        call initialize_follow()
        call initialize_directory_editor_mode(control_x_map, control_x_4_map, control_x_5_map, tab_prefix_map)
        ! Mail: mail-utils.el, mail-extr.el, sendmail.el, smtpmail.el, mailabbrev.el, message.el and
        ! simple.el's compose-mail, with their keys from bindings.el.
        call initialize_mail_utilities()
        call initialize_quoted_printable()
        call initialize_rfc2047()
        call initialize_mail_extract()
        call initialize_sendmail()
        call initialize_smtpmail()
        call initialize_mailabbrev()
        call initialize_message()
        call initialize_rmail()
        call initialize_rmail_keywords()
        call initialize_rmail_edit()
        call initialize_rmail_output()
        call initialize_rmail_summary()
        call initialize_rmail_mime()
        call initialize_simple_mail()
        call bind(control_x_map, "m", "compose_mail")
        call bind(control_x_4_map, "m", "compose_mail_other_window")
        ! add-log.el: (define-key ctl-x-4-map "a" 'add-change-log-entry-other-window)
        call initialize_add_log(control_x_4_map)
        call initialize_incremental_search(global_map)
        ! The minibuffer: minibuffer-local-map's keys from minibuffer.el,
        ! then its keys from bindings.el.
        if (is_keymap_live(minibuffer_local_map)) call destroy_keymap(minibuffer_local_map, status)
        call initialize_minibuf()
        call initialize_minibuffer()
        call initialize_authentication_source()
        call initialize_completing_read_multiple()
        call initialize_basic_subprograms()
        call bind(minibuffer_local_map, "M-n", "next_history_element")
        call bind(minibuffer_local_map, "<next>", "next_history_element")
        call bind(minibuffer_local_map, "<down>", "next_line_or_history_element")
        call bind(minibuffer_local_map, "M-p", "previous_history_element")
        call bind(minibuffer_local_map, "<prior>", "previous_history_element")
        call bind(minibuffer_local_map, "<up>", "previous_line_or_history_element")
        ! Override the global binding (which calls indent-relative via
        ! indent-for-tab-command).  The alignment that indent-relative
        ! tries to do doesn't make much sense here since the prompt messes
        ! it up.
        call bind(minibuffer_local_map, "TAB", "self_insert_command")
        ! Help (help.el, help-fns.el), after the maps it names exist.
        call initialize_help(global_map)
        ! Buttons (button.el).
        call initialize_button()
        call initialize_help_functions()
        call initialize_apropos()
        call initialize_multilingual_environment(global_map, control_x_map, help_map)
        ! The other keymap variables, for \<MAP> and help--binding-locus.
        call register_keymap_name("escape_map", escape_map)
        call register_keymap_name("control_x_map", control_x_map)
        call register_keymap_name("control_x_4_map", control_x_4_map)
        call register_keymap_name("control_x_5_map", control_x_5_map)
        call register_keymap_name("directory_editor_mode_map", directory_editor_mode_map)
        call register_keymap_name("control_x_r_map", control_x_r_map)
        call register_keymap_name("abbreviation_map", abbreviation_map)
        call register_keymap_name("edit_abbreviations_mode_map", edit_abbreviations_mode_map)
        call register_keymap_name("keyboard_macro_keymap", keyboard_macro_keymap)
        call register_keymap_name("bookmark_map", bookmark_map)
        call register_keymap_name("bookmark_minibuffer_read_name_map", bookmark_minibuffer_read_name_map)
        call register_keymap_name("incremental_search_mode_map", incremental_search_mode_map)
        call register_keymap_name("text_mode_map", text_mode_map)
        call register_keymap_name("programming_mode_map", programming_mode_map)
        call register_keymap_name("special_mode_map", special_mode_map)
        call register_keymap_name("compilation_mode_map", compilation_mode_map)
        call register_keymap_name("command_interpreter_mode_map", command_interpreter_mode_map)
        call register_keymap_name("shell_mode_map", shell_mode_map)
        call register_keymap_name("mail_mode_map", mail_mode_map)
        call register_keymap_name("goto_map", goto_map)
        call register_keymap_name("narrow_map", narrow_map)
        call register_keymap_name("search_map", search_map)
        call register_keymap_name("minibuffer_local_completion_map", minibuffer_local_completion_map)
        call register_keymap_name("minibuffer_local_must_match_map", minibuffer_local_must_match_map)
        call register_keymap_name("minibuffer_local_filename_completion_map", minibuffer_local_filename_completion_map)
        call register_keymap_name("minibuffer_local_filename_must_match_map", minibuffer_local_filename_must_match_map)
        ! bindings.el: function-key-map, the parent of every terminal's
        ! local-function-key-map.
        call make_sparse_keymap(function_key_map)
        call translate(function_key_map, "<backspace>", value_type(127_int64))
        call translate(function_key_map, "<delete>", value_type(127_int64))
        call translate(function_key_map, "<kp-delete>", value_type(127_int64))

        if (.not. is_keymap_live(local_function_key_map)) then
            call make_sparse_keymap(local_function_key_map)
        end if

        call set_keymap_parent(local_function_key_map, function_key_map, status)
        ! paren.el, electric.el and elec-pair.el; Show Paren mode and
        ! Electric Indent mode are on (their :init-value, set after the
        ! files load at startup).
        call initialize_parenthesis()
        call initialize_which_function()
        call initialize_electric(global_map)
        call initialize_electric_pair()
        call minor_modes%show_parenthesis_mode%enable(status)
        call minor_modes%electric_indent_mode%enable(status)
    end subroutine initialize_bindings
end module bindings_mod
