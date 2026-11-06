! main.f90 -- test-drive runner for all Fortmacs test suites.
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

!> Runs every suite, or one suite (and optionally one test) by name:
!>   fpm test -- [suite [test]]
!> New suites are registered in the `testsuites` array below.
program tester
    use, intrinsic :: iso_fortran_env, only: error_unit
    use system_dependent_mod, only: system_unsetenv
    use testdrive, only: run_testsuite, run_selected, new_testsuite, &
                         testsuite_type, select_suite
    use test_version_mod, only: collect_version_mod
    use test_basic_subprograms_mod, only: collect_basic_subprograms_mod
    use test_help_mod, only: collect_help_mod
    use test_apropos_mod, only: collect_apropos_mod
    use test_simple_set_variable_mod, only: collect_simple_set_variable_mod
    use test_syntax_mod, only: collect_syntax_mod
    use test_syntax_scan_mod, only: collect_syntax_scan_mod
    use test_font_lock_mod, only: collect_font_lock_mod
    use test_paragraphs_mod, only: collect_paragraphs_mod
    use test_regular_expression_mod, only: collect_regular_expression_mod
    use test_casefiddle_mod, only: collect_casefiddle_mod
    use test_replace_mod, only: collect_replace_mod
    use test_read_multiple_choice_mod, only: collect_read_multiple_choice_mod
    use test_novice_mod, only: collect_novice_mod
    use test_fill_mod, only: collect_fill_mod
    use test_newcomment_mod, only: collect_newcomment_mod
    use test_configuration_mode_mod, only: collect_configuration_mode_mod
    use test_make_mode_mod, only: collect_make_mode_mod
    use test_python_mod, only: collect_python_mod
    use test_css_mode_mod, only: collect_css_mode_mod
    use test_javascript_mod, only: collect_javascript_mod
    use test_sgml_mode_mod, only: collect_sgml_mode_mod
    use test_skeleton_mod, only: collect_skeleton_mod
    use test_character_set_mod, only: collect_character_set_mod
    use test_expression_motion_mod, only: collect_expression_motion_mod
    use test_page_mod, only: collect_page_mod
    use test_button_mod, only: collect_button_mod
    use test_help_mode_mod, only: collect_help_mode_mod
    use test_simple_goto_mod, only: collect_simple_goto_mod
    use test_syntax_propertize_mod, only: collect_syntax_propertize_mod
    use test_simple_minded_indentation_engine_mod, only: collect_simple_minded_indentation_engine_mod
    use test_shell_script_mod, only: collect_shell_script_mod
    use test_f90_mod, only: collect_f90_mod
    use test_fortran_completion_mod, only: collect_fortran_completion_mod
    use test_error_mod, only: collect_error_mod
    use test_handle_mod, only: collect_handle_mod
    use test_string_type_mod, only: collect_string_type_mod
    use test_character_mod, only: collect_character_mod
    use test_buffer_mod, only: collect_buffer_mod
    use test_insert_delete_mod, only: collect_insert_delete_mod
    use test_marker_mod, only: collect_marker_mod
    use test_system_dependent_mod, only: collect_system_dependent_mod
    use test_editing_functions_mod, only: collect_editing_functions_mod
    use test_utility_functions_mod, only: collect_utility_functions_mod
    use test_md5_mod, only: collect_md5_mod
    use test_sha1_mod, only: collect_sha1_mod
    use test_sha256_mod, only: collect_sha256_mod
    use test_sha512_mod, only: collect_sha512_mod
    use test_sha3_mod, only: collect_sha3_mod
    use test_editing_commands_mod, only: collect_editing_commands_mod
    use test_values_mod, only: collect_values_mod
    use test_data_buffer_mod, only: collect_data_buffer_mod
    use test_derived_mod, only: collect_derived_mod
    use test_easy_minor_mode_mod, only: collect_easy_minor_mode_mod
    use test_text_properties_mod, only: collect_text_properties_mod
    use test_keymap_mod, only: collect_keymap_mod
    use test_keyboard_mod, only: collect_keyboard_mod
    use test_bindings_mod, only: collect_bindings_mod
    use test_terminal_xterm_mod, only: collect_terminal_xterm_mod
    use test_terminfo_mod, only: collect_terminfo_mod
    use test_terminal_output_mod, only: collect_terminal_output_mod
    use test_display_update_mod, only: collect_display_update_mod
    use test_display_engine_mod, only: collect_display_engine_mod
    use test_fake_graphical_user_interface_mod, only: collect_fake_graphical_user_interface_mod
    use test_file_input_output_mod, only: collect_file_input_output_mod
    use test_time_functions_mod, only: collect_time_functions_mod
    use test_files_mod, only: collect_files_mod
    use test_minibuffer_reading_mod, only: collect_minibuffer_reading_mod
    use test_indent_mod, only: collect_indent_mod
    use test_fortmacs_mod, only: collect_fortmacs_mod
    use test_execute_process_mod, only: collect_execute_process_mod
    use test_environment_mod, only: collect_environment_mod
    use test_process_mod, only: collect_process_mod
    use test_simple_shell_mod, only: collect_simple_shell_mod
    use test_compile_mod, only: collect_compile_mod
    use test_grep_mod, only: collect_grep_mod
    use test_cross_reference_mod, only: collect_cross_reference_mod
    use test_fortmacs_tags_mod, only: collect_fortmacs_tags_mod
    use test_ring_mod, only: collect_ring_mod
    use test_command_interpreter_mod, only: collect_command_interpreter_mod
    use test_shell_mod, only: collect_shell_mod
    use test_configuration_mod, only: collect_configuration_mod
    use test_directory_editor_mod, only: collect_directory_editor_mod
    use test_directory_editor_mode_mod, only: collect_directory_editor_mode_mod
    use test_directory_editor_auxiliary_mod, only: collect_directory_editor_auxiliary_mod
    use test_diff_mode_mod, only: collect_diff_mode_mod
    use test_fortmacs_diff_diff_mod, only: collect_fortmacs_diff_diff_mod
    use test_fortmacs_diff_utilities_mod, only: collect_fortmacs_diff_utilities_mod
    use test_fortmacs_diff_merging_mod, only: collect_fortmacs_diff_merging_mod
    use test_fortmacs_diff_multiple_mod, only: collect_fortmacs_diff_multiple_mod
    use test_simple_merge_mode_mod, only: collect_simple_merge_mode_mod
    use test_view_mod, only: collect_view_mod
    use test_files_copy_mod, only: collect_files_copy_mod
    use test_case_table_mod, only: collect_case_table_mod
    use test_character_table_mod, only: collect_character_table_mod
    use test_category_mod, only: collect_category_mod
    use test_bidirectional_mod, only: collect_bidirectional_mod
    use test_search_mod, only: collect_search_mod
    use test_incremental_search_mod, only: collect_incremental_search_mod
    use test_window_mod, only: collect_window_mod
    use test_window_sides_mod, only: collect_window_sides_mod
    use test_frame_mod, only: collect_frame_mod
    use test_undo_mod, only: collect_undo_mod
    use test_simple_mod, only: collect_simple_mod
    use test_terminal_colors_mod, only: collect_terminal_colors_mod
    use test_faces_mod, only: collect_faces_mod
    use test_face_realization_mod, only: collect_face_realization_mod
    use test_rectangle_mod, only: collect_rectangle_mod
    use test_register_mod, only: collect_register_mod
    use test_keyboard_macros_mod, only: collect_keyboard_macros_mod
    use test_abbreviation_mod, only: collect_abbreviation_mod
    use test_dynamic_abbreviation_mod, only: collect_dynamic_abbreviation_mod
    use test_spellcheck_mod, only: collect_spellcheck_mod
    use test_spellcheck_mode_mod, only: collect_spellcheck_mode_mod
    use test_coding_mod, only: collect_coding_mod
    use test_minibuffer_mod, only: collect_minibuffer_mod
    use test_timer_mod, only: collect_timer_mod
    use test_interval_tree_mod, only: collect_interval_tree_mod
    use test_parenthesis_mod, only: collect_parenthesis_mod
    use test_index_menu_mod, only: collect_index_menu_mod
    use test_which_function_mod, only: collect_which_function_mod
    use test_electric_mod, only: collect_electric_mod
    use test_electric_pair_mod, only: collect_electric_pair_mod
    use test_simple_electric_mod, only: collect_simple_electric_mod
    use test_version_control_mod, only: collect_version_control_mod
    use test_project_mod, only: collect_project_mod
    use test_version_control_mercurial_mod, only: collect_version_control_mercurial_mod
    use test_version_control_annotate_mod, only: collect_version_control_annotate_mod
    use test_version_control_subversion_mod, only: collect_version_control_subversion_mod
    use test_log_edit_mod, only: collect_log_edit_mod
    use test_add_log_mod, only: collect_add_log_mod
    use test_fortmacs_widget_for_object_collections_mod, only: collect_fortmacs_widget_for_object_collections_mod
    use test_inotify_mod, only: collect_inotify_mod
    use test_filenotify_mod, only: collect_filenotify_mod
    use test_autorevert_mod, only: collect_autorevert_mod
    use test_repeat_mod, only: collect_repeat_mod
    use test_follow_mod, only: collect_follow_mod
    use test_namelist_mod, only: collect_namelist_mod
    use test_save_history_mod, only: collect_save_history_mod
    use test_winner_mod, only: collect_winner_mod
    use test_tab_bar_mod, only: collect_tab_bar_mod
    use test_window_move_mod, only: collect_window_move_mod
    use test_saveplace_mod, only: collect_saveplace_mod
    use test_recent_files_mod, only: collect_recent_files_mod
    use test_bookmark_mod, only: collect_bookmark_mod
    use test_info_mod, only: collect_info_mod
    use test_info_lookup_mod, only: collect_info_lookup_mod
    use test_programmable_completion_mod, only: collect_programmable_completion_mod
    use test_mail_utilities_mod, only: collect_mail_utilities_mod
    use test_mail_extract_mod, only: collect_mail_extract_mod
    use test_mail_abbreviations_mod, only: collect_mail_abbreviations_mod
    use test_send_mail_mod, only: collect_send_mail_mod
    use test_message_mod, only: collect_message_mod
    use test_smtpmail_mod, only: collect_smtpmail_mod
    use test_movemail_mod, only: collect_movemail_mod
    use test_rmail_mod, only: collect_rmail_mod
    use test_rmail_keywords_mod, only: collect_rmail_keywords_mod
    use test_rmail_edit_mod, only: collect_rmail_edit_mod
    use test_rmail_output_mod, only: collect_rmail_output_mod
    use test_rmail_summary_mod, only: collect_rmail_summary_mod
    use test_ansi_color_mod, only: collect_ansi_color_mod
    use test_buffer_menu_mod, only: collect_buffer_menu_mod
    use test_calendar_mod, only: collect_calendar_mod
    use test_holidays_mod, only: collect_holidays_mod
    use test_ibuffer_mod, only: collect_ibuffer_mod
    use test_ibuffer_extensions_mod, only: collect_ibuffer_extensions_mod
    use test_time_date_mod, only: collect_time_date_mod
    use test_multilingual_environment_utilities_mod, only: collect_multilingual_environment_utilities_mod
    use test_tabulated_list_mod, only: collect_tabulated_list_mod
    implicit none(type, external)
    ! test-drive's interfaces take default integers.
    integer :: stat, is
    character(len=:), allocatable :: suite_name, test_name
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'

    stat = 0
    ! The editor started by the tests takes no language environment from
    ! the locale (startup.el:command-line's set-locale-environment).
    is = system_unsetenv("LC_ALL")
    is = system_unsetenv("LC_CTYPE")
    is = system_unsetenv("LANG")
    testsuites = [ &
                 new_testsuite("version_mod", collect_version_mod), &
                 new_testsuite("basic_subprograms_mod", collect_basic_subprograms_mod), &
                 new_testsuite("error_mod", collect_error_mod), &
                 new_testsuite("handle_mod", collect_handle_mod), &
                 new_testsuite("string_type_mod", collect_string_type_mod), &
                 new_testsuite("character_mod", collect_character_mod), &
                 new_testsuite("buffer_mod", collect_buffer_mod), &
                 new_testsuite("insert_delete_mod", collect_insert_delete_mod), &
                 new_testsuite("marker_mod", collect_marker_mod), &
                 new_testsuite("system_dependent_mod", collect_system_dependent_mod), &
                 new_testsuite("editing_functions_mod", collect_editing_functions_mod), &
                 new_testsuite("utility_functions_mod", collect_utility_functions_mod), &
                 new_testsuite("md5_mod", collect_md5_mod), &
                 new_testsuite("sha1_mod", collect_sha1_mod), &
                 new_testsuite("sha256_mod", collect_sha256_mod), &
                 new_testsuite("sha512_mod", collect_sha512_mod), &
                 new_testsuite("sha3_mod", collect_sha3_mod), &
                 new_testsuite("editing_commands_mod", collect_editing_commands_mod), &
                 new_testsuite("values_mod", collect_values_mod), &
                 new_testsuite("data_buffer_mod", collect_data_buffer_mod), &
                 new_testsuite("derived_mod", collect_derived_mod), &
                 new_testsuite("easy_minor_mode_mod", collect_easy_minor_mode_mod), &
                 new_testsuite("text_properties_mod", collect_text_properties_mod), &
                 new_testsuite("keymap_mod", collect_keymap_mod), &
                 new_testsuite("keyboard_mod", collect_keyboard_mod), &
                 new_testsuite("bindings_mod", collect_bindings_mod), &
                 new_testsuite("terminal_xterm_mod", collect_terminal_xterm_mod), &
                 new_testsuite("terminfo_mod", collect_terminfo_mod), &
                 new_testsuite("terminal_output_mod", collect_terminal_output_mod), &
                 new_testsuite("display_update_mod", collect_display_update_mod), &
                 new_testsuite("display_engine_mod", collect_display_engine_mod), &
                 new_testsuite("fake_graphical_user_interface_mod", collect_fake_graphical_user_interface_mod), &
                 new_testsuite("file_input_output_mod", collect_file_input_output_mod), &
                 new_testsuite("time_functions_mod", collect_time_functions_mod), &
                 new_testsuite("files_mod", collect_files_mod), &
                 new_testsuite("minibuffer_reading_mod", collect_minibuffer_reading_mod), &
                 new_testsuite("indent_mod", collect_indent_mod), &
                 new_testsuite("fortmacs_mod", collect_fortmacs_mod), &
                 new_testsuite("environment_mod", collect_environment_mod), &
                 new_testsuite("execute_process_mod", collect_execute_process_mod), &
                 new_testsuite("process_mod", collect_process_mod), &
                 new_testsuite("simple_shell_mod", collect_simple_shell_mod), &
                 new_testsuite("compile_mod", collect_compile_mod), &
                 new_testsuite("grep_mod", collect_grep_mod), &
                 new_testsuite("cross_reference_mod", collect_cross_reference_mod), &
                 new_testsuite("fortmacs_tags_mod", collect_fortmacs_tags_mod), &
                 new_testsuite("ring_mod", collect_ring_mod), &
                 new_testsuite("command_interpreter_mod", collect_command_interpreter_mod), &
                 new_testsuite("shell_mod", collect_shell_mod), &
                 new_testsuite("configuration_mod", collect_configuration_mod), &
                 new_testsuite("directory_editor_mod", collect_directory_editor_mod), &
                 new_testsuite("directory_editor_mode_mod", collect_directory_editor_mode_mod), &
                 new_testsuite("directory_editor_auxiliary_mod", collect_directory_editor_auxiliary_mod), &
                 new_testsuite("diff_mode_mod", collect_diff_mode_mod), &
                 new_testsuite("fortmacs_diff_diff_mod", collect_fortmacs_diff_diff_mod), &
                 new_testsuite("fortmacs_diff_utilities_mod", collect_fortmacs_diff_utilities_mod), &
                 new_testsuite("fortmacs_diff_merging_mod", collect_fortmacs_diff_merging_mod), &
                 new_testsuite("fortmacs_diff_multiple_mod", collect_fortmacs_diff_multiple_mod), &
                 new_testsuite("simple_merge_mode_mod", collect_simple_merge_mode_mod), &
                 new_testsuite("view_mod", collect_view_mod), &
                 new_testsuite("files_copy_mod", collect_files_copy_mod), &
                 new_testsuite("case_table_mod", collect_case_table_mod), &
                 new_testsuite("character_table_mod", collect_character_table_mod), &
                 new_testsuite("category_mod", collect_category_mod), &
                 new_testsuite("bidirectional_mod", collect_bidirectional_mod), &
                 new_testsuite("search_mod", collect_search_mod), &
                 new_testsuite("incremental_search_mod", collect_incremental_search_mod), &
                 new_testsuite("window_mod", collect_window_mod), &
                 new_testsuite("window_sides_mod", collect_window_sides_mod), &
                 new_testsuite("frame_mod", collect_frame_mod), &
                 new_testsuite("undo_mod", collect_undo_mod), &
                 new_testsuite("simple_mod", collect_simple_mod), &
                 new_testsuite("help_mod", collect_help_mod), &
                 new_testsuite("apropos_mod", collect_apropos_mod), &
                 new_testsuite("simple_set_variable_mod", collect_simple_set_variable_mod), &
                 new_testsuite("syntax_mod", collect_syntax_mod), &
                 new_testsuite("syntax_scan_mod", collect_syntax_scan_mod), &
                 new_testsuite("paragraphs_mod", collect_paragraphs_mod), &
                 new_testsuite("regular_expression_mod", collect_regular_expression_mod), &
                 new_testsuite("casefiddle_mod", collect_casefiddle_mod), &
                 new_testsuite("replace_mod", collect_replace_mod), &
                 new_testsuite("read_multiple_choice_mod", collect_read_multiple_choice_mod), &
                 new_testsuite("novice_mod", collect_novice_mod), &
                 new_testsuite("fill_mod", collect_fill_mod), &
                 new_testsuite("newcomment_mod", collect_newcomment_mod), &
                 new_testsuite("configuration_mode_mod", collect_configuration_mode_mod), &
                 new_testsuite("make_mode_mod", collect_make_mode_mod), &
                 new_testsuite("python_mod", collect_python_mod), &
                 new_testsuite("css_mode_mod", collect_css_mode_mod), &
                 new_testsuite("javascript_mod", collect_javascript_mod), &
                 new_testsuite("sgml_mode_mod", collect_sgml_mode_mod), &
                 new_testsuite("skeleton_mod", collect_skeleton_mod), &
                 new_testsuite("character_set_mod", collect_character_set_mod), &
                 new_testsuite("expression_motion_mod", collect_expression_motion_mod), &
                 new_testsuite("page_mod", collect_page_mod), &
                 new_testsuite("button_mod", collect_button_mod), &
                 new_testsuite("help_mode_mod", collect_help_mode_mod), &
                 new_testsuite("simple_goto_mod", collect_simple_goto_mod), &
                 new_testsuite("syntax_propertize_mod", collect_syntax_propertize_mod), &
                 new_testsuite("simple_minded_indentation_engine_mod", collect_simple_minded_indentation_engine_mod), &
                 new_testsuite("shell_script_mod", collect_shell_script_mod), &
                 new_testsuite("f90_mod", collect_f90_mod), &
                 new_testsuite("fortran_completion_mod", collect_fortran_completion_mod), &
                 new_testsuite("terminal_colors_mod", collect_terminal_colors_mod), &
                 new_testsuite("faces_mod", collect_faces_mod), &
                 new_testsuite("face_realization_mod", collect_face_realization_mod), &
                 new_testsuite("font_lock_mod", collect_font_lock_mod), &
                 new_testsuite("rectangle_mod", collect_rectangle_mod), &
                 new_testsuite("register_mod", collect_register_mod), &
                 new_testsuite("keyboard_macros_mod", collect_keyboard_macros_mod), &
                 new_testsuite("abbreviation_mod", collect_abbreviation_mod), &
                 new_testsuite("dynamic_abbreviation_mod", collect_dynamic_abbreviation_mod), &
                 new_testsuite("spellcheck_mod", collect_spellcheck_mod), &
                 new_testsuite("spellcheck_mode_mod", collect_spellcheck_mode_mod), &
                 new_testsuite("coding_mod", collect_coding_mod), &
                 new_testsuite("minibuffer_mod", collect_minibuffer_mod), &
                 new_testsuite("timer_mod", collect_timer_mod), &
                 new_testsuite("interval_tree_mod", collect_interval_tree_mod), &
                 new_testsuite("parenthesis_mod", collect_parenthesis_mod), &
                 new_testsuite("index_menu_mod", collect_index_menu_mod), &
                 new_testsuite("which_function_mod", collect_which_function_mod), &
                 new_testsuite("electric_mod", collect_electric_mod), &
                 new_testsuite("electric_pair_mod", collect_electric_pair_mod), &
                 new_testsuite("simple_electric_mod", collect_simple_electric_mod), &
                 new_testsuite("version_control_mod", collect_version_control_mod), &
                 new_testsuite("project_mod", collect_project_mod), &
                 new_testsuite("version_control_mercurial_mod", collect_version_control_mercurial_mod), &
                 new_testsuite("version_control_annotate_mod", collect_version_control_annotate_mod), &
                 new_testsuite("version_control_subversion_mod", collect_version_control_subversion_mod), &
                 new_testsuite("log_edit_mod", collect_log_edit_mod), &
                 new_testsuite("add_log_mod", collect_add_log_mod), &
                 new_testsuite("fortmacs_widget_for_object_collections_mod", collect_fortmacs_widget_for_object_collections_mod), &
                 new_testsuite("inotify_mod", collect_inotify_mod), &
                 new_testsuite("filenotify_mod", collect_filenotify_mod), &
                 new_testsuite("autorevert_mod", collect_autorevert_mod), &
                 new_testsuite("repeat_mod", collect_repeat_mod), &
                 new_testsuite("follow_mod", collect_follow_mod), &
                 new_testsuite("namelist_mod", collect_namelist_mod), &
                 new_testsuite("save_history_mod", collect_save_history_mod), &
                 new_testsuite("winner_mod", collect_winner_mod), &
                 new_testsuite("tab_bar_mod", collect_tab_bar_mod), &
                 new_testsuite("window_move_mod", collect_window_move_mod), &
                 new_testsuite("saveplace_mod", collect_saveplace_mod), &
                 new_testsuite("recent_files_mod", collect_recent_files_mod), &
                 new_testsuite("bookmark_mod", collect_bookmark_mod), &
                 new_testsuite("buffer_menu_mod", collect_buffer_menu_mod), &
                 new_testsuite("calendar_mod", collect_calendar_mod), &
                 new_testsuite("holidays_mod", collect_holidays_mod), &
                 new_testsuite("ibuffer_mod", collect_ibuffer_mod), &
                 new_testsuite("ibuffer_extensions_mod", collect_ibuffer_extensions_mod), &
                 new_testsuite("time_date_mod", collect_time_date_mod), &
                 new_testsuite("multilingual_environment_utilities_mod", collect_multilingual_environment_utilities_mod), &
                 new_testsuite("tabulated_list_mod", collect_tabulated_list_mod), &
                 new_testsuite("info_mod", collect_info_mod), &
                 new_testsuite("info_lookup_mod", collect_info_lookup_mod), &
                 new_testsuite("ansi_color_mod", collect_ansi_color_mod), &
                 new_testsuite("programmable_completion_mod", collect_programmable_completion_mod), &
                 new_testsuite("mail_utilities_mod", collect_mail_utilities_mod), &
                 new_testsuite("mail_extract_mod", collect_mail_extract_mod), &
                 new_testsuite("mail_abbreviations_mod", collect_mail_abbreviations_mod), &
                 new_testsuite("send_mail_mod", collect_send_mail_mod), &
                 new_testsuite("message_mod", collect_message_mod), &
                 new_testsuite("smtpmail_mod", collect_smtpmail_mod), &
                 new_testsuite("movemail_mod", collect_movemail_mod), &
                 new_testsuite("rmail_mod", collect_rmail_mod), &
                 new_testsuite("rmail_keywords_mod", collect_rmail_keywords_mod), &
                 new_testsuite("rmail_edit_mod", collect_rmail_edit_mod), &
                 new_testsuite("rmail_output_mod", collect_rmail_output_mod), &
                 new_testsuite("rmail_summary_mod", collect_rmail_summary_mod) &
                 ]
    call get_argument(1, suite_name)
    call get_argument(2, test_name)

    if (allocated(suite_name)) then
        is = select_suite(testsuites, suite_name)

        if (is > 0 .and. is <= size(testsuites)) then
            if (allocated(test_name)) then
                write (error_unit, fmt) "Suite:", testsuites(is)%name
                call run_selected(testsuites(is)%collect, test_name, &
                                  error_unit, stat)
                if (stat < 0) error stop 1
            else
                write (error_unit, fmt) "Testing:", testsuites(is)%name
                call run_testsuite(testsuites(is)%collect, error_unit, stat)
            end if
        else
            write (error_unit, fmt) "Available testsuites"

            do is = 1, size(testsuites)
                write (error_unit, fmt) "-", testsuites(is)%name
            end do

            error stop 1
        end if
    else
        do is = 1, size(testsuites)
            write (error_unit, fmt) "Testing:", testsuites(is)%name
            call run_testsuite(testsuites(is)%collect, error_unit, stat)
        end do
    end if

    ! The program's variables live on MAIN__'s stack, so what they hold
    ! is lost, not reachable, when it returns.
    if (allocated(suite_name)) deallocate (suite_name)
    if (allocated(test_name)) deallocate (test_name)

    if (stat > 0) then
        write (error_unit, "(i0, 1x, a)") stat, "test(s) failed!"
        error stop 1
    end if

contains

    !> Fetch command-line argument `index`, leaving `arg` unallocated if absent.
    subroutine get_argument(index, argument)
        integer, intent(in) :: index
        character(len=:), allocatable, intent(out) :: argument
        integer :: length, astat

        call get_command_argument(index, length=length, status=astat)
        if (astat /= 0 .or. length == 0) return
        allocate (character(len=length) :: argument)
        call get_command_argument(index, argument, status=astat)
        if (astat /= 0) deallocate (argument)
    end subroutine get_argument
end program tester
