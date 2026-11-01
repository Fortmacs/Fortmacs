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
    use sysdep_mod, only: sys_unsetenv
    use testdrive, only: run_testsuite, run_selected, new_testsuite, &
                         testsuite_type, select_suite
    use test_version_mod, only: collect_version_mod
    use test_help_mod, only: collect_help_mod
    use test_apropos_mod, only: collect_apropos_mod
    use test_simple_setvar_mod, only: collect_simple_setvar_mod
    use test_syntax_mod, only: collect_syntax_mod
    use test_syntax_scan_mod, only: collect_syntax_scan_mod
    use test_font_lock_mod, only: collect_font_lock_mod
    use test_paragraphs_mod, only: collect_paragraphs_mod
    use test_regex_emacs_mod, only: collect_regex_emacs_mod
    use test_casefiddle_mod, only: collect_casefiddle_mod
    use test_replace_mod, only: collect_replace_mod
    use test_rmc_mod, only: collect_rmc_mod
    use test_novice_mod, only: collect_novice_mod
    use test_fill_mod, only: collect_fill_mod
    use test_newcomment_mod, only: collect_newcomment_mod
    use test_conf_mode_mod, only: collect_conf_mode_mod
    use test_make_mode_mod, only: collect_make_mode_mod
    use test_python_mod, only: collect_python_mod
    use test_css_mode_mod, only: collect_css_mode_mod
    use test_js_mod, only: collect_js_mod
    use test_sgml_mode_mod, only: collect_sgml_mode_mod
    use test_charset_mod, only: collect_charset_mod
    use test_expression_motion_mod, only: collect_expression_motion_mod
    use test_page_mod, only: collect_page_mod
    use test_simple_goto_mod, only: collect_simple_goto_mod
    use test_syntax_propertize_mod, only: collect_syntax_propertize_mod
    use test_smie_mod, only: collect_smie_mod
    use test_sh_script_mod, only: collect_sh_script_mod
    use test_f90_mod, only: collect_f90_mod
    use test_error_mod, only: collect_error_mod
    use test_handle_mod, only: collect_handle_mod
    use test_character_mod, only: collect_character_mod
    use test_buffer_mod, only: collect_buffer_mod
    use test_insdel_mod, only: collect_insdel_mod
    use test_marker_mod, only: collect_marker_mod
    use test_sysdep_mod, only: collect_sysdep_mod
    use test_editfns_mod, only: collect_editfns_mod
    use test_fns_mod, only: collect_fns_mod
    use test_cmds_mod, only: collect_cmds_mod
    use test_values_mod, only: collect_values_mod
    use test_data_buffer_mod, only: collect_data_buffer_mod
    use test_derived_mod, only: collect_derived_mod
    use test_easy_mmode_mod, only: collect_easy_mmode_mod
    use test_textprop_mod, only: collect_textprop_mod
    use test_keymap_mod, only: collect_keymap_mod
    use test_keyboard_mod, only: collect_keyboard_mod
    use test_bindings_mod, only: collect_bindings_mod
    use test_term_xterm_mod, only: collect_term_xterm_mod
    use test_terminfo_mod, only: collect_terminfo_mod
    use test_term_mod, only: collect_term_mod
    use test_dispnew_mod, only: collect_dispnew_mod
    use test_xdisp_mod, only: collect_xdisp_mod
    use test_fake_gui_mod, only: collect_fake_gui_mod
    use test_fileio_mod, only: collect_fileio_mod
    use test_files_mod, only: collect_files_mod
    use test_minibuf_mod, only: collect_minibuf_mod
    use test_indent_mod, only: collect_indent_mod
    use test_emacs_mod, only: collect_emacs_mod
    use test_callproc_mod, only: collect_callproc_mod
    use test_process_mod, only: collect_process_mod
    use test_simple_shell_mod, only: collect_simple_shell_mod
    use test_compile_mod, only: collect_compile_mod
    use test_grep_mod, only: collect_grep_mod
    use test_ring_mod, only: collect_ring_mod
    use test_comint_mod, only: collect_comint_mod
    use test_shell_mod, only: collect_shell_mod
    use test_config_mod, only: collect_config_mod
    use test_dired_mod, only: collect_dired_mod
    use test_dired_mode_mod, only: collect_dired_mode_mod
    use test_dired_aux_mod, only: collect_dired_aux_mod
    use test_diff_mode_mod, only: collect_diff_mode_mod
    use test_view_mod, only: collect_view_mod
    use test_files_copy_mod, only: collect_files_copy_mod
    use test_casetab_mod, only: collect_casetab_mod
    use test_chartab_mod, only: collect_chartab_mod
    use test_category_mod, only: collect_category_mod
    use test_bidi_mod, only: collect_bidi_mod
    use test_search_mod, only: collect_search_mod
    use test_isearch_mod, only: collect_isearch_mod
    use test_window_mod, only: collect_window_mod
    use test_undo_mod, only: collect_undo_mod
    use test_simple_mod, only: collect_simple_mod
    use test_tty_colors_mod, only: collect_tty_colors_mod
    use test_faces_mod, only: collect_faces_mod
    use test_xfaces_mod, only: collect_xfaces_mod
    use test_rect_mod, only: collect_rect_mod
    use test_register_mod, only: collect_register_mod
    use test_kmacro_mod, only: collect_kmacro_mod
    use test_abbrev_mod, only: collect_abbrev_mod
    use test_dabbrev_mod, only: collect_dabbrev_mod
    use test_coding_mod, only: collect_coding_mod
    use test_minibuffer_mod, only: collect_minibuffer_mod
    use test_timer_mod, only: collect_timer_mod
    use test_itree_mod, only: collect_itree_mod
    use test_paren_mod, only: collect_paren_mod
    use test_electric_mod, only: collect_electric_mod
    use test_elec_pair_mod, only: collect_elec_pair_mod
    use test_simple_electric_mod, only: collect_simple_electric_mod
    use test_vc_mod, only: collect_vc_mod
    use test_log_edit_mod, only: collect_log_edit_mod
    use test_ewoc_mod, only: collect_ewoc_mod
    use test_inotify_mod, only: collect_inotify_mod
    use test_filenotify_mod, only: collect_filenotify_mod
    use test_autorevert_mod, only: collect_autorevert_mod
    use test_repeat_mod, only: collect_repeat_mod
    use test_namelist_mod, only: collect_namelist_mod
    use test_savehist_mod, only: collect_savehist_mod
    use test_saveplace_mod, only: collect_saveplace_mod
    use test_recentf_mod, only: collect_recentf_mod
    use test_bookmark_mod, only: collect_bookmark_mod
    use test_info_mod, only: collect_info_mod
    use test_info_look_mod, only: collect_info_look_mod
    use test_pcomplete_mod, only: collect_pcomplete_mod
    use test_ansi_color_mod, only: collect_ansi_color_mod
    use test_buff_menu_mod, only: collect_buff_menu_mod
    use test_ibuffer_mod, only: collect_ibuffer_mod
    use test_ibuf_ext_mod, only: collect_ibuf_ext_mod
    use test_time_date_mod, only: collect_time_date_mod
    use test_mule_util_mod, only: collect_mule_util_mod
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
    is = sys_unsetenv("LC_ALL")
    is = sys_unsetenv("LC_CTYPE")
    is = sys_unsetenv("LANG")
    testsuites = [ &
                 new_testsuite("version_mod", collect_version_mod), &
                 new_testsuite("error_mod", collect_error_mod), &
                 new_testsuite("handle_mod", collect_handle_mod), &
                 new_testsuite("character_mod", collect_character_mod), &
                 new_testsuite("buffer_mod", collect_buffer_mod), &
                 new_testsuite("insdel_mod", collect_insdel_mod), &
                 new_testsuite("marker_mod", collect_marker_mod), &
                 new_testsuite("sysdep_mod", collect_sysdep_mod), &
                 new_testsuite("editfns_mod", collect_editfns_mod), &
                 new_testsuite("fns_mod", collect_fns_mod), &
                 new_testsuite("cmds_mod", collect_cmds_mod), &
                 new_testsuite("values_mod", collect_values_mod), &
                 new_testsuite("data_buffer_mod", collect_data_buffer_mod), &
                 new_testsuite("derived_mod", collect_derived_mod), &
                 new_testsuite("easy_mmode_mod", collect_easy_mmode_mod), &
                 new_testsuite("textprop_mod", collect_textprop_mod), &
                 new_testsuite("keymap_mod", collect_keymap_mod), &
                 new_testsuite("keyboard_mod", collect_keyboard_mod), &
                 new_testsuite("bindings_mod", collect_bindings_mod), &
                 new_testsuite("term_xterm_mod", collect_term_xterm_mod), &
                 new_testsuite("terminfo_mod", collect_terminfo_mod), &
                 new_testsuite("term_mod", collect_term_mod), &
                 new_testsuite("dispnew_mod", collect_dispnew_mod), &
                 new_testsuite("xdisp_mod", collect_xdisp_mod), &
                 new_testsuite("fake_gui_mod", collect_fake_gui_mod), &
                 new_testsuite("fileio_mod", collect_fileio_mod), &
                 new_testsuite("files_mod", collect_files_mod), &
                 new_testsuite("minibuf_mod", collect_minibuf_mod), &
                 new_testsuite("indent_mod", collect_indent_mod), &
                 new_testsuite("emacs_mod", collect_emacs_mod), &
                 new_testsuite("callproc_mod", collect_callproc_mod), &
                 new_testsuite("process_mod", collect_process_mod), &
                 new_testsuite("simple_shell_mod", collect_simple_shell_mod), &
                 new_testsuite("compile_mod", collect_compile_mod), &
                 new_testsuite("grep_mod", collect_grep_mod), &
                 new_testsuite("ring_mod", collect_ring_mod), &
                 new_testsuite("comint_mod", collect_comint_mod), &
                 new_testsuite("shell_mod", collect_shell_mod), &
                 new_testsuite("config_mod", collect_config_mod), &
                 new_testsuite("dired_mod", collect_dired_mod), &
                 new_testsuite("dired_mode_mod", collect_dired_mode_mod), &
                 new_testsuite("dired_aux_mod", collect_dired_aux_mod), &
                 new_testsuite("diff_mode_mod", collect_diff_mode_mod), &
                 new_testsuite("view_mod", collect_view_mod), &
                 new_testsuite("files_copy_mod", collect_files_copy_mod), &
                 new_testsuite("casetab_mod", collect_casetab_mod), &
                 new_testsuite("chartab_mod", collect_chartab_mod), &
                 new_testsuite("category_mod", collect_category_mod), &
                 new_testsuite("bidi_mod", collect_bidi_mod), &
                 new_testsuite("search_mod", collect_search_mod), &
                 new_testsuite("isearch_mod", collect_isearch_mod), &
                 new_testsuite("window_mod", collect_window_mod), &
                 new_testsuite("undo_mod", collect_undo_mod), &
                 new_testsuite("simple_mod", collect_simple_mod), &
                 new_testsuite("help_mod", collect_help_mod), &
                 new_testsuite("apropos_mod", collect_apropos_mod), &
                 new_testsuite("simple_setvar_mod", collect_simple_setvar_mod), &
                 new_testsuite("syntax_mod", collect_syntax_mod), &
                 new_testsuite("syntax_scan_mod", collect_syntax_scan_mod), &
                 new_testsuite("paragraphs_mod", collect_paragraphs_mod), &
                 new_testsuite("regex_emacs_mod", collect_regex_emacs_mod), &
                 new_testsuite("casefiddle_mod", collect_casefiddle_mod), &
                 new_testsuite("replace_mod", collect_replace_mod), &
                 new_testsuite("rmc_mod", collect_rmc_mod), &
                 new_testsuite("novice_mod", collect_novice_mod), &
                 new_testsuite("fill_mod", collect_fill_mod), &
                 new_testsuite("newcomment_mod", collect_newcomment_mod), &
                 new_testsuite("conf_mode_mod", collect_conf_mode_mod), &
                 new_testsuite("make_mode_mod", collect_make_mode_mod), &
                 new_testsuite("python_mod", collect_python_mod), &
                 new_testsuite("css_mode_mod", collect_css_mode_mod), &
                 new_testsuite("js_mod", collect_js_mod), &
                 new_testsuite("sgml_mode_mod", collect_sgml_mode_mod), &
                 new_testsuite("charset_mod", collect_charset_mod), &
                 new_testsuite("expression_motion_mod", collect_expression_motion_mod), &
                 new_testsuite("page_mod", collect_page_mod), &
                 new_testsuite("simple_goto_mod", collect_simple_goto_mod), &
                 new_testsuite("syntax_propertize_mod", collect_syntax_propertize_mod), &
                 new_testsuite("smie_mod", collect_smie_mod), &
                 new_testsuite("sh_script_mod", collect_sh_script_mod), &
                 new_testsuite("f90_mod", collect_f90_mod), &
                 new_testsuite("tty_colors_mod", collect_tty_colors_mod), &
                 new_testsuite("faces_mod", collect_faces_mod), &
                 new_testsuite("xfaces_mod", collect_xfaces_mod), &
                 new_testsuite("font_lock_mod", collect_font_lock_mod), &
                 new_testsuite("rect_mod", collect_rect_mod), &
                 new_testsuite("register_mod", collect_register_mod), &
                 new_testsuite("kmacro_mod", collect_kmacro_mod), &
                 new_testsuite("abbrev_mod", collect_abbrev_mod), &
                 new_testsuite("dabbrev_mod", collect_dabbrev_mod), &
                 new_testsuite("coding_mod", collect_coding_mod), &
                 new_testsuite("minibuffer_mod", collect_minibuffer_mod), &
                 new_testsuite("timer_mod", collect_timer_mod), &
                 new_testsuite("itree_mod", collect_itree_mod), &
                 new_testsuite("paren_mod", collect_paren_mod), &
                 new_testsuite("electric_mod", collect_electric_mod), &
                 new_testsuite("elec_pair_mod", collect_elec_pair_mod), &
                 new_testsuite("simple_electric_mod", collect_simple_electric_mod), &
                 new_testsuite("vc_mod", collect_vc_mod), &
                 new_testsuite("log_edit_mod", collect_log_edit_mod), &
                 new_testsuite("ewoc_mod", collect_ewoc_mod), &
                 new_testsuite("inotify_mod", collect_inotify_mod), &
                 new_testsuite("filenotify_mod", collect_filenotify_mod), &
                 new_testsuite("autorevert_mod", collect_autorevert_mod), &
                 new_testsuite("repeat_mod", collect_repeat_mod), &
                 new_testsuite("namelist_mod", collect_namelist_mod), &
                 new_testsuite("savehist_mod", collect_savehist_mod), &
                 new_testsuite("saveplace_mod", collect_saveplace_mod), &
                 new_testsuite("recentf_mod", collect_recentf_mod), &
                 new_testsuite("bookmark_mod", collect_bookmark_mod), &
                 new_testsuite("buff_menu_mod", collect_buff_menu_mod), &
                 new_testsuite("ibuffer_mod", collect_ibuffer_mod), &
                 new_testsuite("ibuf_ext_mod", collect_ibuf_ext_mod), &
                 new_testsuite("time_date_mod", collect_time_date_mod), &
                 new_testsuite("mule_util_mod", collect_mule_util_mod), &
                 new_testsuite("tabulated_list_mod", collect_tabulated_list_mod), &
                 new_testsuite("info_mod", collect_info_mod), &
                 new_testsuite("info_look_mod", collect_info_look_mod), &
                 new_testsuite("ansi_color_mod", collect_ansi_color_mod), &
                 new_testsuite("pcomplete_mod", collect_pcomplete_mod) &
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
    subroutine get_argument(index, arg)
        integer, intent(in) :: index
        character(len=:), allocatable, intent(out) :: arg
        integer :: length, astat

        call get_command_argument(index, length=length, status=astat)
        if (astat /= 0 .or. length == 0) return
        allocate (character(len=length) :: arg)
        call get_command_argument(index, arg, status=astat)
        if (astat /= 0) deallocate (arg)
    end subroutine get_argument
end program tester
