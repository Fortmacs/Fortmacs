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
    use sysdep_fm, only: sys_unsetenv
    use testdrive, only: run_testsuite, run_selected, new_testsuite, &
                         testsuite_type, select_suite
    use test_version_fm, only: collect_version_fm
    use test_help_fm, only: collect_help_fm
    use test_apropos_fm, only: collect_apropos_fm
    use test_simple_setvar_fm, only: collect_simple_setvar_fm
    use test_syntax_fm, only: collect_syntax_fm
    use test_syntax_scan_fm, only: collect_syntax_scan_fm
    use test_font_lock_fm, only: collect_font_lock_fm
    use test_paragraphs_fm, only: collect_paragraphs_fm
    use test_regex_emacs_fm, only: collect_regex_emacs_fm
    use test_casefiddle_fm, only: collect_casefiddle_fm
    use test_replace_fm, only: collect_replace_fm
    use test_rmc_fm, only: collect_rmc_fm
    use test_novice_fm, only: collect_novice_fm
    use test_fill_fm, only: collect_fill_fm
    use test_newcomment_fm, only: collect_newcomment_fm
    use test_conf_mode_fm, only: collect_conf_mode_fm
    use test_make_mode_fm, only: collect_make_mode_fm
    use test_python_fm, only: collect_python_fm
    use test_css_mode_fm, only: collect_css_mode_fm
    use test_js_fm, only: collect_js_fm
    use test_sgml_mode_fm, only: collect_sgml_mode_fm
    use test_charset_fm, only: collect_charset_fm
    use test_lisp_cmds_fm, only: collect_lisp_cmds_fm
    use test_page_fm, only: collect_page_fm
    use test_simple_goto_fm, only: collect_simple_goto_fm
    use test_syntax_propertize_fm, only: collect_syntax_propertize_fm
    use test_smie_fm, only: collect_smie_fm
    use test_sh_script_fm, only: collect_sh_script_fm
    use test_f90_fm, only: collect_f90_fm
    use test_error_fm, only: collect_error_fm
    use test_handle_fm, only: collect_handle_fm
    use test_character_fm, only: collect_character_fm
    use test_buffer_fm, only: collect_buffer_fm
    use test_insdel_fm, only: collect_insdel_fm
    use test_marker_fm, only: collect_marker_fm
    use test_sysdep_fm, only: collect_sysdep_fm
    use test_editfns_fm, only: collect_editfns_fm
    use test_fns_fm, only: collect_fns_fm
    use test_cmds_fm, only: collect_cmds_fm
    use test_values_fm, only: collect_values_fm
    use test_data_buffer_fm, only: collect_data_buffer_fm
    use test_derived_fm, only: collect_derived_fm
    use test_easy_mmode_fm, only: collect_easy_mmode_fm
    use test_textprop_fm, only: collect_textprop_fm
    use test_keymap_fm, only: collect_keymap_fm
    use test_keyboard_fm, only: collect_keyboard_fm
    use test_bindings_fm, only: collect_bindings_fm
    use test_term_xterm_fm, only: collect_term_xterm_fm
    use test_terminfo_fm, only: collect_terminfo_fm
    use test_term_fm, only: collect_term_fm
    use test_dispnew_fm, only: collect_dispnew_fm
    use test_xdisp_fm, only: collect_xdisp_fm
    use test_fake_gui_fm, only: collect_fake_gui_fm
    use test_fileio_fm, only: collect_fileio_fm
    use test_files_fm, only: collect_files_fm
    use test_minibuf_fm, only: collect_minibuf_fm
    use test_indent_fm, only: collect_indent_fm
    use test_emacs_fm, only: collect_emacs_fm
    use test_callproc_fm, only: collect_callproc_fm
    use test_process_fm, only: collect_process_fm
    use test_simple_shell_fm, only: collect_simple_shell_fm
    use test_compile_fm, only: collect_compile_fm
    use test_grep_fm, only: collect_grep_fm
    use test_ring_fm, only: collect_ring_fm
    use test_comint_fm, only: collect_comint_fm
    use test_shell_fm, only: collect_shell_fm
    use test_config_fm, only: collect_config_fm
    use test_dired_fm, only: collect_dired_fm
    use test_dired_mode_fm, only: collect_dired_mode_fm
    use test_dired_aux_fm, only: collect_dired_aux_fm
    use test_diff_mode_fm, only: collect_diff_mode_fm
    use test_view_fm, only: collect_view_fm
    use test_files_copy_fm, only: collect_files_copy_fm
    use test_casetab_fm, only: collect_casetab_fm
    use test_search_fm, only: collect_search_fm
    use test_isearch_fm, only: collect_isearch_fm
    use test_window_fm, only: collect_window_fm
    use test_undo_fm, only: collect_undo_fm
    use test_simple_fm, only: collect_simple_fm
    use test_tty_colors_fm, only: collect_tty_colors_fm
    use test_faces_fm, only: collect_faces_fm
    use test_xfaces_fm, only: collect_xfaces_fm
    use test_rect_fm, only: collect_rect_fm
    use test_register_fm, only: collect_register_fm
    use test_kmacro_fm, only: collect_kmacro_fm
    use test_abbrev_fm, only: collect_abbrev_fm
    use test_dabbrev_fm, only: collect_dabbrev_fm
    use test_coding_fm, only: collect_coding_fm
    use test_quail_fm, only: collect_quail_fm
    use test_minibuffer_fm, only: collect_minibuffer_fm
    use test_timer_fm, only: collect_timer_fm
    use test_itree_fm, only: collect_itree_fm
    use test_paren_fm, only: collect_paren_fm
    use test_electric_fm, only: collect_electric_fm
    use test_elec_pair_fm, only: collect_elec_pair_fm
    use test_simple_electric_fm, only: collect_simple_electric_fm
    use test_vc_fm, only: collect_vc_fm
    use test_log_edit_fm, only: collect_log_edit_fm
    use test_ewoc_fm, only: collect_ewoc_fm
    use test_inotify_fm, only: collect_inotify_fm
    use test_filenotify_fm, only: collect_filenotify_fm
    use test_autorevert_fm, only: collect_autorevert_fm
    use test_repeat_fm, only: collect_repeat_fm
    use test_namelist_fm, only: collect_namelist_fm
    use test_savehist_fm, only: collect_savehist_fm
    use test_saveplace_fm, only: collect_saveplace_fm
    use test_recentf_fm, only: collect_recentf_fm
    use test_bookmark_fm, only: collect_bookmark_fm
    use test_info_fm, only: collect_info_fm
    use test_info_look_fm, only: collect_info_look_fm
    use test_pcomplete_fm, only: collect_pcomplete_fm
    use test_ansi_color_fm, only: collect_ansi_color_fm
    use test_buff_menu_fm, only: collect_buff_menu_fm
    use test_ibuffer_fm, only: collect_ibuffer_fm
    use test_ibuf_ext_fm, only: collect_ibuf_ext_fm
    use test_time_date_fm, only: collect_time_date_fm
    use test_mule_util_fm, only: collect_mule_util_fm
    use test_tabulated_list_fm, only: collect_tabulated_list_fm
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
                 new_testsuite("version_fm", collect_version_fm), &
                 new_testsuite("error_fm", collect_error_fm), &
                 new_testsuite("handle_fm", collect_handle_fm), &
                 new_testsuite("character_fm", collect_character_fm), &
                 new_testsuite("buffer_fm", collect_buffer_fm), &
                 new_testsuite("insdel_fm", collect_insdel_fm), &
                 new_testsuite("marker_fm", collect_marker_fm), &
                 new_testsuite("sysdep_fm", collect_sysdep_fm), &
                 new_testsuite("editfns_fm", collect_editfns_fm), &
                 new_testsuite("fns_fm", collect_fns_fm), &
                 new_testsuite("cmds_fm", collect_cmds_fm), &
                 new_testsuite("values_fm", collect_values_fm), &
                 new_testsuite("data_buffer_fm", collect_data_buffer_fm), &
                 new_testsuite("derived_fm", collect_derived_fm), &
                 new_testsuite("easy_mmode_fm", collect_easy_mmode_fm), &
                 new_testsuite("textprop_fm", collect_textprop_fm), &
                 new_testsuite("keymap_fm", collect_keymap_fm), &
                 new_testsuite("keyboard_fm", collect_keyboard_fm), &
                 new_testsuite("bindings_fm", collect_bindings_fm), &
                 new_testsuite("term_xterm_fm", collect_term_xterm_fm), &
                 new_testsuite("terminfo_fm", collect_terminfo_fm), &
                 new_testsuite("term_fm", collect_term_fm), &
                 new_testsuite("dispnew_fm", collect_dispnew_fm), &
                 new_testsuite("xdisp_fm", collect_xdisp_fm), &
                 new_testsuite("fake_gui_fm", collect_fake_gui_fm), &
                 new_testsuite("fileio_fm", collect_fileio_fm), &
                 new_testsuite("files_fm", collect_files_fm), &
                 new_testsuite("minibuf_fm", collect_minibuf_fm), &
                 new_testsuite("indent_fm", collect_indent_fm), &
                 new_testsuite("emacs_fm", collect_emacs_fm), &
                 new_testsuite("callproc_fm", collect_callproc_fm), &
                 new_testsuite("process_fm", collect_process_fm), &
                 new_testsuite("simple_shell_fm", collect_simple_shell_fm), &
                 new_testsuite("compile_fm", collect_compile_fm), &
                 new_testsuite("grep_fm", collect_grep_fm), &
                 new_testsuite("ring_fm", collect_ring_fm), &
                 new_testsuite("comint_fm", collect_comint_fm), &
                 new_testsuite("shell_fm", collect_shell_fm), &
                 new_testsuite("config_fm", collect_config_fm), &
                 new_testsuite("dired_fm", collect_dired_fm), &
                 new_testsuite("dired_mode_fm", collect_dired_mode_fm), &
                 new_testsuite("dired_aux_fm", collect_dired_aux_fm), &
                 new_testsuite("diff_mode_fm", collect_diff_mode_fm), &
                 new_testsuite("view_fm", collect_view_fm), &
                 new_testsuite("files_copy_fm", collect_files_copy_fm), &
                 new_testsuite("casetab_fm", collect_casetab_fm), &
                 new_testsuite("search_fm", collect_search_fm), &
                 new_testsuite("isearch_fm", collect_isearch_fm), &
                 new_testsuite("window_fm", collect_window_fm), &
                 new_testsuite("undo_fm", collect_undo_fm), &
                 new_testsuite("simple_fm", collect_simple_fm), &
                 new_testsuite("help_fm", collect_help_fm), &
                 new_testsuite("apropos_fm", collect_apropos_fm), &
                 new_testsuite("simple_setvar_fm", collect_simple_setvar_fm), &
                 new_testsuite("syntax_fm", collect_syntax_fm), &
                 new_testsuite("syntax_scan_fm", collect_syntax_scan_fm), &
                 new_testsuite("paragraphs_fm", collect_paragraphs_fm), &
                 new_testsuite("regex_emacs_fm", collect_regex_emacs_fm), &
                 new_testsuite("casefiddle_fm", collect_casefiddle_fm), &
                 new_testsuite("replace_fm", collect_replace_fm), &
                 new_testsuite("rmc_fm", collect_rmc_fm), &
                 new_testsuite("novice_fm", collect_novice_fm), &
                 new_testsuite("fill_fm", collect_fill_fm), &
                 new_testsuite("newcomment_fm", collect_newcomment_fm), &
                 new_testsuite("conf_mode_fm", collect_conf_mode_fm), &
                 new_testsuite("make_mode_fm", collect_make_mode_fm), &
                 new_testsuite("python_fm", collect_python_fm), &
                 new_testsuite("css_mode_fm", collect_css_mode_fm), &
                 new_testsuite("js_fm", collect_js_fm), &
                 new_testsuite("sgml_mode_fm", collect_sgml_mode_fm), &
                 new_testsuite("charset_fm", collect_charset_fm), &
                 new_testsuite("lisp_cmds_fm", collect_lisp_cmds_fm), &
                 new_testsuite("page_fm", collect_page_fm), &
                 new_testsuite("simple_goto_fm", collect_simple_goto_fm), &
                 new_testsuite("syntax_propertize_fm", collect_syntax_propertize_fm), &
                 new_testsuite("smie_fm", collect_smie_fm), &
                 new_testsuite("sh_script_fm", collect_sh_script_fm), &
                 new_testsuite("f90_fm", collect_f90_fm), &
                 new_testsuite("tty_colors_fm", collect_tty_colors_fm), &
                 new_testsuite("faces_fm", collect_faces_fm), &
                 new_testsuite("xfaces_fm", collect_xfaces_fm), &
                 new_testsuite("font_lock_fm", collect_font_lock_fm), &
                 new_testsuite("rect_fm", collect_rect_fm), &
                 new_testsuite("register_fm", collect_register_fm), &
                 new_testsuite("kmacro_fm", collect_kmacro_fm), &
                 new_testsuite("abbrev_fm", collect_abbrev_fm), &
                 new_testsuite("dabbrev_fm", collect_dabbrev_fm), &
                 new_testsuite("coding_fm", collect_coding_fm), &
                 new_testsuite("quail_fm", collect_quail_fm), &
                 new_testsuite("minibuffer_fm", collect_minibuffer_fm), &
                 new_testsuite("timer_fm", collect_timer_fm), &
                 new_testsuite("itree_fm", collect_itree_fm), &
                 new_testsuite("paren_fm", collect_paren_fm), &
                 new_testsuite("electric_fm", collect_electric_fm), &
                 new_testsuite("elec_pair_fm", collect_elec_pair_fm), &
                 new_testsuite("simple_electric_fm", collect_simple_electric_fm), &
                 new_testsuite("vc_fm", collect_vc_fm), &
                 new_testsuite("log_edit_fm", collect_log_edit_fm), &
                 new_testsuite("ewoc_fm", collect_ewoc_fm), &
                 new_testsuite("inotify_fm", collect_inotify_fm), &
                 new_testsuite("filenotify_fm", collect_filenotify_fm), &
                 new_testsuite("autorevert_fm", collect_autorevert_fm), &
                 new_testsuite("repeat_fm", collect_repeat_fm), &
                 new_testsuite("namelist_fm", collect_namelist_fm), &
                 new_testsuite("savehist_fm", collect_savehist_fm), &
                 new_testsuite("saveplace_fm", collect_saveplace_fm), &
                 new_testsuite("recentf_fm", collect_recentf_fm), &
                 new_testsuite("bookmark_fm", collect_bookmark_fm), &
                 new_testsuite("buff_menu_fm", collect_buff_menu_fm), &
                 new_testsuite("ibuffer_fm", collect_ibuffer_fm), &
                 new_testsuite("ibuf_ext_fm", collect_ibuf_ext_fm), &
                 new_testsuite("time_date_fm", collect_time_date_fm), &
                 new_testsuite("mule_util_fm", collect_mule_util_fm), &
                 new_testsuite("tabulated_list_fm", collect_tabulated_list_fm), &
                 new_testsuite("info_fm", collect_info_fm), &
                 new_testsuite("info_look_fm", collect_info_look_fm), &
                 new_testsuite("ansi_color_fm", collect_ansi_color_fm), &
                 new_testsuite("pcomplete_fm", collect_pcomplete_fm) &
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

    !> Fetch command-line argument `idx`, leaving `arg` unallocated if absent.
    subroutine get_argument(idx, arg)
        integer, intent(in) :: idx
        character(len=:), allocatable, intent(out) :: arg
        integer :: length, astat

        call get_command_argument(idx, length=length, status=astat)
        if (astat /= 0 .or. length == 0) return
        allocate (character(len=length) :: arg)
        call get_command_argument(idx, arg, status=astat)
        if (astat /= 0) deallocate (arg)
    end subroutine get_argument
end program tester
