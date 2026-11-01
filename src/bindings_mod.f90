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

!> `init_bindings' registers the ported commands and builds global-map,
!> esc-map, ctl-x-map and function-key-map with the bindings upstream
!> gives them, as far as their commands exist.  Bindings to commands not
!> ported yet are left out rather than failing with `void_function'.
module bindings_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use callint_mod, only: PREFIX_NONE
    use commands_mod, only: commands
    use error_mod, only: status_type, set_error, FM_GENERIC_ERROR
    use handle_mod, only: handle_type
    use value_mod, only: value_type, make_keymap_ref, list
    use character_mod, only: MAX_CHAR, CHAR_CTL, CHAR_META
    use keymap_mod, only: make_keymap, make_sparse_keymap, define_key, &
                          define_key_range, use_global_map, set_keymap_parent, &
                          key_parse, is_keymap_live, destroy_keymap, set_repeat_map
    use callint_mod, only: current_prefix_arg, prefix_numeric_value, intern_command_names
    use page_mod, only: init_page
    use keyboard_mod, only: local_function_key_map, init_macros_commands, help_events
    use kmacro_mod, only: init_kmacro, kmacro_keymap
    use cmds_mod, only: init_cmds, forward_char, backward_char
    use dispnew_mod, only: init_display_commands
    use simple_mod, only: init_simple, backward_word, region_bounds, is_region_noncontiguous
    use syntax_mod, only: init_syntax, forward_word
    use syntax_propertize_mod, only: init_syntax_propertize
    use smie_mod, only: init_smie
    use executable_mod, only: init_executable
    use sh_script_mod, only: init_sh_script
    use paragraphs_mod, only: init_paragraphs
    use expression_motion_mod, only: init_expression_motion
    use search_mod, only: init_search, begin_running_asynch_code, end_running_asynch_code
    use replace_mod, only: init_replace, init_occur
    use casefiddle_mod, only: init_casefiddle, casefiddle_region_bounds => region_bounds, &
                              casefiddle_is_region_noncontiguous => is_region_noncontiguous
    use novice_mod, only: init_novice
    use indent_mod, only: init_indent
    use fill_mod, only: init_fill
    use isearch_mod, only: init_isearch
    use window_mod, only: init_window_commands
    use buffer_mod, only: init_buffer_commands
    use files_mod, only: init_files
    use vc_hooks_mod, only: init_vc_hooks
    use vc_mod, only: init_vc
    use vc_git_mod, only: init_vc_git
    use log_view_mod, only: init_log_view
    use vc_dir_mod, only: init_vc_dir
    use autorevert_mod, only: init_autorevert
    use repeat_mod, only: init_repeat
    use help_mod, only: init_help, register_keymap_name, help_map
    use mule_mod, only: init_mule
    use quail_mod, only: init_quail
    use leim_list_mod, only: init_leim_list
    use isearch_mod, only: isearch_mode_map
    use text_mode_mod, only: text_mode_map
    use prog_mode_mod, only: prog_mode_map
    use simple_mod, only: special_mode_map
    use minibuffer_mod, only: minibuffer_local_completion_map, minibuffer_local_must_match_map, &
                              minibuffer_local_filename_completion_map, minibuffer_local_filename_must_match_map
    use help_fns_mod, only: init_help_fns
    use apropos_mod, only: init_apropos
    use minibuf_mod, only: init_minibuf, minibuffer_local_map
    use minibuffer_mod, only: init_minibuffer, completion_at_point
    use subr_mod, only: init_subr
    use text_mode_mod, only: init_text_mode
    use conf_mode_mod, only: init_conf_mode
    use make_mode_mod, only: init_make_mode
    use python_mod, only: init_python
    use css_mode_mod, only: init_css_mode
    use js_mod, only: init_js
    use sgml_mode_mod, only: init_sgml_mode
    use f90_mod, only: init_f90
    use prog_mode_mod, only: init_prog_mode
    use font_core_mod, only: init_font_core
    use font_lock_mod, only: init_font_lock
    use jit_lock_mod, only: init_jit_lock
    use dired_mode_mod, only: init_dired_mode, dired_mode_map
    use view_mod, only: init_view
    use process_mod, only: init_process, begin_asynch_code, end_asynch_code
    use compile_mod, only: init_compile, compilation_mode_map
    use grep_mod, only: init_grep
    use diff_mode_mod, only: init_diff_mode
    use diff_mod, only: init_diff
    use comint_mod, only: init_comint, comint_mode_map
    use pcomplete_mod, only: init_pcomplete
    use auth_source_mod, only: init_auth_source
    use ansi_color_mod, only: init_ansi_color
    use shell_mod, only: init_shell, shell_mode_map
    use rect_mod, only: init_rect
    use savehist_mod, only: init_savehist
    use saveplace_mod, only: init_saveplace
    use recentf_mod, only: init_recentf
    use tabulated_list_mod, only: init_tabulated_list
    use simple_mod, only: init_simple_process
    use bookmark_mod, only: init_bookmark, bookmark_map, bookmark_minibuffer_read_name_map
    use info_mod, only: init_info
    use info_look_mod, only: init_info_look
    use buff_menu_mod, only: init_buff_menu
    use ibuffer_mod, only: init_ibuffer
    use crm_mod, only: init_crm
    use register_mod, only: init_register
    use abbrev_mod, only: init_abbrev, edit_abbrevs_mode_map
    use dabbrev_mod, only: init_dabbrev
    use paren_mod, only: init_paren
    use electric_mod, only: init_electric
    use elec_pair_mod, only: init_elec_pair
    use newcomment_mod, only: init_newcomment
    use modes_mod, only: minor_modes
    implicit none(type, external)
    private

    public :: init_bindings

    integer(int64), parameter :: M_CTL = int(CHAR_CTL, int64), &
                                 M_META = int(CHAR_META, int64)

    !> subr.el:global-map, esc-map, ctl-x-map; keymap.c:Vfunction_key_map;
    !> bindings.el:ctl-x-x-map (keymap for subcommands of `C-x x');
    !> bindings.el:ctl-x-r-map (keymap for subcommands of `C-x r');
    !> subr.el:ctl-x-4-map (keymap for subcommands of `C-x 4').
    type(handle_type), public :: global_map, esc_map, ctl_x_map, &
                                 function_key_map, ctl_x_x_map, ctl_x_r_map, ctl_x_4_map
    !> bindings.el:goto-map, keymap for navigation commands (M-g).
    type(handle_type), public :: goto_map
    !> bindings.el:narrow-map, keymap for narrowing commands (C-x n).
    type(handle_type), public :: narrow_map
    !> bindings.el:search-map, keymap for search related commands (M-s).
    type(handle_type), public :: search_map

    !> bindings.el:abbrev-map (keymap for abbrev commands).
    type(handle_type), public :: abbrev_map

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
    subroutine cmd_complete_name(status)
        type(status_type), intent(out) :: status

        if (current_prefix_arg%kind /= PREFIX_NONE) then
            call set_error(status, FM_GENERIC_ERROR, "info_complete_name is not ported yet")
            return
        end if

        call completion_at_point(status)
    end subroutine cmd_complete_name

    !> Move point N characters to the right (to the left if N is negative).
    !> On reaching beginning or end of buffer, stop, and an error condition occurs.
    !>
    !> In left-to-right text this moves forward in the buffer, like
    !> \[forward_char].  Port of bindings.el:right-char (left-to-right
    !> paragraphs only).
    subroutine cmd_right_char(status)
        type(status_type), intent(out) :: status

        call forward_char(status, prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_right_char

    !> Move point N characters to the left (to the right if N is negative).
    !> On reaching beginning or end of buffer, stop, and an error condition occurs.
    !>
    !> In left-to-right text this moves backward in the buffer, like
    !> \[backward_char].  Port of bindings.el:left-char (left-to-right
    !> paragraphs only).
    subroutine cmd_left_char(status)
        type(status_type), intent(out) :: status

        call backward_char(status, prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_left_char

    !> Move point N words to the right (to the left if N is negative).
    !>
    !> In left-to-right text this moves forward in the buffer, like
    !> \[forward_word].
    !>
    !> The word boundaries are normally determined by the buffer's syntax
    !> table and character script (according to `char_script_table').
    !>
    !> If an edge of the buffer or a field boundary is reached, point is left there.
    !> Field boundaries are not noticed if `inhibit_field_text_motion' is
    !> true.  Port of bindings.el:right-word (left-to-right paragraphs
    !> only).
    subroutine cmd_right_word(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call forward_word(prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_right_word

    !> Move point N words to the left (to the right if N is negative).
    !>
    !> In left-to-right text this moves backward in the buffer, like
    !> \[backward_word].
    !>
    !> The word boundaries are normally determined by the buffer's syntax
    !> table and character script (according to `char_script_table').
    !>
    !> If an edge of the buffer or a field boundary is reached, point is left there.
    !> Field boundaries are not noticed if `inhibit_field_text_motion' is
    !> true.  Port of bindings.el:left-word (left-to-right paragraphs
    !> only).
    subroutine cmd_left_word(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call backward_word(prefix_numeric_value(current_prefix_arg))
    end subroutine cmd_left_word

    !> Bind `keys' (kbd syntax) to command `cmd' in `map'.  Not a port.
    subroutine bind(map, keys, cmd)
        type(handle_type), intent(in) :: map
        character(len=*), intent(in) :: keys, cmd
        type(value_type), allocatable :: key(:)
        type(status_type) :: status

        call key_parse(keys, key, status)
        call define_key(map, key, value_type(name=cmd), status)
    end subroutine bind

    !> Bind the single event `c' to command `cmd' in `map'.  Not a port.
    subroutine bind_char(map, c, cmd)
        type(handle_type), intent(in) :: map
        integer(int64), intent(in) :: c
        character(len=*), intent(in) :: cmd
        type(value_type) :: key(1)
        type(status_type) :: status

        key(1) = value_type(c)
        call define_key(map, key, value_type(name=cmd), status)
    end subroutine bind_char

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
    subroutine init_bindings()
        type(status_type) :: status
        type(value_type) :: key(1)
        integer(int64) :: c

        call intern_command_names()
        ! bindings.el's help-event-list.
        help_events = [character(len=16) :: "<help>", "<f1>", "?"]
        ! Rebuilding: drop the old maps.
        if (is_keymap_live(global_map)) call destroy_keymap(global_map, status)
        if (is_keymap_live(esc_map)) call destroy_keymap(esc_map, status)
        if (is_keymap_live(ctl_x_map)) call destroy_keymap(ctl_x_map, status)
        if (is_keymap_live(ctl_x_x_map)) call destroy_keymap(ctl_x_x_map, status)
        if (is_keymap_live(ctl_x_4_map)) call destroy_keymap(ctl_x_4_map, status)
        if (is_keymap_live(ctl_x_r_map)) call destroy_keymap(ctl_x_r_map, status)
        if (is_keymap_live(function_key_map)) call destroy_keymap(function_key_map, status)
        if (is_keymap_live(goto_map)) call destroy_keymap(goto_map, status)
        if (is_keymap_live(narrow_map)) call destroy_keymap(narrow_map, status)
        if (is_keymap_live(search_map)) call destroy_keymap(search_map, status)
        if (is_keymap_live(abbrev_map)) call destroy_keymap(abbrev_map, status)
        if (is_keymap_live(undo_repeat_map)) call destroy_keymap(undo_repeat_map, status)
        if (is_keymap_live(buffer_navigation_repeat_map)) call destroy_keymap(buffer_navigation_repeat_map, status)
        if (is_keymap_live(next_error_repeat_map)) call destroy_keymap(next_error_repeat_map, status)
        if (is_keymap_live(page_navigation_repeat_map)) call destroy_keymap(page_navigation_repeat_map, status)
        if (is_keymap_live(other_window_repeat_map)) call destroy_keymap(other_window_repeat_map, status)
        ! First: bindings.el's minor-mode-alist starts with abbrev-mode.
        call init_abbrev()
        call init_cmds()
        call init_display_commands()
        call init_simple()
        call init_window_commands()
        call init_buffer_commands()
        commands%right_char%execute => cmd_right_char
        commands%complete_name%execute => cmd_complete_name
        commands%left_char%execute => cmd_left_char
        commands%right_word%execute => cmd_right_word
        commands%left_word%execute => cmd_left_word
        call init_syntax()
        call init_syntax_propertize()
        call init_smie()
        call init_paragraphs()
        call init_expression_motion()
        call init_search()
        ! A process filter or sentinel keeps the match data (search.c).
        begin_asynch_code => begin_running_asynch_code
        end_asynch_code => end_running_asynch_code
        call init_replace()
        call init_casefiddle()
        casefiddle_region_bounds => region_bounds
        casefiddle_is_region_noncontiguous => is_region_noncontiguous
        call init_indent()
        call init_fill()
        call init_newcomment()
        call init_novice()
        call init_text_mode()
        call init_conf_mode()
        call init_make_mode()
        call init_python()
        call init_css_mode()
        call init_js()
        call init_sgml_mode()
        call init_f90()
        call init_prog_mode()
        call init_jit_lock()
        call init_font_core()
        call init_font_lock()
        call init_executable()
        call init_sh_script()
        call init_process()
        call init_comint()
        call init_pcomplete()
        call init_ansi_color()
        call init_shell()
        call init_compile()
        call init_occur()
        call init_diff_mode()
        call init_diff()
        call init_rect()
        call init_savehist()
        call init_saveplace()
        call init_recentf()
        call init_tabulated_list()
        call init_simple_process()
        call init_bookmark()
        call init_buff_menu()
        call init_ibuffer()
        call init_info()
        call init_info_look()
        call init_register()
        call init_macros_commands()
        call init_kmacro()
        call init_dabbrev()
        ! subr.el: esc-map, ctl-x-map, global-map.
        call make_keymap(esc_map)
        call make_keymap(ctl_x_map)
        call make_keymap(global_map)
        key(1) = value_type(27_int64)
        call define_key(global_map, key, make_keymap_ref(esc_map), status)
        key(1) = value_type(24_int64)
        call define_key(global_map, key, make_keymap_ref(ctl_x_map), status)
        ! bindings.el: narrow-map.
        call make_sparse_keymap(narrow_map)
        call bind(narrow_map, "n", "narrow_to_region")
        call bind(narrow_map, "w", "widen")
        call bind(narrow_map, "g", "goto_line_relative")
        key(1) = value_type(int(iachar("n"), int64))
        call define_key(ctl_x_map, key, make_keymap_ref(narrow_map), status)
        call bind_char(global_map, 9_int64, "self_insert_command")

        do c = 32, 126
            call bind_char(global_map, c, "self_insert_command")
        end do

        ! mule-conf.el: all non-ASCII characters self-insert (this covers
        ! subr.el's 0240..0377 loop too).
        call define_key_range(global_map, 128_int64, int(MAX_CHAR, int64), &
                              value_type(name="self_insert_command"), status)
        call bind(global_map, "C-a", "move_beginning_of_line")
        call bind(global_map, "C-b", "backward_char")
        call bind(global_map, "C-e", "move_end_of_line")
        call bind(global_map, "C-f", "forward_char")
        call bind(global_map, "C-M-c", "exit_recursive_edit")
        call bind(global_map, "C-]", "abort_recursive_edit")
        call bind(global_map, "C-n", "next_line")
        call bind(global_map, "C-p", "previous_line")
        ! bindings.el
        call bind(global_map, "C-g", "keyboard_quit")
        call bind(global_map, "C-m", "newline")
        call bind(global_map, "C-o", "open_line")
        call bind(global_map, "C-q", "quoted_insert")
        call bind(global_map, "C-u", "universal_argument")

        do c = 0, 9
            call bind_char(esc_map, iachar("0") + c, "digit_argument")
            call bind_char(global_map, M_CTL + iachar("0") + c, "digit_argument")
            call bind_char(global_map, M_CTL + M_META + iachar("0") + c, "digit_argument")
        end do

        call bind(esc_map, "-", "negative_argument")
        call bind_char(global_map, M_CTL + iachar("-"), "negative_argument")
        call bind_char(global_map, M_CTL + M_META + iachar("-"), "negative_argument")
        call bind(global_map, "DEL", "delete_backward_char")
        call bind(global_map, "C-d", "delete_char")
        call bind(global_map, "<home>", "move_beginning_of_line")
        call bind(global_map, "<end>", "move_end_of_line")
        call bind(global_map, "<left>", "left_char")
        call bind(global_map, "<right>", "right_char")
        call bind(global_map, "<down>", "next_line")
        call bind(global_map, "<up>", "previous_line")
        call bind(global_map, "<deletechar>", "delete_forward_char")
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
        call bind(esc_map, "x", "execute_extended_command")
        call bind(esc_map, "TAB", "complete_name")
        call bind(global_map, "<next>", "scroll_up_command")
        call bind(global_map, "<prior>", "scroll_down_command")
        call bind(global_map, "C-v", "scroll_up_command")
        call bind(esc_map, "v", "scroll_down_command")
        call bind(esc_map, "C-v", "scroll_other_window")
        call bind(global_map, "M-<next>", "scroll_other_window")
        call bind(esc_map, "<next>", "scroll_other_window")
        call bind(global_map, "M-<prior>", "scroll_other_window_down")
        call bind(esc_map, "<prior>", "scroll_other_window_down")
        call bind(esc_map, "C-S-v", "scroll_other_window_down")
        ! window.el
        call bind(global_map, "C-l", "recenter_top_bottom")
        call bind(esc_map, "r", "move_to_window_line_top_bottom")
        ! Undo (bindings.el).
        commands%undo%advertised_binding = "C-x u"
        call bind(global_map, "C-_", "undo")
        call bind(global_map, "C-/", "undo")
        call bind(ctl_x_map, "u", "undo")
        call make_sparse_keymap(undo_repeat_map)
        call bind(undo_repeat_map, "u", "undo")
        call set_repeat_map(undo_repeat_map)
        ! bindings.el: ctl-x-x-map.
        call make_sparse_keymap(ctl_x_x_map)
        call bind(ctl_x_x_map, "f", "font_lock_update")
        key(1) = value_type(int(iachar("x"), int64))
        call define_key(ctl_x_map, key, make_keymap_ref(ctl_x_x_map), status)
        ! bindings.el: ctl-x-r-map (rect.el, register.el and bookmark.el;
        ! frameset-to-register on f is not ported).
        call bind(ctl_x_map, "SPC", "rectangle_mark_mode")
        call make_sparse_keymap(ctl_x_r_map)
        call bind(ctl_x_r_map, "c", "clear_rectangle")
        call bind(ctl_x_r_map, "k", "cut_rectangle")
        call bind(ctl_x_r_map, "d", "delete_rectangle")
        call bind(ctl_x_r_map, "y", "paste_rectangle")
        call bind(ctl_x_r_map, "o", "open_rectangle")
        call bind(ctl_x_r_map, "t", "string_rectangle")
        call bind(ctl_x_r_map, "N", "rectangle_number_lines")
        call bind(ctl_x_r_map, "M-w", "copy_rectangle")
        call bind(ctl_x_r_map, "C-@", "point_to_register")
        call bind(ctl_x_r_map, "C-SPC", "point_to_register")
        call bind(ctl_x_r_map, "SPC", "point_to_register")
        call bind(ctl_x_r_map, "j", "jump_to_register")
        call bind(ctl_x_r_map, "s", "copy_to_register")
        call bind(ctl_x_r_map, "x", "copy_to_register")
        call bind(ctl_x_r_map, "i", "insert_register")
        call bind(ctl_x_r_map, "g", "insert_register")
        call bind(ctl_x_r_map, "r", "copy_rectangle_to_register")
        call bind(ctl_x_r_map, "n", "number_to_register")
        call bind(ctl_x_r_map, "+", "increment_register")
        call bind(ctl_x_r_map, "w", "window_configuration_to_register")
        call bind(ctl_x_r_map, "F", "file_to_register")
        call bind(ctl_x_r_map, "B", "buffer_to_register")
        ! bookmark.el (autoloaded bindings).
        call bind(ctl_x_r_map, "b", "bookmark_jump")
        call bind(ctl_x_r_map, "m", "bookmark_set")
        call bind(ctl_x_r_map, "M", "bookmark_set_no_overwrite")
        call bind(ctl_x_r_map, "l", "bookmark_bmenu_list")
        key(1) = value_type(int(iachar("r"), int64))
        call define_key(ctl_x_map, key, make_keymap_ref(ctl_x_r_map), status)
        ! kmacro.el (autoloads): keyboard macros.
        call bind(ctl_x_map, "(", "kmacro_start_macro")
        call bind(ctl_x_map, ")", "kmacro_end_macro")
        call bind(ctl_x_map, "e", "kmacro_end_and_execute_macro")
        call bind(global_map, "<f3>", "kmacro_start_macro_or_insert_counter")
        call bind(global_map, "<f4>", "kmacro_end_or_execute_macro")
        key(1) = value_type(11_int64)
        call define_key(ctl_x_map, key, make_keymap_ref(kmacro_keymap), status)
        ! bindings.el: abbrev-map, and the abbrev keys.
        call make_sparse_keymap(abbrev_map)
        call bind(abbrev_map, "l", "add_mode_abbrev")
        call bind(abbrev_map, "C-a", "add_mode_abbrev")
        call bind(abbrev_map, "g", "add_global_abbrev")
        call bind(abbrev_map, "+", "add_mode_abbrev")
        call bind(abbrev_map, "i g", "inverse_add_global_abbrev")
        call bind(abbrev_map, "i l", "inverse_add_mode_abbrev")
        call bind(abbrev_map, "-", "inverse_add_global_abbrev")
        call bind(abbrev_map, "e", "expand_abbrev")
        call bind(abbrev_map, "'", "expand_abbrev")
        key(1) = value_type(int(iachar("a"), int64))
        call define_key(ctl_x_map, key, make_keymap_ref(abbrev_map), status)
        call bind(esc_map, "'", "abbrev_prefix_mark")
        call bind(ctl_x_map, "'", "expand_abbrev")
        ! dabbrev.el (autoloads).
        call bind(esc_map, "/", "dabbrev_expand")
        call bind(global_map, "C-M-/", "dabbrev_completion")
        call bind(global_map, "C-?", "undo_redo")
        call bind(global_map, "C-M-_", "undo_redo")
        ! The mark and killing (bindings.el).
        commands%set_mark_command%advertised_binding = "C-SPC"
        call bind(global_map, "C-@", "set_mark_command")
        call bind(global_map, "C-SPC", "set_mark_command")
        call bind(ctl_x_map, "C-x", "exchange_point_and_mark")
        call bind(ctl_x_map, "h", "mark_whole_buffer")
        ! Pages (bindings.el).
        call init_page()
        call bind(ctl_x_map, "[", "backward_page")
        call bind(ctl_x_map, "]", "forward_page")
        call bind(ctl_x_map, "C-p", "mark_page")
        call bind(ctl_x_map, "l", "count_lines_page")
        call bind(ctl_x_map, "n p", "narrow_to_page")
        call make_sparse_keymap(page_navigation_repeat_map)
        call bind(page_navigation_repeat_map, "]", "forward_page")
        call bind(page_navigation_repeat_map, "[", "backward_page")
        call set_repeat_map(page_navigation_repeat_map)
        ! Windows (window.el) and buffers (subr.el ctl-x-map).
        call bind(ctl_x_map, "0", "delete_window")
        call bind(ctl_x_map, "1", "delete_other_windows")
        call bind(ctl_x_map, "2", "split_window_below")
        call bind(ctl_x_map, "3", "split_window_right")
        call bind(ctl_x_map, "o", "other_window")
        call make_sparse_keymap(other_window_repeat_map)
        call bind(other_window_repeat_map, "o", "other_window")
        call bind(other_window_repeat_map, "O", "other_window_backward")
        call set_repeat_map(other_window_repeat_map)
        call bind(ctl_x_map, "b", "switch_to_buffer")
        call bind(ctl_x_map, "<right>", "next_buffer")
        call bind(ctl_x_map, "C-<right>", "next_buffer")
        call bind(global_map, "<XF86Forward>", "next_buffer")
        call bind(ctl_x_map, "<left>", "previous_buffer")
        call bind(ctl_x_map, "C-<left>", "previous_buffer")
        call bind(global_map, "<XF86Back>", "previous_buffer")
        call make_sparse_keymap(buffer_navigation_repeat_map)
        call bind(buffer_navigation_repeat_map, "<right>", "next_buffer")
        call bind(buffer_navigation_repeat_map, "<left>", "previous_buffer")
        call set_repeat_map(buffer_navigation_repeat_map)
        call bind(ctl_x_map, "k", "close_buffer")
        call bind(global_map, "C-w", "cut_region")
        call bind(esc_map, "w", "copy_region")
        call bind(global_map, "C-k", "cut_line")
        call bind(global_map, "C-y", "paste")
        call bind(esc_map, "y", "paste_previous")
        call bind(global_map, "<C-S-backspace>", "cut_whole_line")
        ! Words and paragraphs (bindings.el).
        call bind(esc_map, "@", "mark_word")
        call bind(esc_map, "f", "forward_word")
        call bind(esc_map, "b", "backward_word")
        call bind(esc_map, "d", "cut_word")
        call bind(esc_map, "DEL", "backward_cut_word")
        call bind(global_map, "M-<right>", "right_word")
        call bind(global_map, "M-;", "comment_dwim")
        call bind(global_map, "C-x ;", "comment_set_column")
        call bind(global_map, "C-x C-;", "comment_line")
        call bind(esc_map, "<right>", "forward_word")
        call bind(global_map, "M-<left>", "left_word")
        call bind(esc_map, "<left>", "backward_word")
        call bind(global_map, "C-<right>", "right_word")
        call bind(global_map, "C-<left>", "left_word")
        call bind(global_map, "C-<delete>", "cut_word")
        call bind(global_map, "C-<backspace>", "backward_cut_word")
        call bind(global_map, "C-<up>", "backward_paragraph")
        call bind(global_map, "C-<down>", "forward_paragraph")
        call bind(esc_map, "{", "backward_paragraph")
        call bind(esc_map, "}", "forward_paragraph")
        call bind(esc_map, "h", "mark_paragraph")
        call bind(esc_map, "a", "backward_sentence")
        call bind(esc_map, "e", "forward_sentence")
        call bind(esc_map, "k", "cut_sentence")
        call bind(ctl_x_map, "DEL", "backward_cut_sentence")
        ! Balanced expressions (bindings.el).
        call bind(global_map, "C-M-<left>", "backward_expression")
        call bind(esc_map, "C-<left>", "backward_expression")
        call bind(global_map, "C-M-<right>", "forward_expression")
        call bind(esc_map, "C-<right>", "forward_expression")
        call bind(global_map, "C-M-<up>", "backward_up_group")
        call bind(esc_map, "C-<up>", "backward_up_group")
        call bind(global_map, "C-M-<down>", "down_group")
        call bind(esc_map, "C-<down>", "down_group")
        call bind(esc_map, "C-f", "forward_expression")
        call bind(esc_map, "C-b", "backward_expression")
        call bind(esc_map, "C-u", "backward_up_group")
        call bind(esc_map, "C-@", "mark_expression")
        call bind(esc_map, "C-SPC", "mark_expression")
        call bind(esc_map, "C-d", "down_group")
        call bind(esc_map, "C-k", "cut_expression")
        call bind(global_map, "C-M-<delete>", "backward_cut_expression")
        call bind(global_map, "C-M-<backspace>", "backward_cut_expression")
        call bind(esc_map, "C-<delete>", "backward_cut_expression")
        call bind(esc_map, "C-<backspace>", "backward_cut_expression")
        call bind(global_map, "C-M-<home>", "beginning_of_subprogram")
        call bind(esc_map, "C-<home>", "beginning_of_subprogram")
        call bind(global_map, "C-M-<end>", "end_of_subprogram")
        call bind(esc_map, "C-<end>", "end_of_subprogram")
        call bind(esc_map, "C-a", "beginning_of_subprogram")
        call bind(esc_map, "C-e", "end_of_subprogram")
        call bind(esc_map, "C-h", "mark_subprogram")
        call bind(ctl_x_map, "n d", "narrow_to_subprogram")
        call bind(esc_map, "(", "insert_parentheses")
        call bind(esc_map, "C-n", "forward_group")
        call bind(esc_map, "C-p", "backward_group")
        call bind(esc_map, "%", "query_replace")
        call bind(esc_map, "C-%", "query_replace_regexp")
        call bind(esc_map, "!", "shell_command")
        call bind(esc_map, "|", "shell_command_on_region")
        call bind(esc_map, "&", "async_shell_command")
        call bind(ctl_x_map, "`", "next_error")
        call make_sparse_keymap(next_error_repeat_map)
        call bind(next_error_repeat_map, "n", "next_error")
        call bind(next_error_repeat_map, "M-n", "next_error")
        call bind(next_error_repeat_map, "p", "previous_error")
        call bind(next_error_repeat_map, "M-p", "previous_error")
        call set_repeat_map(next_error_repeat_map)
        ! bindings.el: (define-key ctl-x-map "z" 'repeat)
        call bind(ctl_x_map, "z", "repeat")
        ! bindings.el: goto-map (the keys of ported commands).
        call make_sparse_keymap(goto_map)
        call bind(goto_map, "n", "next_error")
        call bind(goto_map, "M-n", "next_error")
        call bind(goto_map, "p", "previous_error")
        call bind(goto_map, "M-p", "previous_error")
        call bind(goto_map, "c", "goto_char")
        call bind(goto_map, "g", "goto_line")
        call bind(goto_map, "M-g", "goto_line")
        call bind(goto_map, "TAB", "move_to_column")
        key(1) = value_type(int(iachar("g"), int64))
        call define_key(esc_map, key, make_keymap_ref(goto_map), status)
        ! bindings.el: search-map (the keys of ported commands).
        call make_sparse_keymap(search_map)
        call bind(search_map, "o", "occur")
        key(1) = value_type(int(iachar("s"), int64))
        call define_key(esc_map, key, make_keymap_ref(search_map), status)
        call bind(global_map, "C-t", "transpose_chars")
        call bind(esc_map, "t", "transpose_words")
        call bind(ctl_x_map, "C-b", "list_buffers")
        call bind(esc_map, "=", "count_words_region")
        call bind(ctl_x_map, "C-t", "transpose_lines")
        call bind(esc_map, "SPC", "cycle_spacing")
        call bind(esc_map, "\", "delete_horizontal_space")
        call bind(ctl_x_map, "C-o", "delete_blank_lines")
        call bind(esc_map, "^", "delete_indentation")
        call bind(esc_map, "u", "upcase_word")
        call bind(esc_map, "l", "downcase_word")
        call bind(esc_map, "c", "capitalize_word")
        call bind(ctl_x_map, "C-u", "upcase_region")
        call bind(ctl_x_map, "C-l", "downcase_region")
        call bind(global_map, "TAB", "indent_for_tab_command")
        call bind(esc_map, "C-\", "indent_region")
        call bind(ctl_x_map, "TAB", "indent_rigidly")
        call bind(esc_map, "i", "tab_to_tab_stop")
        call bind(esc_map, "m", "back_to_indentation")
        call bind(esc_map, "q", "fill_paragraph")
        call bind(ctl_x_map, "f", "set_fill_column")
        call bind(ctl_x_map, ".", "set_fill_prefix")
        call bind(esc_map, "j", "default_indent_new_line")
        call bind(esc_map, "C-j", "default_indent_new_line")
        call bind(esc_map, "<", "beginning_of_buffer")
        call bind(esc_map, ">", "end_of_buffer")
        call bind(global_map, "C-<home>", "beginning_of_buffer")
        call bind(global_map, "C-<end>", "end_of_buffer")
        call use_global_map(global_map, status)
        ! subr.el: (define-key ctl-x-map "4" 'ctl-x-4-prefix)
        call make_sparse_keymap(ctl_x_4_map)
        key(1) = value_type(int(iachar("4"), int64))
        call define_key(ctl_x_map, key, make_keymap_ref(ctl_x_4_map), status)
        call init_files(ctl_x_map, ctl_x_4_map)
        call init_vc_hooks(ctl_x_map)
        ! (After vc_hooks: `grep_find_ignored_directories' defaults to
        ! `vc_directory_exclusions'.)
        call init_grep()
        call init_vc()
        call init_log_view()
        call init_vc_dir()
        call init_autorevert()
        call init_repeat()
        call init_vc_git()
        call init_view()
        call init_dired_mode(ctl_x_map, ctl_x_4_map)
        call init_isearch(global_map)
        ! The minibuffer: minibuffer-local-map's keys from minibuffer.el,
        ! then its keys from bindings.el.
        if (is_keymap_live(minibuffer_local_map)) call destroy_keymap(minibuffer_local_map, status)
        call init_minibuf()
        call init_minibuffer()
        call init_auth_source()
        call init_crm()
        call init_subr()
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
        call init_help(global_map)
        call init_help_fns()
        call init_apropos()
        call init_mule(global_map, ctl_x_map, help_map)
        call init_quail()
        call init_leim_list()
        ! The other keymap variables, for \<MAP> and help--binding-locus.
        call register_keymap_name("esc_map", esc_map)
        call register_keymap_name("ctl_x_map", ctl_x_map)
        call register_keymap_name("ctl_x_4_map", ctl_x_4_map)
        call register_keymap_name("dired_mode_map", dired_mode_map)
        call register_keymap_name("ctl_x_r_map", ctl_x_r_map)
        call register_keymap_name("abbrev_map", abbrev_map)
        call register_keymap_name("edit_abbrevs_mode_map", edit_abbrevs_mode_map)
        call register_keymap_name("kmacro_keymap", kmacro_keymap)
        call register_keymap_name("bookmark_map", bookmark_map)
        call register_keymap_name("bookmark_minibuffer_read_name_map", bookmark_minibuffer_read_name_map)
        call register_keymap_name("isearch_mode_map", isearch_mode_map)
        call register_keymap_name("text_mode_map", text_mode_map)
        call register_keymap_name("prog_mode_map", prog_mode_map)
        call register_keymap_name("special_mode_map", special_mode_map)
        call register_keymap_name("compilation_mode_map", compilation_mode_map)
        call register_keymap_name("comint_mode_map", comint_mode_map)
        call register_keymap_name("shell_mode_map", shell_mode_map)
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
        call init_paren()
        call init_electric(global_map)
        call init_elec_pair()
        call minor_modes%show_paren_mode%enable(status)
        call minor_modes%electric_indent_mode%enable(status)
    end subroutine init_bindings
end module bindings_mod
