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
    use simple_fm, only: kill_new, current_kill, kill_region, yank, push_mark, set_mark, mark, &
                         region_active_p, deactivate_mark
    use minibuf_fm, only: completion_table_type, completion_list_table, completion_obarray_table, &
                          completion_function_table, completion_predicate, completion_function, &
                          try_completion, all_completions, test_completion
    use minibuffer_fm, only: completing_read, read_file_name, REQUIRE_MATCH_NIL, REQUIRE_MATCH_T, &
                             REQUIRE_MATCH_CONFIRM, REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION
    use simple_fm, only: execute_extended_command
    use keymap_fm, only: where_is_internal
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
    public :: region_beginning, region_end, kill_new, current_kill, kill_region, yank, push_mark, &
              set_mark, mark, region_active_p, deactivate_mark
    public :: completion_table_type, completion_list_table, completion_obarray_table, &
              completion_function_table, completion_predicate, completion_function, try_completion, &
              all_completions, test_completion, completing_read, read_file_name, REQUIRE_MATCH_NIL, &
              REQUIRE_MATCH_T, REQUIRE_MATCH_CONFIRM, REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION, &
              execute_extended_command, where_is_internal
    public :: tab_width, make_backup_files, line_move_visual, goal_column, &
              history_length, enable_recursive_minibuffers, history_add_new_input, &
              insert_default_directory, inhibit_field_text_motion, visible_bell, &
              transient_mark_mode, mark_even_if_inactive, kill_ring_max, mark_ring_max, &
              kill_whole_line_option, delete_active_region, kill_read_only_ok, completion_ignore_case, &
              completion_auto_help, completion_show_inline_help, completion_fail_discreetly, &
              suggest_key_bindings, completion_ignored_extensions
end module fortmacs
