! fortmacs_diff_multiple_mod.f90 -- Ediff session groups and the session registry.
! Ported from GNU Emacs 31.1 lisp/vc/ediff-mult.el (in part: see
! docs/status/fortmacs_diff_multiple_mod.md).
!
! Copyright (C) 1995-2026 Free Software Foundation, Inc.
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

!> Session groups: comparing or merging the files two or three
!> directories have in common, one Ediff session per file, from a
!> session group buffer ("*Ediff Session Group Panel*", Ediff Meta mode)
!> listing the sessions with their sizes and dates.  RET or v on a
!> session starts it (a subdirectory opens a group of its own), and
!> quitting it brings the group back; h, m and x mark and hide sessions,
!> = marks identical files, P collects the custom diffs of the marked
!> sessions, D shows the files only some directories have.  The registry
!> ("*Ediff Registry*", M-x eregistry, R in any Ediff buffer) lists the
!> active sessions and groups.
!>
!> What upstream keeps in a group buffer's local variables (the meta
!> list with its header, `fortmacs_diff_directory_difference_list', the parent group)
!> is one `fortmacs_diff_session_group_type' per group buffer in
!> `fortmacs_diff_session_groups'.  The session records of a buffer are found
!> through their overlays, as upstream finds the overlays'
!> `ediff-meta-info' property.
module fortmacs_diff_multiple_mod
    use, intrinsic :: iso_fortran_env, only: int64, int32
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE, USER_ERROR_CODE, WRONG_TYPE_ARGUMENT_CODE
    use handle_mod, only: handle_type, null_handle, same_handle
    use value_mod, only: value_type, command_type, vtag, xfixnum, V_INTEGER
    use string_type_mod, only: string_type
    use buffer_mod, only: buffers, current, current_buffer, get_buffer, set_buffer, get_or_make_buffer, is_buffer_live, &
                          erase_buffer, &
                          restore_buffer_modified, buffer_name, close_buffer
    use editing_functions_mod, only: point, goto_character, point_minimum, point_maximum, is_beginning_of_buffer, &
                                     is_end_of_buffer, character_after, insert_buffer_substring
    use insert_delete_mod, only: insert
    use editing_commands_mod, only: delete_character, backward_character
    use syntax_mod, only: skip_characters_forward, skip_characters_backward
    use search_mod, only: search_forward, SEARCH_NOERROR
    use interval_tree_mod, only: make_overlay, overlay_put, overlays_at, overlay_start, overlay_end, next_overlay_change, &
                                 previous_overlay_change, delete_overlay
    use keymap_mod, only: make_sparse_keymap, keymap_set, use_local_map
    use modes_mod, only: major_modes
    use hooks_mod, only: hook_type
    use execute_interactively_mod, only: this_command, current_prefix_argument, prefix_numeric_value, PREFIX_NONE
    use keyboard_mod, only: last_command_event
    use commands_mod, only: commands
    use window_mod, only: bury_buffer, select_window, set_window_buffer, other_window, display_buffer, delete_other_windows, &
                          window_body_width, display_action_parameters_type
    use frame_mod, only: selected_frame
    use file_input_output_mod, only: expand_file_name, file_name_as_directory, file_name_nondirectory, is_file_directory, &
                                     file_exists, file_attributes, file_status_type, copy_file
    use files_mod, only: abbreviate_file_name
    use utility_functions_mod, only: count_characters
    use time_functions_mod, only: decode_time, decoded_time_type, time_type
    use minibuffer_reading_mod, only: read_string
    use data_mod, only: string_setting_type, history_setting_type, forward_setting
    use basic_subprograms_mod, only: read_y_or_n, shell_quote_argument
    use replace_mod, only: format_prompt
    use help_mod, only: substitute_command_keys
    use display_update_mod, only: ding
    use display_engine_mod, only: message
    use diff_mode_mod, only: diff_mode
    use simple_mod, only: shell_command
    use fortmacs_diff_settings_mod, only: fortmacs_diff_sessions, fortmacs_diff_session_index, &
                                          close_fortmacs_diff_buffer_carefully, is_fortmacs_diff_ancestor_metajob, &
                                          is_fortmacs_diff_collect_diffs_metajob, is_fortmacs_diff_merge_metajob, &
                                          is_fortmacs_diff_metajob3, is_fortmacs_diff_comparison_metajob3, &
                                          abbreviate_fortmacs_diff_job_name, fortmacs_diff_autostore_merges, &
                                          fortmacs_diff_merge_filename_prefix, fortmacs_diff_fortmacs_help_in_control_buffer, &
                                          FORTMACS_DIFF_KILLED_VITAL_BUFFER, FORTMACS_DIFF_BUFFER_TYPE_A, &
                                          FORTMACS_DIFF_BUFFER_TYPE_B, FORTMACS_DIFF_BUFFER_TYPE_C
    use fortmacs_diff_utilities_mod, only: fortmacs_diff_unique_buffer_name, recenter_fortmacs_diff
    use fortmacs_diff_window_mod, only: get_fortmacs_diff_visible_buffer_window
    use fortmacs_diff_diff_mod, only: get_fortmacs_diff_directory_files, compare_fortmacs_diff_contents, &
                                      fortmacs_diff_custom_diff_program, fortmacs_diff_custom_diff_options
    use fortmacs_diff_mod, only: fortmacs_diff_files, fortmacs_diff_files3, fortmacs_diff_merge_files, &
                                 fortmacs_diff_merge_files_with_ancestor, fortmacs_diff_directories_internal
    implicit none(type, external)
    private

    public :: intersect_fortmacs_diff_directories, prepare_fortmacs_diff_meta_buffer, show_fortmacs_diff_meta_buffer, &
              update_fortmacs_diff_meta_buffer, &
              update_fortmacs_diff_registry, clean_up_fortmacs_diff_meta_buffer, fortmacs_diff_group_index, &
              fortmacs_diff_meta_mode, next_fortmacs_diff_meta_item, &
              previous_fortmacs_diff_meta_item, toggle_fortmacs_diff_verbose_help_meta_buffer, &
              toggle_fortmacs_diff_filename_truncation, &
              show_fortmacs_diff_registry, show_fortmacs_diff_current_session_meta_buffer, &
              show_fortmacs_diff_meta_buffer_from_registry, &
              quit_fortmacs_diff_meta_buffer, execute_fortmacs_diff_filegroup_action, execute_fortmacs_diff_registry_action, &
              mark_fortmacs_diff_for_hiding_at_position, mark_fortmacs_diff_for_operation_at_position, &
              hide_fortmacs_diff_marked_sessions, &
              unmark_fortmacs_diff_all_for_operation, unmark_fortmacs_diff_all_for_hiding, collect_fortmacs_diff_custom_diffs, &
              up_fortmacs_diff_meta_hierarchy, show_fortmacs_diff_directory_diffs, copy_fortmacs_diff_directory_diff_file, &
              bury_fortmacs_diff_directory_diffs_buffer, &
              mark_fortmacs_diff_meta_equal_files, get_fortmacs_diff_meta_information, initialize_fortmacs_diff_multiple, &
              get_fortmacs_diff_default_filtering_regular_expression, get_latest_fortmacs_diff_filtering_regular_expression

    character, parameter :: LF = achar(10), TAB = achar(9)

    ! ------------------------------------------------------------------
    ! Session groups
    ! ------------------------------------------------------------------

    !> The status of a session in its group (the character the group
    !> buffer shows): none, marked for hiding (H), hidden (I) or marked for
    !> an operation (*).  Upstream's ?H, ?I, ?* and no status.
    character, parameter, public :: FORTMACS_DIFF_SESSION_STATUS_NONE = " ", FORTMACS_DIFF_SESSION_STATUS_HIDE = "H", &
                                    FORTMACS_DIFF_SESSION_STATUS_HIDDEN = "I", FORTMACS_DIFF_SESSION_STATUS_MARKED = "*"

    !> The membership codes of a file in directory 1, 2 and 3: the code of
    !> a file is the product of the codes of the directories it is in.
    !> Port of ediff-mult.el ediff-membership-code1, -code2, -code3 and
    !> ediff-product-of-memcodes.
    integer, parameter, public :: FORTMACS_DIFF_MEMBERSHIP_CODE1 = 2, FORTMACS_DIFF_MEMBERSHIP_CODE2 = 3, &
                                  FORTMACS_DIFF_MEMBERSHIP_CODE3 = 5, &
                                  FORTMACS_DIFF_PRODUCT_OF_MEMBERSHIP_CODES = 30

    !> A file of a session in a group (upstream's (FILENAME EQSTATUS)): its
    !> name (unallocated for none), and whether it is equal to another file
    !> of the session.
    type, public :: fortmacs_diff_group_file_type
        character(len=:), allocatable :: name
        logical :: is_equal = .false.
    end type fortmacs_diff_group_file_type

    !> A session of a group (upstream's session meta descriptor
    !> (SESSION-CTL-BUFFER STATUS OBJA OBJB OBJC)): `buffer' is the null
    !> handle for a session never started, the control buffer (or the
    !> group buffer of a subdirectory) of a started one; `status' is an
    !> FORTMACS_DIFF_SESSION_STATUS_*; `files' are A, B and C.  `overlay' is the
    !> session's record in the group buffer (the null handle if not shown)
    !> and `invisible' whether it is hidden there.
    type, public :: fortmacs_diff_group_session_type
        type(handle_type) :: buffer
        character :: status = FORTMACS_DIFF_SESSION_STATUS_NONE
        type(fortmacs_diff_group_file_type) :: files(3)
        type(handle_type) :: overlay
        logical :: invisible = .false.
    end type fortmacs_diff_group_session_type

    !> A file that not all directories of a group have (an entry of
    !> upstream's `ediff-dir-difference-list', (FILE . NUMBER)): its name and
    !> its membership code (FORTMACS_DIFF_MEMBERSHIP_CODE*).
    type, public :: fortmacs_diff_directory_difference_type
        character(len=:), allocatable :: file
        integer :: membership = 1
    end type fortmacs_diff_directory_difference_type

    !> A file shown in the directory difference buffer, by its overlay:
    !> its name in the directories (`file') and its name in the directory
    !> of its column (`full').
    type :: fortmacs_diff_directory_diff_record_type
        type(handle_type) :: overlay
        character(len=:), allocatable :: file, full
    end type fortmacs_diff_directory_diff_record_type

    !> A session or group shown in the registry, by its overlay: its
    !> control buffer or group buffer.
    type :: fortmacs_diff_registry_record_type
        type(handle_type) :: overlay, buffer
    end type fortmacs_diff_registry_record_type

    !> A session group (and the registry), upstream's local variables of
    !> the group buffer.  `job_name' is the command that made the group
    !> (`fortmacs_diff_metajob_name'; null for the registry).  `regular_expression',
    !> `directory_a', `_b', `_c' and `merge_autostore_directory' are the meta
    !> list's header (unallocated for none), `sessions' its sessions.
    type, public :: fortmacs_diff_session_group_type
        !> The group buffer (`fortmacs_diff_meta_buffer'); the null handle for a
        !> free entry.
        type(handle_type) :: buffer
        type(command_type), pointer :: job_name => null()
        character(len=:), allocatable :: regular_expression, directory_a, directory_b, directory_c, merge_autostore_directory
        type(fortmacs_diff_group_session_type), allocatable :: sessions(:)
        !> `fortmacs_diff_directory_difference_list': the files not all directories have
        !> (`has_directory_differences' is .false. when there is no such list).
        logical :: has_directory_differences = .false.
        type(fortmacs_diff_directory_difference_type), allocatable :: directory_differences(:)
        !> The directory difference buffer (upstream's `ediff-dir-diffs-buffer'
        !> of the group buffer) and the files shown there.
        type(handle_type) :: directory_diffs_buffer
        !> Whether q closes the directory difference buffer: C redrew it
        !> (upstream's `ediff-dir-diffs-buffer' of that buffer itself).
        logical :: directory_diffs_closed_on_bury = .false.
        !> The registry only: whether the registry of sessions was empty when
        !> its buffer was made (upstream's meta list of the registry).
        logical :: registry_meta_list_empty = .false.
        type(fortmacs_diff_directory_diff_record_type), allocatable :: directory_diff_records(:)
        !> The registry's records.
        type(fortmacs_diff_registry_record_type), allocatable :: registry_records(:)
        !> `fortmacs_diff_parent_meta_buffer' and `fortmacs_diff_meta_session_number': the
        !> group this one is a session of, and its number there.
        type(handle_type) :: parent_meta_buffer
        integer :: meta_session_number = 0
        !> `fortmacs_diff_meta_diff_buffer': where P collects the custom diffs.
        type(handle_type) :: meta_diff_buffer
        !> `fortmacs_diff_verbose_help_enabled'.
        logical :: verbose_help_enabled = .false.
        !> `fortmacs_diff_recurse_to_subdirectories': whether the user was asked to
        !> compare subdirectories recursively, and the answer (upstream's
        !> empty value, yes and no).
        logical :: recursion_asked = .false., recurse_to_subdirectories = .false.
        !> `fortmacs_diff_meta_buffer_map', the group buffer's own keymap.
        type(handle_type) :: keymap
    contains
        !> Find the group's entry of `fortmacs_diff_session_groups' again.
        procedure, private :: get_index => get_group_index
        !> Update the activity markers and status of the sessions in the group buffer.
        procedure :: update_markers
        !> The other procedures acting on one group (ediff-mult.el); the group
        !> registry can grow under them, see fortmacs_diff_session_type.
        procedure, private :: is_registry, is_problematic_session, set_up_meta_map, redraw_directory_group_buffer, &
            insert_session_information, insert_directories, set_session_invisible, &
            update_session_marker, replace_session_activity_marker, replace_session_status, get_session_at, &
            get_visible_session_at, get_registry_record_at, get_record_overlay_at, is_record_invisible, &
            get_next_overlay_start, get_previous_overlay_start, next_item, previous_item, skip_to_item, &
            draw_directory_diffs, mark_session_for_hiding, mark_session_for_operation, &
            operate_on_marked_sessions, append_custom_diff
    end type fortmacs_diff_session_group_type

    !> The session groups and the registry, one per group buffer (entries
    !> of closed buffers are reused).
    type(fortmacs_diff_session_group_type), allocatable, target, public :: fortmacs_diff_session_groups(:)

    !> The registry buffer.  Port of ediff-mult.el ediff-registry-buffer.
    type(handle_type), public :: fortmacs_diff_registry_buffer

    !> The registry of Ediff sessions: their control buffers and the
    !> group buffers, the newest first.  Port of ediff-mult.el
    !> ediff-session-registry.
    type(handle_type), allocatable, public :: fortmacs_diff_session_registry(:)

    ! ------------------------------------------------------------------
    ! User options and hooks
    ! ------------------------------------------------------------------

    abstract interface
        !> A procedure computing the default filter of
        !> `fortmacs_diff_default_filtering_regular_expression' into REGULAR_EXPRESSION, "" for none.  Not a
        !> port.
        subroutine fortmacs_diff_default_filtering_regular_expression_procedure(regular_expression)
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: regular_expression
        end subroutine fortmacs_diff_default_filtering_regular_expression_procedure
    end interface

    public :: fortmacs_diff_default_filtering_regular_expression_procedure

    !> The setting `fortmacs_diff_default_filtering_regular_expression': a regular expression,
    !> set with `=', or a procedure computing it (R6-37), set with `=>'.
    !> Not a port.
    type, extends(string_setting_type), public :: fortmacs_diff_default_filtering_setting_type
        procedure(fortmacs_diff_default_filtering_regular_expression_procedure), pointer, nopass :: procedure => null()
    contains
        procedure :: clear_kinds => fortmacs_diff_default_filtering_regular_expression_clear_kinds
        procedure :: other_value => fortmacs_diff_default_filtering_regular_expression_other_value
    end type fortmacs_diff_default_filtering_setting_type

    !> Default regular expression used as a filename filter in multifile
    !> comparisons.
    !> A regular expression (`fortmacs_diff_default_filtering_regular_expression = "\.f90$"'),
    !> or a procedure computing it
    !> (`fortmacs_diff_default_filtering_regular_expression%procedure => mine'), for instance
    !> `get_latest_fortmacs_diff_filtering_regular_expression' (the last filter used); unset
    !> for none.  Port of ediff-mult.el ediff-default-filtering-regexp.
    type(fortmacs_diff_default_filtering_setting_type), public :: fortmacs_diff_default_filtering_regular_expression = &
                                                                  fortmacs_diff_default_filtering_setting_type( &
                                                                  name="fortmacs_diff_default_filtering_regular_expression")

    !> The filters of file names typed in Ediff's directory jobs, the
    !> newest first.  Port of ediff-mult.el ediff-filtering-regexp-history.
    type(history_setting_type), target, public :: fortmacs_diff_filtering_regular_expression_history = &
                                                  history_setting_type(name="fortmacs_diff_filtering_regular_expression_history")

    !> If true, truncate long file names in the session group buffers.
    !> This can be toggled with `toggle_fortmacs_diff_filename_truncation'.  Port of
    !> ediff-mult.el ediff-meta-truncate-filenames.
    logical, public :: fortmacs_diff_meta_truncate_filenames = .true.

    !> Hooks run just after setting up meta mode.  Port of ediff-mult.el
    !> ediff-meta-mode-hook.
    type(hook_type), target, public :: fortmacs_diff_meta_mode_hook = hook_type(name="fortmacs_diff_meta_mode_hook")
    !> Hooks run just after the registry control panel is set up.  Port of
    !> ediff-mult.el ediff-registry-setup-hook.
    type(hook_type), target, public :: fortmacs_diff_registry_setup_hook = hook_type(name="fortmacs_diff_registry_setup_hook")
    !> Hooks run just after a meta-buffer controlling a session group is
    !> run.
    !> One example of this is `fortmacs_diff_directories'.  Port of ediff-mult.el
    !> ediff-after-session-group-setup-hook.
    type(hook_type), target, public :: fortmacs_diff_after_session_group_setup_hook = &
                                       hook_type(name="fortmacs_diff_after_session_group_setup_hook")
    !> Hooks run just before exiting a session group.  Port of
    !> ediff-mult.el ediff-quit-session-group-hook.
    type(hook_type), target, public :: fortmacs_diff_quit_session_group_hook = hook_type( &
                                       name="fortmacs_diff_quit_session_group_hook")
    !> Hooks run just after the registry buffer is shown.  Port of
    !> ediff-mult.el ediff-show-registry-hook.
    type(hook_type), target, public :: fortmacs_diff_show_registry_hook = hook_type(name="fortmacs_diff_show_registry_hook")
    !> Hooks run just after a session group buffer is shown.  Port of
    !> ediff-mult.el ediff-show-session-group-hook (its default function,
    !> `delete_other_windows', is added by `initialize_fortmacs_diff_multiple').
    type(hook_type), target, public :: fortmacs_diff_show_session_group_hook = hook_type( &
                                       name="fortmacs_diff_show_session_group_hook")
    !> Hooks run just after setting up the `fortmacs_diff_meta_buffer_map'.
    !> This keymap controls key bindings in the meta buffer and is a local
    !> variable.  This means that you can set different bindings for
    !> different kinds of meta buffers.  Port of ediff-mult.el
    !> ediff-meta-buffer-keymap-setup-hook.
    type(hook_type), target, public :: fortmacs_diff_meta_buffer_keymap_setup_hook = &
                                       hook_type(name="fortmacs_diff_meta_buffer_keymap_setup_hook")

    !> Keymap for buffer showing differences between directories.  Port of
    !> ediff-mult.el ediff-dir-diffs-buffer-map.
    type(handle_type), public :: fortmacs_diff_directory_diffs_buffer_map

    ! ------------------------------------------------------------------
    ! Messages
    ! ------------------------------------------------------------------

    !> Port of ediff-mult.el ediff-meta-buffer-brief-message (before and
    !> after the job's name).
    character(len=*), parameter :: BRIEF_MESSAGE = "Ediff Session Group Panel: ", &
                                   BRIEF_MESSAGE_END = LF//LF//"     Type ? to show useful commands in this buffer"//LF//LF

    !> Port of ediff-mult.el ediff-meta-buffer-verbose-message (before and
    !> after the job's name).
    character(len=*), parameter :: VERBOSE_MESSAGE = "Ediff Session Group Panel: ", &
                                   VERBOSE_MESSAGE_END = LF//LF// &
                                   "Useful commands (type ? to hide them and free up screen):"//LF// &
                                   "     mouse-2, v, or RET over session record:   start that Ediff session"//LF// &
                                   "     M:"//TAB//"in sessions invoked from here, brings back this group panel"//LF// &
                                   "     R:"//TAB//"display the registry of active Ediff sessions"//LF// &
                                   "     h:"//TAB//"mark session for hiding (toggle)"//LF// &
                                   "     x:"//TAB//"hide marked sessions; with prefix arg: unhide"//LF// &
                                   "     m:"//TAB//"mark session for a non-hiding operation (toggle)"//LF// &
                                   " uh/um:"//TAB//"unmark all sessions marked for hiding/operation"//LF// &
                                   " n,SPC:"//TAB//"next session"//LF// &
                                   " p,DEL:"//TAB//"previous session"//LF// &
                                   "     E:"//TAB//"browse Ediff manual"//LF// &
                                   "     T:"//TAB//"toggle truncation of long file names"//LF// &
                                   "     q:"//TAB//"quit this session group"//LF

    !> The help at the top of the registry.  Port of the text of
    !> ediff-mult.el:ediff-redraw-registry-buffer.
    character(len=*), parameter :: REGISTRY_MESSAGE = "This is a registry of all active Ediff sessions."//LF//LF// &
                                   "Useful commands:"//LF// &
                                   "     mouse-2, `v', RET over a session record:  switch to that session"//LF// &
                                   "     M over a session record:  display the associated session group"//LF// &
                                   "     R in any Ediff session:   display session registry"//LF// &
                                   "     n,SPC: next session"//LF// &
                                   "     p,DEL: previous session"//LF// &
                                   "         E: browse Ediff manual"//LF// &
                                   "         q: bury registry"//LF//LF//LF// &
                                   TAB//TAB//"Active Ediff Sessions:"//LF// &
                                   TAB//TAB//"----------------------"//LF//LF

    !> How `mark_session_for_hiding' and `mark_session_for_operation' of a
    !> group change a mark: toggle it, set it
    !> (upstream's `mark'), or remove it (upstream's non-nil UNMARK).
    integer, parameter :: MARK_TOGGLE = 0, MARK_SET = 1, MARK_REMOVE = 2

    ! ------------------------------------------------------------------
    ! Starting a session from a group
    ! ------------------------------------------------------------------

    !> The group buffer, the session number and the file to store a merge
    !> in that the startup hooks of a session (or a subdirectory's group)
    !> started from a group give it: upstream's lambdas closing over them.
    type(handle_type) :: pending_meta_buffer
    integer :: pending_session_number = 0
    character(len=:), allocatable :: pending_merge_store_file

contains

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    !> The entry of `fortmacs_diff_session_groups' of group buffer BUFFER, 0 if
    !> none.  Not a port.
    integer function fortmacs_diff_group_index(buffer) result(group_index)
        type(handle_type), intent(in) :: buffer
        integer :: j

        group_index = 0
        if (.not. allocated(fortmacs_diff_session_groups)) return
        if (.not. is_buffer_live(buffer)) return

        do j = 1, size(fortmacs_diff_session_groups)
            if (same_handle(fortmacs_diff_session_groups(j)%buffer, buffer)) then
                group_index = j
                return
            end if
        end do
    end function fortmacs_diff_group_index

    ! A free entry of `fortmacs_diff_session_groups' (one of a closed buffer, or a
    ! new one), reset, in GROUP_INDEX.  Not a port.
    subroutine make_group_entry(group_index)
        integer, intent(out) :: group_index
        type(fortmacs_diff_session_group_type), allocatable :: grown(:)
        integer :: n

        if (.not. allocated(fortmacs_diff_session_groups)) allocate (fortmacs_diff_session_groups(0))

        do group_index = 1, size(fortmacs_diff_session_groups)
            if (.not. is_buffer_live(fortmacs_diff_session_groups(group_index)%buffer)) then
                fortmacs_diff_session_groups(group_index) = fortmacs_diff_session_group_type()
                return
            end if
        end do

        n = size(fortmacs_diff_session_groups)
        allocate (grown(n + 1))
        grown(1:n) = fortmacs_diff_session_groups
        call move_alloc(grown, fortmacs_diff_session_groups)
        group_index = n + 1
    end subroutine make_group_entry

    ! Append session S to SESSIONS.  Not a port.
    subroutine append_session(sessions, s)
        type(fortmacs_diff_group_session_type), allocatable, intent(inout) :: sessions(:)
        type(fortmacs_diff_group_session_type), intent(in) :: s
        type(fortmacs_diff_group_session_type), allocatable :: grown(:)
        integer :: n

        if (.not. allocated(sessions)) allocate (sessions(0))
        n = size(sessions)
        allocate (grown(n + 1))
        grown(1:n) = sessions
        grown(n + 1) = s
        call move_alloc(grown, sessions)
    end subroutine append_session

    ! The group of the current buffer in GROUP_INDEX, or the error condition of a
    ! command run elsewhere.  Port of
    ! ediff-init.el:ediff-barf-if-not-control-buffer (META-BUF-P).
    subroutine check_meta_buffer(group_index, status)
        integer, intent(out) :: group_index
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        status = status_type()
        group_index = fortmacs_diff_group_index(current_buffer())
        if (group_index > 0) return
        name = "null()"
        if (associated(this_command)) name = trim(this_command%name)
        call set_error(status, USER_ERROR_CODE, name//": This command runs in Ediff Control Buffer only!")
    end subroutine check_meta_buffer

    !> Give in GROUP_INDEX the entry of `fortmacs_diff_session_groups' that is
    !> the group, found again by its buffer (the array may have grown and
    !> moved since the caller took the entry); a `user_error' if the group
    !> has no entry (its buffer was closed).  Not a port: as
    !> ediff-init.el:ediff-barf-if-not-control-buffer (META-BUF-P).
    subroutine get_group_index(self, group_index, status)
        class(fortmacs_diff_session_group_type), intent(in) :: self
        integer, intent(out) :: group_index
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name

        status = status_type()
        group_index = fortmacs_diff_group_index(self%buffer)
        if (group_index > 0) return
        name = "null()"
        if (associated(this_command)) name = trim(this_command%name)
        call set_error(status, USER_ERROR_CODE, name//": This command runs in Ediff Control Buffer only!")
    end subroutine get_group_index

    ! Whether the group is the registry.  Not a port.
    logical function is_registry(self)
        class(fortmacs_diff_session_group_type), intent(in) :: self

        is_registry = same_handle(self%buffer, fortmacs_diff_registry_buffer)
    end function is_registry

    ! The strings of NAMES in the order of `is_string_less'.  Not a port.
    subroutine sort_names(names)
        type(string_type), intent(inout) :: names(:)
        type(string_type) :: key
        integer :: i, j

        do i = 2, size(names)
            key = names(i)
            j = i - 1

            do while (j >= 1)
                if (.not. is_less(key%text, names(j)%text)) exit
                names(j + 1) = names(j)
                j = j - 1
            end do

            names(j + 1) = key
        end do

    contains

        ! Whether A sorts before B, comparing characters by code (a prefix
        ! first).  Port of fns.c:string_cmp.
        pure logical function is_less(a, b)
            character(len=*), intent(in) :: a, b
            integer :: n

            n = min(len(a), len(b))

            if (a(1:n) == b(1:n)) then
                is_less = len(a) < len(b)
            else
                is_less = a(1:n) < b(1:n)
            end if
        end function is_less
    end subroutine sort_names

    ! Whether NAMES has NAME.  Not a port.
    pure logical function has_name(names, name)
        type(string_type), intent(in) :: names(:)
        character(len=*), intent(in) :: name
        integer :: j

        has_name = .false.

        do j = 1, size(names)
            if (names(j)%text == name) then
                has_name = .true.
                return
            end if
        end do
    end function has_name

    ! If STRING is longer than LENGTH characters, "..." and the end of
    ! STRING (LENGTH characters in all).  Port of
    ! subr-x.el:string-truncate-left.
    function truncate_left(string, length) result(text)
        character(len=*), intent(in) :: string
        integer, intent(in) :: length
        character(len=:), allocatable :: text
        integer(int64) :: string_length, keep, skip
        integer :: i

        string_length = count_characters(string)

        if (string_length <= length) then
            text = string
            return
        end if

        keep = max(0, length - 3)
        skip = min(string_length - 1, max(0_int64, string_length - keep))
        i = 0

        ! Skip SKIP characters.
        do while (skip > 0)
            i = i + 1
            if (i >= len(string)) exit
            if (iand(iachar(string(i + 1:i + 1)), 192) /= 128) skip = skip - 1
        end do

        text = "..."//string(i + 1:)
    end function truncate_left

    ! TEXT padded with blanks to WIDTH characters (format's %-WIDTHs).
    ! Not a port.
    function pad(text, width) result(padded)
        character(len=*), intent(in) :: text
        integer, intent(in) :: width
        character(len=:), allocatable :: padded

        padded = text//repeat(" ", int(max(0_int64, width - count_characters(text))))
    end function pad

    ! N right-aligned in WIDTH columns (format's %WIDTHd).  Not a port.
    function right(n, width) result(text)
        integer(int64), intent(in) :: n
        integer, intent(in) :: width
        character(len=:), allocatable :: text
        character(len=24) :: digits

        write (digits, "(i0)") n
        text = repeat(" ", max(0, width - len_trim(digits)))//trim(digits)
    end function right

    ! Insert TEXT, ignoring the read-only flag of the buffer (upstream's
    ! binding of `buffer-read-only' to nil).  Not a port.
    subroutine insert_text(text)
        character(len=*), intent(in) :: text
        type(status_type) :: st
        logical :: read_only

        read_only = buffers(current)%read_only
        buffers(current)%read_only = .false.
        call insert(text, st)
        buffers(current)%read_only = read_only
    end subroutine insert_text

    ! The name of buffer H, "" if none.  Not a port.
    function name_of(h) result(name)
        type(handle_type), intent(in) :: h
        character(len=:), allocatable :: name
        type(status_type) :: st

        name = ""
        if (is_buffer_live(h)) call buffer_name(h, name, st)
    end function name_of

    !> The default filter of file names: the procedure of
    !> `fortmacs_diff_default_filtering_regular_expression' if set, else its regular
    !> expression, "" for none.  Not a port: what upstream's commands do
    !> to read the option.
    function get_fortmacs_diff_default_filtering_regular_expression() result(regular_expression)
        character(len=:), allocatable :: regular_expression

        if (associated(fortmacs_diff_default_filtering_regular_expression%procedure)) then
            call fortmacs_diff_default_filtering_regular_expression%procedure(regular_expression)
        else if (allocated(fortmacs_diff_default_filtering_regular_expression%value)) then
            regular_expression = fortmacs_diff_default_filtering_regular_expression%value
        else
            regular_expression = ""
        end if
    end function get_fortmacs_diff_default_filtering_regular_expression

    !> REGULAR_EXPRESSION is the last filter of file names typed (the newest entry of
    !> `fortmacs_diff_filtering_regular_expression_history'), "" if none.  Port of the example
    !> value of ediff-mult.el ediff-default-filtering-regexp (the newest
    !> entry of ediff-filtering-regexp-history).
    subroutine get_latest_fortmacs_diff_filtering_regular_expression(regular_expression)
        character(len=:), allocatable, intent(out) :: regular_expression

        regular_expression = ""
        if (.not. allocated(fortmacs_diff_filtering_regular_expression_history%items)) return
        if (size(fortmacs_diff_filtering_regular_expression_history%items) == 0) return
        regular_expression = fortmacs_diff_filtering_regular_expression_history%items(1)%text
    end subroutine get_latest_fortmacs_diff_filtering_regular_expression

    !> Forget the procedure of `fortmacs_diff_default_filtering_regular_expression' (every
    !> `=' does).  Not a port.
    subroutine fortmacs_diff_default_filtering_regular_expression_clear_kinds(self)
        class(fortmacs_diff_default_filtering_setting_type), intent(inout) :: self

        nullify (self%procedure)
    end subroutine fortmacs_diff_default_filtering_regular_expression_clear_kinds

    !> The procedure of `fortmacs_diff_default_filtering_regular_expression' as help shows it.
    !> Not a port.
    function fortmacs_diff_default_filtering_regular_expression_other_value(self) result(text)
        class(fortmacs_diff_default_filtering_setting_type), intent(in) :: self
        character(len=:), allocatable :: text

        text = ""
        if (associated(self%procedure)) text = "a procedure set by the configuration"
    end function fortmacs_diff_default_filtering_regular_expression_other_value

    ! ------------------------------------------------------------------
    ! The meta list
    ! ------------------------------------------------------------------

    !> The activity marker of session S: + for an active session (Ediff
    !> runs in it), - for a finished one, " " for one never started.
    !> Port of ediff-mult.el:ediff-get-session-activity-marker (and the " " of
    !> ediff-insert-session-activity-marker-in-meta-buffer).
    function get_fortmacs_diff_session_activity_marker(s) result(marker)
        type(fortmacs_diff_group_session_type), intent(in) :: s
        character :: marker

        if (s%buffer%index == 0) then
            ! Virgin session.
            marker = " "
        else if (is_buffer_live(s%buffer)) then
            ! Active session.
            marker = "+"
        else
            marker = "-"
        end if
    end function get_fortmacs_diff_session_activity_marker

    ! Whether the file of FILE is a directory (.false. for none).
    ! Not a port.
    logical function is_directory(file)
        type(fortmacs_diff_group_file_type), intent(in) :: file

        is_directory = .false.
        if (allocated(file%name)) is_directory = is_file_directory(file%name)
    end function is_directory

    !> Whether session S is a group of its own: its files A and B (and C,
    !> if any) are directories.  Port of ediff-mult.el:ediff-meta-session-p.
    logical function is_fortmacs_diff_meta_session(s)
        type(fortmacs_diff_group_session_type), intent(in) :: s

        is_fortmacs_diff_meta_session = is_directory(s%files(1)) .and. is_directory(s%files(2))

        if (allocated(s%files(3)%name)) then
            is_fortmacs_diff_meta_session = is_fortmacs_diff_meta_session .and. is_directory(s%files(3))
        end if
    end function is_fortmacs_diff_meta_session

    !> Whether session S of the group is problematic: in -with-ancestor
    !> comparisons, when the ancestor is a directory rather than a file,
    !> or when there is no suitable ancestor file in the ancestor
    !> directory.  Port of ediff-mult.el:ediff-problematic-session-p (the
    !> one problem, ancestor-is-dir).
    logical function is_problematic_session(self, s)
        class(fortmacs_diff_session_group_type), intent(in) :: self
        type(fortmacs_diff_group_session_type), intent(in) :: s

        is_problematic_session = .false.
        if (.not. allocated(s%files(1)%name) .or. .not. allocated(s%files(2)%name)) return
        if (is_directory(s%files(1)) .or. is_directory(s%files(2))) return

        ! Either invalid file name or a directory.
        if (allocated(s%files(3)%name)) then
            if (.not. is_directory(s%files(3))) return
        end if

        is_problematic_session = is_fortmacs_diff_ancestor_metajob(self%job_name)
    end function is_problematic_session

    !> Intersect directories DIRECTORY1 and DIRECTORY2 (and DIRECTORY3, "" for none) for
    !> session group job JOB_NAME into GROUP: its header (REGULAR_EXPRESSION, the
    !> directories with a slash, MERGE_AUTOSTORE_DIRECTORY, the directory for
    !> auto-storing the results of merge jobs, "" for none), a session
    !> for each file (or subdirectory) DIRECTORY1 and DIRECTORY2 have in common, and
    !> the files only some of them have with their membership codes.
    !> REGULAR_EXPRESSION is "" or a filter regexp; only file names that match the
    !> regexp are considered.  If a file is a directory in directory1 but not
    !> directory2 (or vice versa), it is not included in the intersection.
    !> However, a regular file that is a directory in directory3 is included, since
    !> directory3 files are supposed to be ancestors for merging.  Port of
    !> ediff-mult.el:ediff-intersect-directories (string= compares, as
    !> COMPARISON-FUNC upstream).
    recursive subroutine intersect_fortmacs_diff_directories(job_name, regular_expression, directory1, directory2, directory3, &
                                                             merge_autostore_directory, group, status)
        type(command_type), pointer, intent(in) :: job_name
        character(len=*), intent(in) :: regular_expression, directory1, directory2, directory3, merge_autostore_directory
        type(fortmacs_diff_session_group_type), intent(out) :: group
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: names1(:), names2(:), names3(:), shared(:), union(:)
        type(fortmacs_diff_group_session_type) :: s
        type(fortmacs_diff_directory_difference_type), allocatable :: diffs(:)
        character(len=:), allocatable :: slashed1, slashed2, slashed3, element
        logical, allocatable :: keep(:)
        integer :: j

        slashed1 = file_name_as_directory(directory1)
        call entries(slashed1, names1)
        if (.not. status%ok()) return
        slashed2 = file_name_as_directory(directory2)
        call entries(slashed2, names2)
        if (.not. status%ok()) return
        allocate (names3(0))

        if (len(directory3) > 0) then
            slashed3 = file_name_as_directory(directory3)
            call entries(slashed3, names3)
            if (.not. status%ok()) return
        end if

        allocate (keep(size(names1)))

        do j = 1, size(names1)
            keep(j) = has_name(names2, names1(j)%text)
        end do

        shared = pack(names1, keep)

        ! In merge with ancestor jobs, we don't intersect with names3.
        ! If there is no ancestor, we'll offer to merge without the
        ! ancestor.  So, we intersect with names3 only when we are doing
        ! 3-way file comparison.
        if (size(names3) > 0 .and. is_fortmacs_diff_comparison_metajob3(job_name)) then
            deallocate (keep)
            allocate (keep(size(shared)))

            do j = 1, size(shared)
                keep(j) = has_name(names3, shared(j)%text)
            end do

            shared = pack(shared, keep)
        end if

        call sort_names(shared)
        ! Compute difference list.
        union = names1

        do j = 1, size(names2)
            if (.not. has_name(union, names2(j)%text)) union = [union, names2(j)]
        end do

        do j = 1, size(names3)
            if (.not. has_name(union, names3(j)%text)) union = [union, names3(j)]
        end do

        deallocate (keep)
        allocate (keep(size(union)))

        do j = 1, size(union)
            keep(j) = .not. has_name(shared, union(j)%text)
        end do

        union = pack(union, keep)
        call sort_names(union)
        ! Check for files belonging to names1/2/3.  The membership code is
        ! a product of a subset of the codes of the directories.
        allocate (diffs(size(union)))

        do j = 1, size(union)
            diffs(j)%file = union(j)%text
            if (has_name(names1, union(j)%text)) diffs(j)%membership = diffs(j)%membership*FORTMACS_DIFF_MEMBERSHIP_CODE1
            if (has_name(names2, union(j)%text)) diffs(j)%membership = diffs(j)%membership*FORTMACS_DIFF_MEMBERSHIP_CODE2
            if (has_name(names3, union(j)%text)) diffs(j)%membership = diffs(j)%membership*FORTMACS_DIFF_MEMBERSHIP_CODE3
        end do

        group%job_name => job_name
        group%regular_expression = regular_expression
        group%directory_a = slashed1
        group%directory_b = slashed2
        if (allocated(slashed3)) group%directory_c = slashed3
        group%merge_autostore_directory = ""
        if (len(merge_autostore_directory) > 0) group%merge_autostore_directory = file_name_as_directory(merge_autostore_directory)
        group%has_directory_differences = .true.
        call move_alloc(diffs, group%directory_differences)
        allocate (group%sessions(0))

        do j = 1, size(shared)
            s = fortmacs_diff_group_session_type()
            element = shared(j)%text
            call expand_file_name(slashed1//element, s%files(1)%name, status)
            if (.not. status%ok()) return
            call expand_file_name(slashed2//element, s%files(2)%name, status)
            if (.not. status%ok()) return

            if (size(names3) > 0) then
                ! In merging with ancestor, we don't intersect with names3.
                ! So, it is possible that element is a file in slashed1/2 but a
                ! directory in slashed3 Or element may not exist in slashed3 at
                ! all.  In the first case, we add a slash at the end.  In
                ! the second case, there is no file C.
                element = slash_if_directory(slashed3, element)

                if (file_exists(slashed3//element)) then
                    call expand_file_name(slashed3//element, s%files(3)%name, status)
                    if (.not. status%ok()) return
                end if
            end if

            call append_session(group%sessions, s)
        end do

    contains

        ! The files of DIRECTORY matching REGULAR_EXPRESSION, but . and .., directories with
        ! a slash.
        recursive subroutine entries(directory, names)
            character(len=*), intent(in) :: directory
            type(string_type), allocatable, intent(out) :: names(:)
            type(string_type), allocatable :: all(:)
            logical, allocatable :: kept(:)
            integer :: i

            call get_fortmacs_diff_directory_files(directory, regular_expression, .false., all, status)
            allocate (kept(size(all)))

            do i = 1, size(all)
                kept(i) = all(i)%text /= "." .and. all(i)%text /= ".."
                all(i)%text = slash_if_directory(directory, all(i)%text)
            end do

            names = pack(all, kept)
        end subroutine entries
    end subroutine intersect_fortmacs_diff_directories

    ! FILE, with a slash if DIRECTORY//FILE is a directory.  Port of
    ! ediff-mult.el:ediff-add-slash-if-directory.
    function slash_if_directory(directory, file) result(name)
        character(len=*), intent(in) :: directory, file
        character(len=:), allocatable :: name

        if (is_file_directory(directory//file)) then
            name = file_name_as_directory(file)
        else
            name = file
        end if
    end function slash_if_directory

    ! ------------------------------------------------------------------
    ! The group buffer
    ! ------------------------------------------------------------------

    !> This mode controls all operations on Ediff session groups.
    !> It is entered through one of the following commands:
    !> `fortmacs_diff_directories'
    !> `edirs'
    !> `fortmacs_diff_directories3'
    !> `edirs3'
    !> `fortmacs_diff_merge_directories'
    !> `edirs_merge'
    !> `fortmacs_diff_merge_directories_with_ancestor'
    !> `edirs_merge_with_ancestor'
    !> Port of ediff-mult.el:ediff-meta-mode (the revision jobs are
    !> Deferred).
    recursive subroutine fortmacs_diff_meta_mode(status)
        type(status_type), intent(out) :: status
        logical :: saved

        call major_modes%fortmacs_diff_meta_mode%begin(saved, status)
        if (.not. status%ok()) return
        call major_modes%fortmacs_diff_meta_mode%end(saved, status)
    end subroutine fortmacs_diff_meta_mode

    !> Make the buffer of group GROUP (a meta list, the registry if
    !> REGISTRY): a buffer named like BUFFER_NAME in Ediff Meta mode, its
    !> keymap, its contents; STARTUP_HOOKS run in it, then
    !> `fortmacs_diff_registry_setup_hook' or
    !> `fortmacs_diff_after_session_group_setup_hook'.  META_BUFFER is the buffer.
    !> Port of ediff-mult.el:ediff-prepare-meta-buffer (ACTION-FUNC and
    !> REDRAW-FUNCTION are those of a group of files, or of the registry).
    recursive subroutine prepare_fortmacs_diff_meta_buffer(group, buffer_name, registry, meta_buffer, status, startup_hooks)
        type(fortmacs_diff_session_group_type), intent(in) :: group
        character(len=*), intent(in) :: buffer_name
        logical, intent(in) :: registry
        type(handle_type), intent(out) :: meta_buffer
        type(status_type), intent(out) :: status
        type(hook_type), intent(inout), optional :: startup_hooks
        type(handle_type) :: old, map
        type(status_type) :: st
        integer :: group_index

        old = current_buffer()
        call get_or_make_buffer(fortmacs_diff_unique_buffer_name(buffer_name, "*"), meta_buffer, status)
        if (.not. status%ok()) return
        call set_buffer(meta_buffer, status)
        if (.not. status%ok()) return

        body: block
            ! Comes first.
            call fortmacs_diff_meta_mode(status)
            if (.not. status%ok()) exit body
            call make_group_entry(group_index)
            fortmacs_diff_session_groups(group_index) = group
            fortmacs_diff_session_groups(group_index)%buffer = meta_buffer

            if (.not. allocated(fortmacs_diff_session_groups(group_index)%sessions)) then
                allocate (fortmacs_diff_session_groups(group_index)%sessions(0))
            end if

            ! Comes after the action is known.
            if (registry) then
                fortmacs_diff_registry_buffer = meta_buffer
                fortmacs_diff_session_groups(group_index)%registry_meta_list_empty = size(registry_list()) == 0
            end if

            call fortmacs_diff_session_groups(group_index)%set_up_meta_map(status)
            if (.not. status%ok()) exit body
            map = fortmacs_diff_session_groups(group_index)%keymap
            ! This command is used only from the registry buffer, not from
            ! other meta-buffers.
            if (registry) call keymap_set(map, "M", "show_fortmacs_diff_meta_buffer_from_registry", status)
            if (.not. status%ok()) exit body
            if (.not. registry) fortmacs_diff_session_registry = [meta_buffer, registry_list()]

            if (registry) then
                call redraw_fortmacs_diff_registry_buffer(status)
            else
                call fortmacs_diff_session_groups(group_index)%redraw_directory_group_buffer(status)
            end if

            if (.not. status%ok()) exit body
            call set_buffer(meta_buffer, status)
            if (.not. status%ok()) exit body
            ! Set read-only/non-modified.
            buffers(current)%read_only = .true.
            call restore_buffer_modified(.false.)

            if (present(startup_hooks)) then
                call startup_hooks%execute(status)
                if (.not. status%ok()) exit body
                call set_buffer(meta_buffer, status)
                if (.not. status%ok()) exit body
            end if

            if (.not. registry) then
                ! Arrange to show directory contents differences.  Must be
                ! after the startup hooks, which may set them.
                group_index = fortmacs_diff_group_index(meta_buffer)
                call fortmacs_diff_session_groups(group_index)%draw_directory_diffs(status)
                if (.not. status%ok()) exit body
                call set_buffer(meta_buffer, status)
                if (.not. status%ok()) exit body
                call bind("h", "mark_fortmacs_diff_for_hiding_at_position")
                call bind("x", "hide_fortmacs_diff_marked_sessions")
                call bind("m", "mark_fortmacs_diff_for_operation_at_position")
                call bind("u m", "unmark_fortmacs_diff_all_for_operation")
                call bind("u h", "unmark_fortmacs_diff_all_for_hiding")
                if (is_fortmacs_diff_collect_diffs_metajob(fortmacs_diff_session_groups(group_index)%job_name)) call bind("P", &
                                                                                               "collect_fortmacs_diff_custom_diffs")
                call bind("^", "up_fortmacs_diff_meta_hierarchy")
                call bind("D", "show_fortmacs_diff_directory_diffs")
                if (.not. status%ok()) exit body
            end if

            if (registry) then
                call fortmacs_diff_registry_setup_hook%execute(status)
            else
                call fortmacs_diff_after_session_group_setup_hook%execute(status)
            end if
        end block body

        ! Make the old buffer current again, keeping the first error.
        if (is_buffer_live(old)) then
            call set_buffer(old, st)
            if (status%ok()) status = st
        end if

    contains

        ! Bind KEY to COMMAND in the group's keymap unless an earlier
        ! binding failed.
        subroutine bind(key, command)
            character(len=*), intent(in) :: key, command

            if (.not. status%ok()) return
            call keymap_set(map, key, command, status)
        end subroutine bind
    end subroutine prepare_fortmacs_diff_meta_buffer

    ! The registry, [] if none yet.  Not a port.
    function registry_list() result(list)
        type(handle_type), allocatable :: list(:)

        if (allocated(fortmacs_diff_session_registry)) then
            list = fortmacs_diff_session_registry
        else
            allocate (list(0))
        end if
    end function registry_list

    !> Set up the keymap of the group's buffer (`fortmacs_diff_meta_buffer_map') and
    !> use it, then run `fortmacs_diff_meta_buffer_keymap_setup_hook'.  Port of
    !> ediff-mult.el:ediff-setup-meta-map (the menu, E and mouse-2 are
    !> Deferred).
    recursive subroutine set_up_meta_map(self, status)
        class(fortmacs_diff_session_group_type), intent(inout), target :: self
        type(status_type), intent(out) :: status
        type(handle_type) :: map
        character(len=:), allocatable :: action
        character :: digit
        integer :: d
        integer :: group_index

        call self%get_index(group_index, status)
        if (.not. status%ok()) return
        call make_sparse_keymap(map)
        fortmacs_diff_session_groups(group_index)%keymap = map
        action = "execute_fortmacs_diff_filegroup_action"
        if (fortmacs_diff_session_groups(group_index)%is_registry()) action = "execute_fortmacs_diff_registry_action"
        ! (suppress-keymap ediff-meta-buffer-map)
        call bind("<remap> <self_insert_command>", "undefined")
        call bind("-", "negative_argument")

        do d = 0, 9
            write (digit, "(i1)") d
            call bind(digit, "digit_argument")
        end do

        call bind("?", "toggle_fortmacs_diff_verbose_help_meta_buffer")
        call bind("q", "quit_fortmacs_diff_meta_buffer")
        call bind("T", "toggle_fortmacs_diff_filename_truncation")
        call bind("R", "show_fortmacs_diff_registry")
        call bind("v", action)
        call bind("C-m", action)
        call bind("SPC", "next_fortmacs_diff_meta_item")
        call bind("n", "next_fortmacs_diff_meta_item")
        call bind("DEL", "previous_fortmacs_diff_meta_item")
        call bind("p", "previous_fortmacs_diff_meta_item")
        call bind("<delete>", "previous_fortmacs_diff_meta_item")
        call bind("<backspace>", "previous_fortmacs_diff_meta_item")
        call bind("= =", "mark_fortmacs_diff_meta_equal_files")
        call bind("= m", "mark_fortmacs_diff_meta_equal_files")
        call bind("= h", "mark_fortmacs_diff_meta_equal_files")
        if (.not. fortmacs_diff_fortmacs_help_in_control_buffer) call bind("C-h", "previous_fortmacs_diff_meta_item")
        if (.not. status%ok()) return
        call use_local_map(map, status)
        if (.not. status%ok()) return
        ! Modify ediff-meta-buffer-map here.
        call fortmacs_diff_meta_buffer_keymap_setup_hook%execute(status)

    contains

        ! Bind KEY to COMMAND in the map unless an earlier binding failed.
        subroutine bind(key, command)
            character(len=*), intent(in) :: key, command

            if (.not. status%ok()) return
            call keymap_set(map, key, command, status)
        end subroutine bind
    end subroutine set_up_meta_map

    !> Redraw the buffer of the group (`fortmacs_diff_directories' and the other
    !> directory jobs): the help, the directories, and a record for each
    !> session not hidden.  Port of
    !> ediff-mult.el:ediff-redraw-directory-group-buffer.
    recursive subroutine redraw_directory_group_buffer(self, status)
        class(fortmacs_diff_session_group_type), intent(inout), target :: self
        type(status_type), intent(out) :: status
        type(handle_type) :: old
        type(status_type) :: st
        integer(int64) :: saved_point
        logical :: empty
        integer :: n
        integer :: group_index

        call self%get_index(group_index, status)
        if (.not. status%ok()) return
        old = current_buffer()
        call set_buffer(fortmacs_diff_session_groups(group_index)%buffer, status)
        if (.not. status%ok()) return

        associate (g => fortmacs_diff_session_groups(group_index))
            saved_point = point()
            buffers(current)%read_only = .false.
            call erase_buffer(status)

            if (.not. status%ok()) then
                if (is_buffer_live(old)) call set_buffer(old, st)
                return
            end if

            ! Delete phony overlays that used to represent sessions before
            ! the buffer was redrawn.
            do n = 1, size(g%sessions)
                if (g%sessions(n)%overlay%index /= 0) call delete_overlay(g%sessions(n)%overlay)
                g%sessions(n)%overlay = null_handle()
                g%sessions(n)%invisible = .false.
            end do

            if (g%verbose_help_enabled) then
                call insert_text(VERBOSE_MESSAGE//abbreviate_fortmacs_diff_job_name(g%job_name)//VERBOSE_MESSAGE_END)

                if (is_fortmacs_diff_collect_diffs_metajob(g%job_name)) then
                    call insert_text("     P:"//TAB//"collect custom diffs of all marked sessions"//LF)
                end if

                call insert_text("     ^:"//TAB//"show parent session group"//LF)
                call insert_text("     D:"//TAB//"show differences among directories"//LF// &
                                 "    ==:"//TAB//"for each session, show which files are identical"//LF// &
                                 "    =h:"//TAB//"like ==, but also marks sessions for hiding"//LF// &
                                 "    =m:"//TAB//"like ==, but also marks sessions for operation"//LF//LF)
            else
                call insert_text(BRIEF_MESSAGE//abbreviate_fortmacs_diff_job_name(g%job_name)//BRIEF_MESSAGE_END)
            end if

            call insert_text(LF)

            if (allocated(g%regular_expression)) then
                if (len(g%regular_expression) > 0) call insert_text("*** Filter-through regular expression: " &
                                                                    //g%regular_expression//LF)
            end if

            call fortmacs_diff_session_groups(group_index)%insert_directories()

            if (.not. fortmacs_diff_autostore_merges%is_off() .and. is_fortmacs_diff_merge_metajob(g%job_name)) then
                if (allocated(g%merge_autostore_directory)) then
                    if (len(g%merge_autostore_directory) > 0) then
                        call insert_text(LF//"Merge results are automatically stored in:"//LF//TAB//g%merge_autostore_directory//LF)
                    end if
                end if
            end if

            call insert_text(LF//LF//"        Size   Last modified           Name"//LF// &
                             "    ----------------------------------------------"//LF//LF)
            empty = all(g%sessions%status == FORTMACS_DIFF_SESSION_STATUS_HIDDEN)
            if (empty) call insert_text("     ******   ******   This session group has no members"//LF)

            ! Now organize file names like this:
            !     use-mark sizeA dateA  sizeB dateB  filename
            ! make sure directories are displayed with a trailing slash.
            do n = 1, size(g%sessions)
                if (g%sessions(n)%status == FORTMACS_DIFF_SESSION_STATUS_HIDDEN) cycle
                call fortmacs_diff_session_groups(group_index)%insert_session_information(n, status)
                if (.not. status%ok()) exit
            end do

            call restore_buffer_modified(.false.)
            buffers(current)%read_only = .true.
            call goto_character(saved_point)
        end associate

        ! Make the old buffer current again, keeping the first error.
        if (is_buffer_live(old)) then
            call set_buffer(old, st)
            if (status%ok()) status = st
        end if
    end subroutine redraw_directory_group_buffer

    !> Insert all file info of session SESSION_NUMBER of the group at point: the activity
    !> marker, the status, the session's number and its files, with an
    !> overlay around them.  Port of
    !> ediff-mult.el:ediff-insert-session-info-in-meta-buffer (with
    !> ediff-insert-session-activity-marker-in-meta-buffer and
    !> ediff-insert-session-status-in-meta-buffer).
    recursive subroutine insert_session_information(self, session_number, status)
        class(fortmacs_diff_session_group_type), intent(inout), target :: self
        integer, intent(in) :: session_number
        type(status_type), intent(out) :: status
        integer(int64) :: beginning_of_record
        character(len=12) :: number
        integer :: t
        integer :: group_index

        call self%get_index(group_index, status)
        if (.not. status%ok()) return

        associate (s => fortmacs_diff_session_groups(group_index)%sessions(session_number))
            beginning_of_record = point()
            ! Insert activity marker, i.e., SPC, - or +.
            call insert_text(get_fortmacs_diff_session_activity_marker(s))
            ! Insert session status, i.e., *, H.
            call insert_text(s%status)
            write (number, "(i0)") session_number
            call insert_text("  Session "//trim(number)//":"//LF)

            do t = 1, 3
                call insert_fortmacs_diff_meta_file_information(s%files(t), status)
                if (.not. status%ok()) return
            end do

            call set_fortmacs_diff_meta_overlay(beginning_of_record, point(), s%overlay, status, &
                                                hidden=s%status == FORTMACS_DIFF_SESSION_STATUS_HIDDEN)
            s%invisible = s%status == FORTMACS_DIFF_SESSION_STATUS_HIDDEN
        end associate
    end subroutine insert_session_information

    !> Insert the line of FILE at point: =, its size, its date and its
    !> name; nothing for no file.  Port of
    !> ediff-mult.el:ediff-meta-insert-file-info1 (no remote files).
    recursive subroutine insert_fortmacs_diff_meta_file_information(file, status)
        type(fortmacs_diff_group_file_type), intent(in) :: file
        type(status_type), intent(out) :: status
        type(file_status_type) :: stat
        character(len=:), allocatable :: size, date, name
        logical :: found
        integer :: maximum_filename_width

        status = status_type()
        if (.not. allocated(file%name)) return
        maximum_filename_width = 500

        if (fortmacs_diff_meta_truncate_filenames) then
            maximum_filename_width = window_body_width(selected_frame%selected_window, status) - 41
            if (.not. status%ok()) return
        end if

        found = .false.
        if (file_exists(file%name)) call file_attributes(file%name, stat, found)

        if (found) then
            ! Set real size and modtime.
            size = right(stat%size, 10)
            date = format_fortmacs_diff_date(stat%mtime_second)
        else
            ! File doesn't exist.
            size = repeat(" ", 8)//"--"
            date = "*file doesn't exist*"
        end if

        ! Dir names in meta lists have trailing slashes, so we just
        ! abbreviate the file name, if file exists.
        name = truncate_left(abbreviate_file_name(file%name), maximum_filename_width)
        call insert_text(merge("=", " ", file%is_equal)//"  "//size//"   "//pad(date, 20)//"   "//name//LF)
    end subroutine insert_fortmacs_diff_meta_file_information

    !> The modification time SECONDS (since the epoch) as the group buffer
    !> shows it: "Mon DD YYYY HH:MM:SS" in local time.  Port of
    !> ediff-mult.el:ediff-format-date (with ediff-months and
    !> ediff-fill-leading-zero).
    function format_fortmacs_diff_date(seconds) result(text)
        integer(int64), intent(in) :: seconds
        character(len=:), allocatable :: text
        character(len=3), parameter :: MONTHS(12) = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", &
                                                     "Oct", "Nov", "Dec"]
        type(decoded_time_type) :: time
        character(len=32) :: buffer

        time = decode_time(time_type(seconds=seconds))
        write (buffer, "(a, ' ', i2, ' ', i4, ' ', i2.2, ':', i2.2, ':', i2.2)") MONTHS(time%month), time%day, time%year, &
            time%hour, time%minute, time%second%seconds
        text = trim(buffer)
    end function format_fortmacs_diff_date

    !> Insert the directories of the group.  Port of
    !> ediff-mult.el:ediff-insert-dirs-in-meta-buffer.
    subroutine insert_directories(self)
        class(fortmacs_diff_session_group_type), intent(inout), target :: self
        character(len=:), allocatable :: directory_a, directory_b, directory_c
        logical :: has_b, has_c

        ! Copies: inserting runs the change hooks, which may make groups.
        directory_a = abbreviate_file_name(self%directory_a)
        has_b = allocated(self%directory_b)
        has_c = allocated(self%directory_c)
        directory_b = ""
        if (has_b) directory_b = abbreviate_file_name(self%directory_b)
        directory_c = ""
        if (has_c) directory_c = abbreviate_file_name(self%directory_c)
        call insert_text("*** Directory A: "//directory_a//LF)
        if (has_b) call insert_text("*** Directory B: "//directory_b//LF)
        if (has_c) call insert_text("*** Directory C: "//directory_c//LF)
        call insert_text(LF)
    end subroutine insert_directories

    !> Put an overlay around a record, from BEGINNING to END, in OVERLAY;
    !> HIDDEN makes it invisible.  Port of
    !> ediff-mult.el:ediff-set-meta-overlay (the record's information is
    !> kept by the caller; `mouse-face' and `follow-link' wait for the
    !> mouse).
    subroutine set_fortmacs_diff_meta_overlay(beginning, end, overlay, status, hidden)
        integer(int64), intent(in) :: beginning, end
        type(handle_type), intent(out) :: overlay
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: hidden
        logical :: invisible

        call make_overlay(overlay, beginning, end, status)
        if (.not. status%ok()) return
        invisible = .false.
        if (present(hidden)) invisible = hidden
        call overlay_put(overlay, "invisible", value_type(invisible), status)
    end subroutine set_fortmacs_diff_meta_overlay

    ! Make the overlay of session SESSION_NUMBER of the group visible or not.
    ! Not a port: (ediff-overlay-put overl 'invisible INVISIBLE).
    subroutine set_session_invisible(self, session_number, invisible)
        class(fortmacs_diff_session_group_type), intent(inout), target :: self
        integer, intent(in) :: session_number
        logical, intent(in) :: invisible
        type(status_type) :: st

        call overlay_put(self%sessions(session_number)%overlay, "invisible", &
                         value_type(invisible), st)
        self%sessions(session_number)%invisible = invisible
    end subroutine set_session_invisible

    !> Update the activity markers and status of the sessions of the group
    !> in its buffer, hiding and showing them by their status.  Port of
    !> ediff-mult.el:ediff-update-markers-in-dir-meta-buffer.
    recursive subroutine update_markers(self, status)
        class(fortmacs_diff_session_group_type), intent(inout), target :: self
        type(status_type), intent(out) :: status
        integer(int64) :: saved_point
        integer :: n, m
        integer :: group_index

        call self%get_index(group_index, status)
        if (.not. status%ok()) return
        saved_point = point()
        call goto_character(point_minimum())
        call fortmacs_diff_session_groups(group_index)%next_item(status)
        if (.not. status%ok()) return

        do while (.not. is_beginning_of_buffer())
            n = fortmacs_diff_session_groups(group_index)%get_visible_session_at(point())
            m = fortmacs_diff_session_groups(group_index)%get_session_at(point())

            if (n > 0) then
                associate (s => fortmacs_diff_session_groups(group_index)%sessions(n))
                    if (s%status == FORTMACS_DIFF_SESSION_STATUS_HIDDEN) then
                        ! Do hiding.
                        call fortmacs_diff_session_groups(group_index)%set_session_invisible(n, .true.)
                    else
                        call fortmacs_diff_session_groups(group_index)%replace_session_activity_marker(n, &
                                                                                       get_fortmacs_diff_session_activity_marker(s))
                        call fortmacs_diff_session_groups(group_index)%replace_session_status(n, s%status)
                    end if
                end associate
            else if (m > 0) then
                ! Do unhiding.
                if (fortmacs_diff_session_groups(group_index)%sessions(m)%status == FORTMACS_DIFF_SESSION_STATUS_HIDE) then
                    call fortmacs_diff_session_groups(group_index)%set_session_invisible(m, .false.)
                end if
            end if

            ! Advance to the next item.
            call fortmacs_diff_session_groups(group_index)%next_item(status)
            if (.not. status%ok()) return
        end do

        call restore_buffer_modified(.false.)
        call goto_character(saved_point)
    end subroutine update_markers

    !> Update the markers of session SESSION_NUMBER of the group in its
    !> buffer, then move to the next item.  Port of
    !> ediff-mult.el:ediff-update-session-marker-in-dir-meta-buffer.
    recursive subroutine update_session_marker(self, session_number, status)
        class(fortmacs_diff_session_group_type), intent(inout), target :: self
        integer, intent(in) :: session_number
        type(status_type), intent(out) :: status
        character(len=12) :: number
        integer :: group_index

        call self%get_index(group_index, status)
        if (.not. status%ok()) return

        if (session_number < 1 .or. session_number > size(fortmacs_diff_session_groups(group_index)%sessions)) then
            write (number, "(i0)") session_number
            call set_error(status, GENERIC_ERROR_CODE, &
                           "Bug in update_session_marker: no overlay with given number "// &
                           trim(number))
            return
        end if

        associate (s => fortmacs_diff_session_groups(group_index)%sessions(session_number))
            if (s%overlay%index == 0) then
                write (number, "(i0)") session_number
                call set_error(status, GENERIC_ERROR_CODE, &
                               "Bug in update_session_marker: no overlay with given number "// &
                               trim(number))
                return
            end if

            call goto_character(overlay_start(s%overlay))
            call fortmacs_diff_session_groups(group_index)%replace_session_activity_marker(session_number, &
                                                                                       get_fortmacs_diff_session_activity_marker(s))
            call fortmacs_diff_session_groups(group_index)%replace_session_status(session_number, s%status)
        end associate

        call fortmacs_diff_session_groups(group_index)%next_item(status)
    end subroutine update_session_marker

    !> Replace the activity marker of session SESSION_NUMBER of the group in the buffer by
    !> MARKER.  Port of
    !> ediff-mult.el:ediff-replace-session-activity-marker-in-meta-buffer.
    subroutine replace_session_activity_marker(self, session_number, marker)
        class(fortmacs_diff_session_group_type), intent(inout), target :: self
        integer, intent(in) :: session_number
        character, intent(in) :: marker

        call replace_character(overlay_start(self%sessions(session_number)%overlay), marker)
    end subroutine replace_session_activity_marker

    !> Replace the status of session SESSION_NUMBER of the group in the buffer by STATUS
    !> (the second character of the session's record).  Port of
    !> ediff-mult.el:ediff-replace-session-status-in-meta-buffer.
    subroutine replace_session_status(self, session_number, status)
        class(fortmacs_diff_session_group_type), intent(inout), target :: self
        integer, intent(in) :: session_number
        character, intent(in) :: status

        call replace_character(overlay_start(self%sessions(session_number)%overlay) + 1, &
                               status)
    end subroutine replace_session_status

    ! Make the character at POSITION C, unless it is C already, leaving point
    ! at POSITION.  Not a port.
    subroutine replace_character(position, c)
        integer(int64), intent(in) :: position
        character, intent(in) :: c
        type(status_type) :: st
        integer(int32) :: old
        logical :: found, read_only

        call goto_character(position)
        call character_after(old, found)
        ! If marker shown in buffer is the same as new-marker, do nothing.
        if (found .and. old == iachar(c)) return
        read_only = buffers(current)%read_only
        buffers(current)%read_only = .false.
        call insert(c, st)
        call delete_character(1_int64, st)
        buffers(current)%read_only = read_only
        call restore_buffer_modified(.false.)
        call goto_character(position)
    end subroutine replace_character

    ! ------------------------------------------------------------------
    ! Records and motion
    ! ------------------------------------------------------------------

    ! The session of the group whose record is at POSITION (hidden ones too), 0
    ! if none.  Port of ediff-mult.el:ediff-get-meta-overlay-at-pos (with
    ! ediff-get-session-number-at-pos).
    integer function get_session_at(self, position) result(n)
        class(fortmacs_diff_session_group_type), intent(in) :: self
        integer(int64), intent(in) :: position
        type(handle_type), allocatable :: overlays_here(:)
        integer :: i, j

        n = 0
        call overlays_at(position, overlays_here)

        do i = 1, size(overlays_here)
            do j = 1, size(self%sessions)
                if (same_handle(self%sessions(j)%overlay, overlays_here(i))) then
                    n = j
                    return
                end if
            end do
        end do
    end function get_session_at

    ! The session of the group whose record is at POSITION and is not hidden, 0
    ! if none.  Port of ediff-mult.el:ediff-get-meta-info (in a group).
    integer function get_visible_session_at(self, position) result(n)
        class(fortmacs_diff_session_group_type), intent(in) :: self
        integer(int64), intent(in) :: position
        type(handle_type), allocatable :: overlays_here(:)
        integer :: i, j

        n = 0
        call overlays_at(position, overlays_here)

        do i = 1, size(overlays_here)
            do j = 1, size(self%sessions)
                if (self%sessions(j)%invisible) cycle

                if (same_handle(self%sessions(j)%overlay, overlays_here(i))) then
                    n = j
                    return
                end if
            end do
        end do
    end function get_visible_session_at

    ! The record of the registry (the group) at POSITION, 0 if none.  Port of
    ! ediff-mult.el:ediff-get-meta-info (in the registry).
    integer function get_registry_record_at(self, position) result(r)
        class(fortmacs_diff_session_group_type), intent(in) :: self
        integer(int64), intent(in) :: position
        type(handle_type), allocatable :: overlays_here(:)
        integer :: i, j

        r = 0
        if (.not. allocated(self%registry_records)) return
        call overlays_at(position, overlays_here)

        do i = 1, size(overlays_here)
            do j = 1, size(self%registry_records)
                if (same_handle(self%registry_records(j)%overlay, overlays_here(i))) then
                    r = j
                    return
                end if
            end do
        end do
    end function get_registry_record_at

    ! The overlay of the record at POSITION of the group (hidden ones too), the
    ! null handle if none.  Port of
    ! ediff-mult.el:ediff-get-meta-overlay-at-pos.
    function get_record_overlay_at(self, position) result(overlay)
        class(fortmacs_diff_session_group_type), intent(in) :: self
        integer(int64), intent(in) :: position
        type(handle_type) :: overlay
        integer :: j

        overlay = null_handle()

        if (self%is_registry()) then
            j = self%get_registry_record_at(position)
            if (j > 0) overlay = self%registry_records(j)%overlay
        else
            j = self%get_session_at(position)
            if (j > 0) overlay = self%sessions(j)%overlay
        end if
    end function get_record_overlay_at

    ! Whether the record at POSITION of the group is hidden.  Not a port:
    ! (overlay-get overl 'invisible).
    logical function is_record_invisible(self, position)
        class(fortmacs_diff_session_group_type), intent(in) :: self
        integer(int64), intent(in) :: position
        integer :: j

        is_record_invisible = .false.
        if (self%is_registry()) return
        j = self%get_session_at(position)
        if (j > 0) is_record_invisible = self%sessions(j)%invisible
    end function is_record_invisible

    !> The session of the group in buffer BUFFER whose record is at POSITION in
    !> N (the registry's record in TARGET, its control or group buffer).
    !> Without NOERROR, no record there updates the registry and gives a
    !> user error.  Port of ediff-mult.el:ediff-get-meta-info.
    recursive subroutine get_fortmacs_diff_meta_information(buffer, position, n, target, status, noerror)
        type(handle_type), intent(in) :: buffer
        integer(int64), intent(in) :: position
        integer, intent(out) :: n
        type(handle_type), intent(out) :: target
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: noerror
        type(handle_type) :: old
        type(status_type) :: st
        integer :: group_index, r

        status = status_type()
        n = 0
        target = null_handle()
        group_index = fortmacs_diff_group_index(buffer)

        if (group_index > 0) then
            old = current_buffer()
            call set_buffer(buffer, status)
            if (.not. status%ok()) return

            if (fortmacs_diff_session_groups(group_index)%is_registry()) then
                r = fortmacs_diff_session_groups(group_index)%get_registry_record_at(position)
                if (r > 0) target = fortmacs_diff_session_groups(group_index)%registry_records(r)%buffer
            else
                n = fortmacs_diff_session_groups(group_index)%get_visible_session_at(position)
            end if

            if (is_buffer_live(old)) call set_buffer(old, st)
        end if

        if (n > 0 .or. target%index /= 0) return

        if (present(noerror)) then
            if (noerror) return
        end if

        call update_fortmacs_diff_registry(st)
        call set_error(status, USER_ERROR_CODE, "No session info in this line")
    end subroutine get_fortmacs_diff_meta_information

    !> The location of the next record after POSITION in the group's buffer (the
    !> start of the buffer at its end).  Port of
    !> ediff-mult.el:ediff-next-meta-overlay-start.
    integer(int64) function get_next_overlay_start(self, position) result(next)
        class(fortmacs_diff_session_group_type), intent(in) :: self
        integer(int64), intent(in) :: position
        type(handle_type) :: overlay

        if (is_end_of_buffer()) then
            call goto_character(point_minimum())
            next = point_minimum()
            return
        end if

        overlay = self%get_record_overlay_at(position)

        if (overlay%index /= 0) then
            ! Note: end of current overlay is the beginning of the next one.
            next = overlay_end(overlay)
        else
            next = next_overlay_change(position)
        end if
    end function get_next_overlay_start

    !> The location of the previous record before POSITION in the group's buffer
    !> (the end of the buffer at its start).  Port of
    !> ediff-mult.el:ediff-previous-meta-overlay-start.
    integer(int64) function get_previous_overlay_start(self, position) result(previous)
        class(fortmacs_diff_session_group_type), intent(in) :: self
        integer(int64), intent(in) :: position
        type(handle_type) :: overlay
        type(handle_type), allocatable :: overlays_here(:)

        if (is_beginning_of_buffer()) then
            call goto_character(point_maximum())
            previous = point_maximum()
            return
        end if

        previous = position
        overlay = self%get_record_overlay_at(position)
        if (overlay%index /= 0) previous = overlay_start(overlay)
        ! To get to the beginning of prev overlay.
        if (.not. is_beginning_of_buffer()) previous = previous - 1
        previous = previous_overlay_change(previous)
        ! If we are not over an overlay after subtracting 1, it means we
        ! are in the description area preceding session records.  In this
        ! case, goto the top of the registry buffer.
        call overlays_at(previous, overlays_here)
        if (size(overlays_here) == 0) previous = point_minimum()
    end function get_previous_overlay_start

    !> Move to the next record of the group.  Port of
    !> ediff-mult.el:ediff-next-meta-item1.
    recursive subroutine next_item(self, status)
        class(fortmacs_diff_session_group_type), intent(inout), target :: self
        type(status_type), intent(out) :: status
        integer(int64) :: position
        integer :: group_index

        call self%get_index(group_index, status)
        if (.not. status%ok()) return
        position = fortmacs_diff_session_groups(group_index)%get_next_overlay_start(point())
        call goto_character(position)
        call fortmacs_diff_session_groups(group_index)%skip_to_item(position, status)
    end subroutine next_item

    !> Move to the previous record of the group.  Port of
    !> ediff-mult.el:ediff-previous-meta-item1.
    recursive subroutine previous_item(self, status)
        class(fortmacs_diff_session_group_type), intent(inout), target :: self
        type(status_type), intent(out) :: status
        integer(int64) :: position
        integer :: group_index

        call self%get_index(group_index, status)
        if (.not. status%ok()) return
        position = fortmacs_diff_session_groups(group_index)%get_previous_overlay_start(point())
        call goto_character(position)
        call fortmacs_diff_session_groups(group_index)%skip_to_item(position, status)
    end subroutine previous_item

    ! Move point from the start of the record at POSITION of the group to its
    ! session's name (the registry) or past its markers.  Not a port: the
    ! end of ediff-next-meta-item1.
    recursive subroutine skip_to_item(self, position, status)
        class(fortmacs_diff_session_group_type), intent(inout), target :: self
        integer(int64), intent(in) :: position
        type(status_type), intent(out) :: status
        integer(int64) :: moved, found
        integer :: group_index

        call self%get_index(group_index, status)
        if (.not. status%ok()) return

        if (fortmacs_diff_session_groups(group_index)%is_registry()) then
            if (fortmacs_diff_session_groups(group_index)%get_registry_record_at(position) > 0) then
                call search_forward("*Ediff", found, status, noerror=SEARCH_NOERROR)
                if (.not. status%ok()) return
                if (found > 0) moved = skip_characters_backward("a-zA-Z*", status)
            end if
        else
            moved = skip_characters_forward("-+?H* "//TAB//"0-9", status)
            if (.not. status%ok()) return
            if (moved > 0) call backward_character(status)
        end if
    end subroutine skip_to_item

    ! Move COUNT items forward (FORWARD) or backward in the current group
    ! buffer, skipping the hidden ones.  Not a port: the body of
    ! ediff-next-meta-item and ediff-previous-meta-item.
    recursive subroutine move_items(count, forward, status)
        integer(int64), intent(in) :: count
        logical, intent(in) :: forward
        type(status_type), intent(out) :: status
        integer(int64) :: left
        integer :: group_index

        call check_meta_buffer(group_index, status)
        if (.not. status%ok()) return
        left = count

        do while (left > 0)
            left = left - 1
            call step()
            if (.not. status%ok()) return

            ! Skip invisible ones.
            do while (fortmacs_diff_session_groups(group_index)%is_record_invisible(point()))
                call step()
                if (.not. status%ok()) return
            end do
        end do

    contains

        ! One item.
        recursive subroutine step()
            if (forward) then
                call fortmacs_diff_session_groups(group_index)%next_item(status)
            else
                call fortmacs_diff_session_groups(group_index)%previous_item(status)
            end if
        end subroutine step
    end subroutine move_items

    ! The numeric prefix argument, 1 without one (the count the commands
    ! read).  Not a port.
    integer(int64) function count_argument()
        count_argument = 1
        if (current_prefix_argument%kind /= PREFIX_NONE) count_argument = prefix_numeric_value(current_prefix_argument)
    end function count_argument

    !> Move to the next item in Ediff registry or session group buffer.
    !> Moves in circular fashion.
    !>
    !> With numeric prefix arg COUNT, skip this many items.  Port of
    !> ediff-mult.el:ediff-next-meta-item.
    recursive subroutine next_fortmacs_diff_meta_item(status)
        type(status_type), intent(out) :: status

        call move_items(count_argument(), .true., status)
    end subroutine next_fortmacs_diff_meta_item

    !> Move to the previous item in Ediff registry or session group buffer.
    !> Moves in circular fashion.
    !>
    !> With numeric prefix arg COUNT, skip this many items.  Port of
    !> ediff-mult.el:ediff-previous-meta-item.
    recursive subroutine previous_fortmacs_diff_meta_item(status)
        type(status_type), intent(out) :: status

        call move_items(count_argument(), .false., status)
    end subroutine previous_fortmacs_diff_meta_item

    !> Toggle showing tediously verbose help in meta buffers.  Port of
    !> ediff-mult.el:ediff-toggle-verbose-help-meta-buffer.
    recursive subroutine toggle_fortmacs_diff_verbose_help_meta_buffer(status)
        type(status_type), intent(out) :: status
        integer :: group_index

        call check_meta_buffer(group_index, status)
        if (.not. status%ok()) return
        fortmacs_diff_session_groups(group_index)%verbose_help_enabled = &
            .not. fortmacs_diff_session_groups(group_index)%verbose_help_enabled
        call update_fortmacs_diff_meta_buffer(current_buffer(), status, must_redraw=.true.)
    end subroutine toggle_fortmacs_diff_verbose_help_meta_buffer

    !> Toggle truncation of long file names in session group buffers.
    !> Set `fortmacs_diff_meta_truncate_filenames' variable if you want to change
    !> the default behavior.  Port of
    !> ediff-mult.el:ediff-toggle-filename-truncation.
    recursive subroutine toggle_fortmacs_diff_filename_truncation(status)
        type(status_type), intent(out) :: status

        fortmacs_diff_meta_truncate_filenames = .not. fortmacs_diff_meta_truncate_filenames
        call update_fortmacs_diff_meta_buffer(current_buffer(), status, must_redraw=.true.)
    end subroutine toggle_fortmacs_diff_filename_truncation

    ! ------------------------------------------------------------------
    ! Directory differences
    ! ------------------------------------------------------------------

    !> Draw the buffer of the files only some directories of the group have
    !> (`fortmacs_diff_directory_diffs_buffer'), named BUFFER_NAME (a new
    !> "*Ediff File Group Differences*" if absent).  Port of
    !> ediff-mult.el:ediff-draw-dir-diffs.
    recursive subroutine draw_directory_diffs(self, status, buffer_name)
        class(fortmacs_diff_session_group_type), intent(inout), target :: self
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: buffer_name
        character(len=*), parameter :: UNDERLINE = repeat("-", 26)
        character(len=:), allocatable :: name, directory1, directory2, directory3
        type(handle_type) :: old, buffer
        type(status_type) :: st
        integer(int64) :: saved_point
        integer :: j
        integer :: group_index

        call self%get_index(group_index, status)
        if (.not. status%ok()) return

        if (.not. fortmacs_diff_session_groups(group_index)%has_directory_differences) then
            call set_error(status, USER_ERROR_CODE, "Lost difference info on these directories")
            return
        end if

        if (present(buffer_name)) then
            name = buffer_name
        else
            name = fortmacs_diff_unique_buffer_name("*Ediff File Group Differences", "*")
        end if

        directory1 = abbreviate_file_name(fortmacs_diff_session_groups(group_index)%directory_a)
        directory2 = abbreviate_file_name(fortmacs_diff_session_groups(group_index)%directory_b)

        if (allocated(fortmacs_diff_session_groups(group_index)%directory_c)) then
            directory3 = abbreviate_file_name(fortmacs_diff_session_groups(group_index)%directory_c)
        end if

        ! A new buffer has no `ediff-dir-diffs-buffer' of its own yet.
        if (.not. is_buffer_live(get_buffer(name))) then
            fortmacs_diff_session_groups(group_index)%directory_diffs_closed_on_bury = .false.
        end if

        call get_or_make_buffer(name, buffer, status)
        if (.not. status%ok()) return
        fortmacs_diff_session_groups(group_index)%directory_diffs_buffer = buffer
        old = current_buffer()
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        saved_point = point()
        call use_local_map(fortmacs_diff_directory_diffs_buffer_map, status)

        if (.not. status%ok()) then
            if (is_buffer_live(old)) call set_buffer(old, st)
            return
        end if

        buffers(current)%read_only = .false.
        call erase_buffer(status)

        if (.not. status%ok()) then
            if (is_buffer_live(old)) call set_buffer(old, st)
            return
        end if

        associate (g => fortmacs_diff_session_groups(group_index))
            if (allocated(g%directory_diff_records)) then
                do j = 1, size(g%directory_diff_records)
                    call delete_overlay(g%directory_diff_records(j)%overlay)
                end do

                deallocate (g%directory_diff_records)
            end if

            allocate (g%directory_diff_records(0))
            call insert_text(TAB//TAB//"*** Directory Differences ***"//LF)
            call insert_text(LF//"Useful commands:"//LF// &
                             "  C,mouse-2: over file name -- copy this file to directory that doesn't have it"//LF// &
                             "          q: hide this buffer"//LF// &
                             "      n,SPC: next line"//LF// &
                             "      p,DEL: previous line"//LF//LF)
            call insert_text(LF//"*** Directory A: "//directory1//LF)
            call insert_text("*** Directory B: "//directory2//LF)
            if (allocated(directory3)) call insert_text("*** Directory C: "//directory3//LF)

            if (allocated(g%regular_expression)) then
                if (len(g%regular_expression) > 0) call insert_text("*** Filter-through regular expression: " &
                                                                    //g%regular_expression//LF)
            end if

            call insert_text(LF)
            call insert_text(LF//pad("Directory A", 27)//pad("Directory B", 26))

            if (allocated(directory3)) then
                call insert_text(" "//pad("Directory C", 25)//LF)
            else
                call insert_text(LF)
            end if

            call insert_text(UNDERLINE//UNDERLINE)

            if (allocated(directory3)) then
                call insert_text(UNDERLINE//LF//LF)
            else
                call insert_text(LF//LF)
            end if

            if (size(g%directory_differences) == 0) call insert_text(LF//TAB//"***  No differences  ***"//LF)

            do j = 1, size(g%directory_differences)
                call column(g%directory_differences(j), FORTMACS_DIFF_MEMBERSHIP_CODE1, directory1, "", 27)
                call column(g%directory_differences(j), FORTMACS_DIFF_MEMBERSHIP_CODE2, directory2, "", 26)
                if (allocated(directory3)) call column(g%directory_differences(j), FORTMACS_DIFF_MEMBERSHIP_CODE3, directory3, &
                                                       " ", 25)
                if (.not. status%ok()) exit
                call insert_text(LF)
            end do

            buffers(current)%read_only = .true.
            call restore_buffer_modified(.false.)
            call goto_character(saved_point)
        end associate

        if (is_buffer_live(old)) call set_buffer(old, st)

    contains

        ! Insert the column of DIRECTORY (code CODE) for file DIFF: its name
        ! (truncated, a directory with a slash) with a record, or ---;
        ! after LEAD, padded to WIDTH.
        subroutine column(diff, code, directory, lead, width)
            type(fortmacs_diff_directory_difference_type), intent(in) :: diff
            integer, intent(in) :: code, width
            character(len=*), intent(in) :: directory, lead
            type(fortmacs_diff_directory_diff_record_type) :: record
            type(fortmacs_diff_directory_diff_record_type), allocatable :: grown(:)
            integer(int64) :: beginning
            integer :: n

            if (mod(diff%membership, code) /= 0) then
                call insert_text(lead//pad("---", width))
                return
            end if

            call insert_text(lead)
            beginning = point()

            if (is_file_directory(directory//diff%file)) then
                call insert_text(pad(truncate_left(abbreviate_file_name(file_name_as_directory(diff%file)), 24), width))
            else
                call insert_text(pad(truncate_left(abbreviate_file_name(diff%file), 24), width))
            end if

            ! The meta info in the dir-diff-buffer: the file and its name
            ! in DIRECTORY.
            call set_fortmacs_diff_meta_overlay(beginning, point(), record%overlay, status)
            if (.not. status%ok()) return
            record%file = diff%file
            record%full = directory//diff%file
            n = size(fortmacs_diff_session_groups(group_index)%directory_diff_records)
            allocate (grown(n + 1))
            grown(1:n) = fortmacs_diff_session_groups(group_index)%directory_diff_records
            grown(n + 1) = record
            call move_alloc(grown, fortmacs_diff_session_groups(group_index)%directory_diff_records)
        end subroutine column
    end subroutine draw_directory_diffs

    ! The group whose directory difference buffer is the current buffer,
    ! 0 if none.  Not a port: the buffer's `fortmacs_diff_meta_buffer'.
    integer function directory_diffs_group() result(group_index)
        integer :: j

        group_index = 0
        if (.not. allocated(fortmacs_diff_session_groups)) return

        do j = 1, size(fortmacs_diff_session_groups)
            if (.not. is_buffer_live(fortmacs_diff_session_groups(j)%buffer)) cycle

            if (same_handle(fortmacs_diff_session_groups(j)%directory_diffs_buffer, current_buffer())) then
                group_index = j
                return
            end if
        end do
    end function directory_diffs_group

    !> Bury the directory difference buffer.  Display the meta buffer
    !> instead.  Port of ediff-mult.el:ediff-bury-dir-diffs-buffer (the
    !> buffer is closed once C has redrawn it, as upstream's own
    !> `ediff-dir-diffs-buffer' of the buffer is set only then).
    recursive subroutine bury_fortmacs_diff_directory_diffs_buffer(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: meta, window
        integer :: group_index

        status = status_type()
        group_index = directory_diffs_group()
        if (group_index == 0) return

        if (fortmacs_diff_session_groups(group_index)%directory_diffs_closed_on_bury) then
            fortmacs_diff_session_groups(group_index)%directory_diffs_closed_on_bury = .false.
            call close_fortmacs_diff_buffer_carefully(fortmacs_diff_session_groups(group_index)%directory_diffs_buffer, status)
            if (.not. status%ok()) return
        end if

        meta = fortmacs_diff_session_groups(group_index)%buffer
        window = get_fortmacs_diff_visible_buffer_window(meta)

        if (window%index /= 0) then
            call select_window(window, status)
        else
            call set_window_buffer(selected_frame%selected_window, meta, status)
        end if
    end subroutine bury_fortmacs_diff_directory_diffs_buffer

    !> Display differences among the directories involved in session group.
    !> Port of ediff-mult.el:ediff-show-dir-diffs.
    recursive subroutine show_fortmacs_diff_directory_diffs(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer
        integer :: group_index

        call check_meta_buffer(group_index, status)
        if (.not. status%ok()) return

        if (.not. is_buffer_live(fortmacs_diff_session_groups(group_index)%directory_diffs_buffer)) then
            call fortmacs_diff_session_groups(group_index)%draw_directory_diffs(status)
            if (.not. status%ok()) return
        end if

        buffer = fortmacs_diff_session_groups(group_index)%directory_diffs_buffer
        call other_window(1_int64, status)
        if (.not. status%ok()) return
        call set_window_buffer(selected_frame%selected_window, buffer, status)
        if (.not. status%ok()) return
        call goto_character(point_minimum())
    end subroutine show_fortmacs_diff_directory_diffs

    !> Copy file described at point to directories where this file is
    !> missing.  Port of ediff-mult.el:ediff-dir-diff-copy-file.
    recursive subroutine copy_fortmacs_diff_directory_diff_file(status)
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: overlays_here(:)
        type(fortmacs_diff_group_session_type) :: s
        character(len=:), allocatable :: file_tail, file_absolute
        character(len=:), allocatable :: other1, other2, other3, this_name
        logical :: has3
        logical :: answer
        integer :: group_index, r, i, j

        status = status_type()
        group_index = directory_diffs_group()
        r = 0

        if (group_index > 0) then
            call overlays_at(point(), overlays_here)

            do i = 1, size(overlays_here)
                do j = 1, size(fortmacs_diff_session_groups(group_index)%directory_diff_records)
                    if (same_handle(fortmacs_diff_session_groups(group_index)%directory_diff_records(j)%overlay, &
                                    overlays_here(i))) r = j
                end do
            end do
        end if

        if (r == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "No file suitable for copying described at this location")
            return
        end if

        file_tail = fortmacs_diff_session_groups(group_index)%directory_diff_records(r)%file
        file_absolute = fortmacs_diff_session_groups(group_index)%directory_diff_records(r)%full
        has3 = allocated(fortmacs_diff_session_groups(group_index)%directory_c)
        other1 = abbreviate_file_name(fortmacs_diff_session_groups(group_index)%directory_a)//file_tail
        other2 = abbreviate_file_name(fortmacs_diff_session_groups(group_index)%directory_b)//file_tail
        other3 = ""
        if (has3) other3 = abbreviate_file_name(fortmacs_diff_session_groups(group_index)%directory_c)//file_tail
        this_name = name_of(current_buffer())
        ! Copy file to directories where it doesn't exist, update
        ! ediff-dir-difference-list and redisplay.
        call offer(other1, FORTMACS_DIFF_MEMBERSHIP_CODE1)
        if (.not. status%ok()) return
        call offer(other2, FORTMACS_DIFF_MEMBERSHIP_CODE2)
        if (.not. status%ok()) return

        if (has3) then
            call offer(other3, FORTMACS_DIFF_MEMBERSHIP_CODE3)
            if (.not. status%ok()) return
        end if

        if (file_exists(other1) .and. file_exists(other2) .and. (.not. has3 .or. file_exists(other3))) then
            ! Update ediff-meta-list by direct modification.
            call expand_file_name(other1, s%files(1)%name, status)
            if (.not. status%ok()) return
            call expand_file_name(other2, s%files(2)%name, status)
            if (.not. status%ok()) return

            if (has3) then
                call expand_file_name(other3, s%files(3)%name, status)
                if (.not. status%ok()) return
            end if

            call append_session(fortmacs_diff_session_groups(group_index)%sessions, s)
        end if

        call update_fortmacs_diff_meta_buffer(fortmacs_diff_session_groups(group_index)%buffer, status, must_redraw=.true.)

    contains

        ! Offer to copy the file to OTHER_FILE (membership code CODE) if it
        ! does not exist.
        recursive subroutine offer(other_file, code)
            character(len=*), intent(in) :: other_file
            integer, intent(in) :: code

            if (file_exists(other_file)) return
            call read_y_or_n("Copy "//file_absolute//" to "//other_file//"? ", answer, status)
            if (.not. status%ok()) return
            if (answer) call copy_one(other_file, code)
        end subroutine offer

        ! Copy the file to OTHER_FILE and update its membership (code
        ! CODE), dropping its record once all directories have it.
        recursive subroutine copy_one(other_file, code)
            character(len=*), intent(in) :: other_file
            integer, intent(in) :: code
            character(len=:), allocatable :: from, to
            logical, allocatable :: keep(:)
            integer :: f, new_code

            associate (g => fortmacs_diff_session_groups(group_index))
                f = 0

                do i = 1, size(g%directory_differences)
                    if (g%directory_differences(i)%file == file_tail) f = i
                end do

                if (f == 0) return
                new_code = g%directory_differences(f)%membership*code
                call expand_file_name(file_absolute, from, status)
                if (.not. status%ok()) return
                call expand_file_name(other_file, to, status)
                if (.not. status%ok()) return
                call copy_file(from, to, status)
                if (.not. status%ok()) return
                g%directory_differences(f)%membership = new_code
                call fortmacs_diff_session_groups(group_index)%draw_directory_diffs(status, this_name)
                if (.not. status%ok()) return
                ! Drawn from the buffer itself, which now knows itself.
                g%directory_diffs_closed_on_bury = .true.

                ! If file is in all three dirs or in two dirs and only two
                ! dirs are involved, delete this file's record.
                if (new_code == FORTMACS_DIFF_PRODUCT_OF_MEMBERSHIP_CODES .or. (new_code > FORTMACS_DIFF_MEMBERSHIP_CODE3 &
                                                                                .and. .not. has3)) then
                    allocate (keep(size(g%directory_differences)))
                    keep = .true.
                    keep(f) = .false.
                    g%directory_differences = pack(g%directory_differences, keep)
                end if
            end associate
        end subroutine copy_one
    end subroutine copy_fortmacs_diff_directory_diff_file

    !> Go to the parent session group buffer.  Port of
    !> ediff-mult.el:ediff-up-meta-hierarchy.
    recursive subroutine up_fortmacs_diff_meta_hierarchy(status)
        type(status_type), intent(out) :: status
        integer :: group_index

        call check_meta_buffer(group_index, status)
        if (.not. status%ok()) return

        if (is_buffer_live(fortmacs_diff_session_groups(group_index)%parent_meta_buffer)) then
            call show_fortmacs_diff_meta_buffer(status, fortmacs_diff_session_groups(group_index)%parent_meta_buffer, &
                                                session_number=fortmacs_diff_session_groups(group_index)%meta_session_number)
        else
            call set_error(status, USER_ERROR_CODE, "This session group has no parent")
        end if
    end subroutine up_fortmacs_diff_meta_hierarchy

    ! ------------------------------------------------------------------
    ! The registry
    ! ------------------------------------------------------------------

    !> Redraw the registry buffer: a record for each live session and
    !> group of `fortmacs_diff_session_registry', from which dead buffers are
    !> purged.  Port of ediff-mult.el:ediff-redraw-registry-buffer.
    recursive subroutine redraw_fortmacs_diff_registry_buffer(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: old, element
        type(fortmacs_diff_registry_record_type) :: record
        type(status_type) :: st
        character(len=:), allocatable :: name_a, name_b, name_c, total, current_diff, directory_b, directory_c_name
        character(len=12) :: digits
        integer(int64) :: saved_point, beginning_of_record
        integer :: registry_index, j, group_index, session_index

        status = status_type()
        registry_index = fortmacs_diff_group_index(fortmacs_diff_registry_buffer)
        if (registry_index == 0) return
        old = current_buffer()
        call set_buffer(fortmacs_diff_registry_buffer, status)
        if (.not. status%ok()) return
        saved_point = point()
        buffers(current)%read_only = .false.
        call erase_buffer(status)

        if (.not. status%ok()) then
            if (is_buffer_live(old)) call set_buffer(old, st)
            return
        end if

        ! Delete phony overlays that used to represent sessions before the
        ! buffer was redrawn.
        if (allocated(fortmacs_diff_session_groups(registry_index)%registry_records)) then
            do j = 1, size(fortmacs_diff_session_groups(registry_index)%registry_records)
                call delete_overlay(fortmacs_diff_session_groups(registry_index)%registry_records(j)%overlay)
            end do

            deallocate (fortmacs_diff_session_groups(registry_index)%registry_records)
        end if

        allocate (fortmacs_diff_session_groups(registry_index)%registry_records(0))
        call insert_text(substitute_command_keys(REGISTRY_MESSAGE))
        ! Purge registry list from dead buffers.
        fortmacs_diff_session_registry = registry_list()
        fortmacs_diff_session_registry = pack(fortmacs_diff_session_registry, [(is_buffer_live(fortmacs_diff_session_registry(j)), &
                                                                                j=1, &
                                                                                size(fortmacs_diff_session_registry))])
        if (size(fortmacs_diff_session_registry) == 0) call insert_text("       ******* No active Ediff sessions *******"//LF)

        do j = 1, size(fortmacs_diff_session_registry)
            element = fortmacs_diff_session_registry(j)
            group_index = fortmacs_diff_group_index(element)

            if (group_index > 0) then
                if (fortmacs_diff_session_groups(group_index)%is_registry()) cycle
                beginning_of_record = point()
                call insert_text("  *group*"//TAB//name_of(element)//": "// &
                                 abbreviate_fortmacs_diff_job_name(fortmacs_diff_session_groups(group_index)%job_name)//LF)
                directory_b = ""

                if (allocated(fortmacs_diff_session_groups(group_index)%directory_b)) then
                    directory_b = fortmacs_diff_session_groups(group_index)%directory_b
                end if

                directory_c_name = ""

                if (allocated(fortmacs_diff_session_groups(group_index)%directory_c)) then
                    directory_c_name = fortmacs_diff_session_groups(group_index)%directory_c
                end if

                call insert_text(TAB//TAB//"   "//abbreviate_file_name(fortmacs_diff_session_groups(group_index)%directory_a)// &
                                 "   "//abbreviate_file_name(directory_b)//"   "//abbreviate_file_name(directory_c_name)//LF)
            else
                session_index = fortmacs_diff_session_index(element)
                if (session_index == 0) cycle

                associate (e => fortmacs_diff_sessions(session_index))
                    name_a = killed_or_name(e%buffers(FORTMACS_DIFF_BUFFER_TYPE_A))
                    name_b = killed_or_name(e%buffers(FORTMACS_DIFF_BUFFER_TYPE_B))

                    if (.not. e%three_way_job) then
                        name_c = ""
                    else
                        name_c = killed_or_name(e%buffers(FORTMACS_DIFF_BUFFER_TYPE_C))
                    end if

                    write (digits, "(i0)") e%number_of_differences
                    total = pad(trim(digits), 4)

                    if (e%current_difference == -1) then
                        current_diff = "   _"
                    else if (e%current_difference == e%number_of_differences) then
                        current_diff = "   $"
                    else
                        current_diff = right(int(e%current_difference + 1, int64), 4)
                    end if

                    ! Back in the meta buffer.
                    beginning_of_record = point()
                    call insert_text(current_diff//"/"//total//TAB//name_of(element)//": "//abbreviate_fortmacs_diff_job_name( &
                                     e%job_name))
                    call insert_text(LF//TAB//TAB//"   "//name_a//"   "//name_b//"   "//name_c//LF)
                end associate
            end if

            call set_fortmacs_diff_meta_overlay(beginning_of_record, point(), record%overlay, status)
            if (.not. status%ok()) return
            record%buffer = element
            fortmacs_diff_session_groups(registry_index)%registry_records = &
                [fortmacs_diff_session_groups(registry_index)%registry_records, record]
        end do

        call restore_buffer_modified(.false.)
        buffers(current)%read_only = .true.
        call goto_character(saved_point)
        if (is_buffer_live(old)) call set_buffer(old, st)

    contains

        ! The name of buffer H, or !!!killed buffer!!!.
        function killed_or_name(h) result(name)
            type(handle_type), intent(in) :: h
            character(len=:), allocatable :: name

            if (is_buffer_live(h)) then
                name = name_of(h)
            else
                name = "!!!killed buffer!!!"
            end if
        end function killed_or_name
    end subroutine redraw_fortmacs_diff_registry_buffer

    !> Redraw the registry, making it if there is none.  Port of
    !> ediff-mult.el:ediff-update-registry.
    recursive subroutine update_fortmacs_diff_registry(status)
        type(status_type), intent(out) :: status
        type(fortmacs_diff_session_group_type) :: group
        type(handle_type) :: buffer

        if (fortmacs_diff_group_index(fortmacs_diff_registry_buffer) > 0) then
            call redraw_fortmacs_diff_registry_buffer(status)
        else
            call prepare_fortmacs_diff_meta_buffer(group, "*Ediff Registry", .true., buffer, status)
        end if
    end subroutine update_fortmacs_diff_registry

    !> Display Ediff's registry.  Port of ediff-mult.el:ediff-show-registry
    !> (also `eregistry'; on a terminal frame upstream's choice of a
    !> window comes down to the window showing the registry, or else the
    !> selected window).
    recursive subroutine show_fortmacs_diff_registry(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: window

        call update_fortmacs_diff_registry(status)
        if (.not. status%ok()) return

        if (fortmacs_diff_group_index(fortmacs_diff_registry_buffer) == 0) then
            call set_error(status, USER_ERROR_CODE, "No active Ediff sessions or corrupted session registry")
            return
        end if

        window = get_fortmacs_diff_visible_buffer_window(fortmacs_diff_registry_buffer)

        if (window%index /= 0) then
            call message("Already showing the registry")
            call set_window_buffer(window, fortmacs_diff_registry_buffer, status)
            if (.not. status%ok()) return
            call select_window(window, status)
        else
            call set_window_buffer(selected_frame%selected_window, fortmacs_diff_registry_buffer, status)
        end if

        if (.not. status%ok()) return
        call fortmacs_diff_show_registry_hook%execute(status)
    end subroutine show_fortmacs_diff_registry

    !> Switch to a selected session.  Port of
    !> ediff-mult.el:ediff-registry-action.
    recursive subroutine execute_fortmacs_diff_registry_action(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, control
        type(status_type) :: st
        integer(int64) :: position
        integer :: n

        position = point()
        buffer = current_buffer()
        call get_fortmacs_diff_meta_information(buffer, position, n, control, status)
        if (.not. status%ok()) return

        if (is_buffer_live(control)) then
            ! Check if this is ediff-control-buffer or ediff-meta-buffer.
            if (fortmacs_diff_group_index(control) > 0) then
                ! It's a meta-buffer -- last action should just display it.
                call show_fortmacs_diff_meta_buffer(status, control, no_update=.true.)
            else
                ! It's a session buffer -- invoke go back to session.
                call set_buffer(control, status)
                if (status%ok()) call recenter_fortmacs_diff(status, no_rehighlight=.true.)
            end if
        else
            call ding()
            call message("You've selected a stale session --- try again")
            call update_fortmacs_diff_registry(status)
        end if

        if (is_buffer_live(buffer)) then
            call set_buffer(buffer, st)
            call goto_character(position)
        end if
    end subroutine execute_fortmacs_diff_registry_action

    ! ------------------------------------------------------------------
    ! Showing, updating and quitting groups
    ! ------------------------------------------------------------------

    ! The group buffer of the current buffer (a control buffer, a group
    ! buffer or a directory difference buffer), the null handle if none,
    ! and its session number there.  Not a port: the buffer's
    ! `fortmacs_diff_meta_buffer' and `fortmacs_diff_meta_session_number'.
    subroutine current_meta_buffer(meta, session_number)
        type(handle_type), intent(out) :: meta
        integer, intent(out) :: session_number
        integer :: session_index, group_index

        meta = null_handle()
        session_number = 0
        session_index = fortmacs_diff_session_index()

        if (session_index > 0) then
            meta = fortmacs_diff_sessions(session_index)%meta_buffer
            session_number = fortmacs_diff_sessions(session_index)%meta_session_number
            return
        end if

        group_index = fortmacs_diff_group_index(current_buffer())

        if (group_index > 0) then
            meta = fortmacs_diff_session_groups(group_index)%buffer
            session_number = fortmacs_diff_session_groups(group_index)%meta_session_number
            return
        end if

        group_index = directory_diffs_group()
        if (group_index > 0) meta = fortmacs_diff_session_groups(group_index)%buffer
    end subroutine current_meta_buffer

    !> Show the session group buffer META_BUFFER (that of the current
    !> buffer if absent).  SESSION_NUMBER, if present and positive, is the
    !> session whose markers are updated; NO_UPDATE leaves the buffer as it
    !> is; otherwise the whole group is updated.  Port of
    !> ediff-mult.el:ediff-show-meta-buffer (a SESSION-NUMBER of t is
    !> NO_UPDATE; on a terminal frame upstream's choice of a window comes
    !> down to the window showing the group, or else the selected window).
    recursive subroutine show_fortmacs_diff_meta_buffer(status, meta_buffer, session_number, no_update)
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: meta_buffer
        integer, intent(in), optional :: session_number
        logical, intent(in), optional :: no_update
        type(handle_type) :: meta, window
        logical :: silent
        integer :: number, own_number

        status = status_type()
        silent = present(meta_buffer)

        if (present(meta_buffer)) then
            meta = meta_buffer
        else
            call current_meta_buffer(meta, own_number)
        end if

        if (meta%index == 0) then
            call set_error(status, USER_ERROR_CODE, "This Ediff session is not part of a session group")
            return
        else if (fortmacs_diff_group_index(meta) == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "Can't find this session's group panel -- session itself is ok")
            return
        end if

        number = 0
        if (present(session_number)) number = session_number

        if (number > 0) then
            call update_fortmacs_diff_meta_buffer(meta, status, session_number=number)
        else if (present(no_update)) then
            if (.not. no_update) call clean_up_fortmacs_diff_meta_buffer(meta, status)
        else
            call clean_up_fortmacs_diff_meta_buffer(meta, status)
        end if

        if (.not. status%ok()) return
        window = get_fortmacs_diff_visible_buffer_window(meta)

        if (window%index /= 0) then
            if (.not. silent) call message("Already showing the group panel for this session")
            call set_window_buffer(window, meta, status)
            if (.not. status%ok()) return
            call select_window(window, status)
        else
            call set_window_buffer(selected_frame%selected_window, meta, status)
        end if

        if (.not. status%ok()) return
        call fortmacs_diff_show_session_group_hook%execute(status)
    end subroutine show_fortmacs_diff_meta_buffer

    !> Show the session group buffer of the current session, updating its
    !> record.  Port of ediff-mult.el:ediff-show-current-session-meta-buffer.
    recursive subroutine show_fortmacs_diff_current_session_meta_buffer(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: meta
        integer :: number

        call current_meta_buffer(meta, number)
        call show_fortmacs_diff_meta_buffer(status, session_number=number)
    end subroutine show_fortmacs_diff_current_session_meta_buffer

    !> Display the session group buffer for a selected session group.
    !> Port of ediff-mult.el:ediff-show-meta-buff-from-registry.
    recursive subroutine show_fortmacs_diff_meta_buffer_from_registry(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: target, old
        type(status_type) :: st
        integer :: n

        call get_fortmacs_diff_meta_information(current_buffer(), point(), n, target, status)
        if (.not. status%ok()) return
        if (.not. is_buffer_live(target)) return
        old = current_buffer()
        call set_buffer(target, status)
        if (.not. status%ok()) return
        call show_fortmacs_diff_meta_buffer(status, no_update=.true.)
        if (.not. status%ok() .and. is_buffer_live(old)) call set_buffer(old, st)
    end subroutine show_fortmacs_diff_meta_buffer_from_registry

    !> Update the buffer of group META_BUFFER, if it is live: MUST_REDRAW
    !> forces redrawing of the whole buffer; otherwise, it just goes over
    !> the buffer and updates activity marks and session status (of session
    !> SESSION_NUMBER only, if given), then point leaves hidden records.
    !> Port of ediff-mult.el:ediff-update-meta-buffer.
    recursive subroutine update_fortmacs_diff_meta_buffer(meta_buffer, status, must_redraw, session_number)
        type(handle_type), intent(in) :: meta_buffer
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: must_redraw
        integer, intent(in), optional :: session_number
        type(handle_type) :: old
        type(status_type) :: st
        logical :: redraw
        integer :: group_index, number

        status = status_type()
        group_index = fortmacs_diff_group_index(meta_buffer)
        if (group_index == 0) return
        redraw = .false.
        if (present(must_redraw)) redraw = must_redraw
        number = 0
        if (present(session_number)) number = session_number

        if (fortmacs_diff_session_groups(group_index)%is_registry() .and. number == 0 .and. .not. redraw) then
            ! The registry's meta list is the registry of sessions as it
            ! was when the registry buffer was made, no group's list.
            if (fortmacs_diff_session_groups(group_index)%registry_meta_list_empty) then
                if (.not. associated(this_command, commands%quit_fortmacs_diff)) then
                    call set_error(status, GENERIC_ERROR_CODE, FORTMACS_DIFF_KILLED_VITAL_BUFFER)
                end if
            else
                call set_error(status, WRONG_TYPE_ARGUMENT_CODE, "expected type(fortmacs_diff_session_group_type)")
            end if

            return
        end if

        old = current_buffer()
        call set_buffer(meta_buffer, status)
        if (.not. status%ok()) return

        if (redraw) then
            ! Completely redraw the meta buffer.
            if (fortmacs_diff_session_groups(group_index)%is_registry()) then
                call redraw_fortmacs_diff_registry_buffer(status)
            else
                call fortmacs_diff_session_groups(group_index)%redraw_directory_group_buffer(status)
            end if
        else if (number > 0) then
            ! Redraw only for the given session.
            call fortmacs_diff_session_groups(group_index)%update_session_marker(number, status)
        else
            ! Update what changed only, but scan the entire meta buffer.
            call fortmacs_diff_session_groups(group_index)%update_markers(status)
        end if

        ! Skip the invisible sessions.
        do while (status%ok() .and. fortmacs_diff_session_groups(group_index)%is_record_invisible(point()))
            call fortmacs_diff_session_groups(group_index)%next_item(status)
        end do

        ! Make the old buffer current again, keeping the first error.
        if (is_buffer_live(old)) then
            call set_buffer(old, st)
            if (status%ok()) status = st
        end if
    end subroutine update_fortmacs_diff_meta_buffer

    !> Update the buffer of group META_BUFFER and its parent's record of
    !> it, if META_BUFFER is live.  Port of
    !> ediff-mult.el:ediff-cleanup-meta-buffer.
    recursive subroutine clean_up_fortmacs_diff_meta_buffer(meta_buffer, status)
        type(handle_type), intent(in) :: meta_buffer
        type(status_type), intent(out) :: status
        integer :: group_index

        status = status_type()
        group_index = fortmacs_diff_group_index(meta_buffer)
        if (group_index == 0) return
        call update_fortmacs_diff_meta_buffer(meta_buffer, status)
        if (.not. status%ok()) return
        group_index = fortmacs_diff_group_index(meta_buffer)
        if (group_index == 0) return

        if (is_buffer_live(fortmacs_diff_session_groups(group_index)%parent_meta_buffer)) then
            call update_fortmacs_diff_meta_buffer(fortmacs_diff_session_groups(group_index)%parent_meta_buffer, status, &
                                                  session_number=fortmacs_diff_session_groups(group_index)%meta_session_number)
        end if
    end subroutine clean_up_fortmacs_diff_meta_buffer

    !> Whether no session of group META_BUFFER is in progress.  Port of
    !> ediff-mult.el:ediff-safe-to-quit.
    logical function is_fortmacs_diff_safe_to_quit(meta_buffer)
        type(handle_type), intent(in) :: meta_buffer
        integer :: group_index, n

        is_fortmacs_diff_safe_to_quit = .false.
        group_index = fortmacs_diff_group_index(meta_buffer)
        if (group_index == 0) return
        is_fortmacs_diff_safe_to_quit = .true.

        do n = 1, size(fortmacs_diff_session_groups(group_index)%sessions)
            ! In progress.
            if (is_buffer_live(fortmacs_diff_session_groups(group_index)%sessions(n)%buffer)) then
                is_fortmacs_diff_safe_to_quit = .false.
            end if
        end do
    end function is_fortmacs_diff_safe_to_quit

    !> If the group has no active session, delete the meta buffer.
    !> If no session is in progress, ask to confirm before deleting meta
    !> buffer.  Otherwise, bury the meta buffer.  If this is a session
    !> registry buffer then just bury it.  Port of
    !> ediff-mult.el:ediff-quit-meta-buffer.
    recursive subroutine quit_fortmacs_diff_meta_buffer(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, directory_diffs_buffer, meta_diff_buffer, parent
        type(status_type) :: st
        logical :: dont_show_registry, answer
        integer :: group_index, session_number

        call check_meta_buffer(group_index, status)
        if (.not. status%ok()) return
        buffer = current_buffer()
        directory_diffs_buffer = fortmacs_diff_session_groups(group_index)%directory_diffs_buffer
        meta_diff_buffer = fortmacs_diff_session_groups(group_index)%meta_diff_buffer
        session_number = fortmacs_diff_session_groups(group_index)%meta_session_number
        parent = fortmacs_diff_session_groups(group_index)%parent_meta_buffer
        dont_show_registry = fortmacs_diff_session_groups(group_index)%is_registry()

        if (dont_show_registry) then
            call bury_buffer(status)
            return
        end if

        answer = .false.

        if (is_fortmacs_diff_safe_to_quit(buffer)) then
            call read_y_or_n("Quit this session group? ", answer, status)
            if (.not. status%ok()) return
        end if

        if (answer) then
            call fortmacs_diff_quit_session_group_hook%execute(status)
            if (.not. status%ok()) return
            call message("")
            call dispose_of_fortmacs_diff_meta_buffer(buffer, status)
        else if (is_fortmacs_diff_safe_to_quit(buffer)) then
            call bury_buffer(status)
        else
            call set_error(status, GENERIC_ERROR_CODE, "This session group has active sessions---cannot exit")
        end if

        if (.not. status%ok()) return
        call update_fortmacs_diff_meta_buffer(parent, status, session_number=session_number)
        if (.not. status%ok()) return
        call close_fortmacs_diff_buffer_carefully(directory_diffs_buffer, st)
        call close_fortmacs_diff_buffer_carefully(meta_diff_buffer, st)

        if (is_buffer_live(parent)) then
            dont_show_registry = .true.
            call show_fortmacs_diff_meta_buffer(status, parent, session_number=session_number)
            if (.not. status%ok()) return
        end if

        if (.not. dont_show_registry) call show_fortmacs_diff_registry(status)
    end subroutine quit_fortmacs_diff_meta_buffer

    !> Remove group BUFFER from the registry and close it with its
    !> directory difference buffer.  Port of
    !> ediff-mult.el:ediff-dispose-of-meta-buffer.
    recursive subroutine dispose_of_fortmacs_diff_meta_buffer(buffer, status)
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        integer :: group_index

        status = status_type()
        fortmacs_diff_session_registry = registry_list()
        fortmacs_diff_session_registry = pack(fortmacs_diff_session_registry,.not. same_handle(fortmacs_diff_session_registry, &
                                                                                               buffer))
        group_index = fortmacs_diff_group_index(buffer)

        if (group_index > 0) then
            call close_fortmacs_diff_buffer_carefully(fortmacs_diff_session_groups(group_index)%directory_diffs_buffer, status)
            if (.not. status%ok()) return
        end if

        call close_buffer(buffer, status)
    end subroutine dispose_of_fortmacs_diff_meta_buffer

    ! ------------------------------------------------------------------
    ! Marks
    ! ------------------------------------------------------------------

    !> Change the hiding mark of session SESSION_NUMBER of the group by MODE (MARK_*);
    !> UNMARKED is whether it was unmarked.  Port of
    !> ediff-mult.el:ediff-mark-session-for-hiding.
    subroutine mark_session_for_hiding(self, session_number, mode, unmarked)
        class(fortmacs_diff_session_group_type), intent(inout), target :: self
        integer, intent(in) :: session_number, mode
        logical, intent(out), optional :: unmarked
        logical :: unmark, ignore

        associate (s => self%sessions(session_number))
            ignore = .false.
            unmark = mode == MARK_REMOVE

            if (mode == MARK_SET) then
                unmark = .false.
            else if (s%status == FORTMACS_DIFF_SESSION_STATUS_HIDE) then
                unmark = .true.
            else if (unmark) then
                ! Says unmark, but the marker is different from H.
                ignore = .true.
            end if

            if (ignore) then
                continue
            else if (unmark) then
                s%status = FORTMACS_DIFF_SESSION_STATUS_NONE
            else
                s%status = FORTMACS_DIFF_SESSION_STATUS_HIDE
            end if
        end associate

        if (present(unmarked)) unmarked = unmark
    end subroutine mark_session_for_hiding

    !> Change the operation mark of session SESSION_NUMBER of the group by MODE (MARK_*);
    !> UNMARKED is whether it was unmarked.  Port of
    !> ediff-mult.el:ediff-mark-session-for-operation.
    subroutine mark_session_for_operation(self, session_number, mode, unmarked)
        class(fortmacs_diff_session_group_type), intent(inout), target :: self
        integer, intent(in) :: session_number, mode
        logical, intent(out), optional :: unmarked
        logical :: unmark, ignore

        associate (s => self%sessions(session_number))
            ignore = .false.
            unmark = mode == MARK_REMOVE

            if (mode == MARK_SET) then
                unmark = .false.
            else if (s%status == FORTMACS_DIFF_SESSION_STATUS_MARKED) then
                unmark = .true.
            else if (unmark) then
                ! Says unmark, but the marker is different from *.
                ignore = .true.
            end if

            if (ignore) then
                continue
            else if (unmark) then
                s%status = FORTMACS_DIFF_SESSION_STATUS_NONE
            else
                s%status = FORTMACS_DIFF_SESSION_STATUS_MARKED
            end if
        end associate

        if (present(unmarked)) unmarked = unmark
    end subroutine mark_session_for_operation

    ! Mark the session at point for hiding (HIDING) or an operation, with
    ! a prefix argument unmark it, then move to the next item and update
    ! the session's record.  Not a port: the body of
    ! ediff-mark-for-hiding-at-pos and ediff-mark-for-operation-at-pos.
    recursive subroutine mark_at_point(hiding, status)
        logical, intent(in) :: hiding
        type(status_type), intent(out) :: status
        type(handle_type) :: meta, target
        integer(int64) :: saved_point
        integer :: group_index, n, mode

        call check_meta_buffer(group_index, status)
        if (.not. status%ok()) return
        meta = current_buffer()
        ! get_fortmacs_diff_meta_information gives error if meta-buf or position are
        ! invalid.
        call get_fortmacs_diff_meta_information(meta, point(), n, target, status)
        if (.not. status%ok()) return
        mode = MARK_TOGGLE
        if (current_prefix_argument%kind /= PREFIX_NONE) mode = MARK_REMOVE

        if (hiding) then
            call fortmacs_diff_session_groups(group_index)%mark_session_for_hiding(n, mode)
        else
            call fortmacs_diff_session_groups(group_index)%mark_session_for_operation(n, mode)
        end if

        call move_items(1_int64, .true., status)
        if (.not. status%ok()) return
        saved_point = point()
        call update_fortmacs_diff_meta_buffer(meta, status, session_number=n)
        call goto_character(saved_point)
    end subroutine mark_at_point

    !> Mark session for hiding.
    !>
    !> With prefix arg UNMARK, unmark instead.  Port of
    !> ediff-mult.el:ediff-mark-for-hiding-at-pos.
    recursive subroutine mark_fortmacs_diff_for_hiding_at_position(status)
        type(status_type), intent(out) :: status

        call mark_at_point(.true., status)
    end subroutine mark_fortmacs_diff_for_hiding_at_position

    !> Mark session for a group operation.
    !>
    !> With prefix arg UNMARK, unmark instead.  Port of
    !> ediff-mult.el:ediff-mark-for-operation-at-pos.
    recursive subroutine mark_fortmacs_diff_for_operation_at_position(status)
        type(status_type), intent(out) :: status

        call mark_at_point(.false., status)
    end subroutine mark_fortmacs_diff_for_operation_at_position

    !> Hide marked sessions.
    !>
    !> With prefix arg UNHIDE, unhide instead.  Port of
    !> ediff-mult.el:ediff-hide-marked-sessions.
    recursive subroutine hide_fortmacs_diff_marked_sessions(status)
        type(status_type), intent(out) :: status
        character :: from, to
        logical :: unhide, active_sessions_exist
        integer :: group_index, n, marked_count

        call check_meta_buffer(group_index, status)
        if (.not. status%ok()) return
        unhide = current_prefix_argument%kind /= PREFIX_NONE
        from = merge(FORTMACS_DIFF_SESSION_STATUS_HIDDEN, FORTMACS_DIFF_SESSION_STATUS_HIDE, unhide)
        to = merge(FORTMACS_DIFF_SESSION_STATUS_HIDE, FORTMACS_DIFF_SESSION_STATUS_HIDDEN, unhide)
        marked_count = 0
        active_sessions_exist = .false.

        do n = 1, size(fortmacs_diff_session_groups(group_index)%sessions)
            associate (s => fortmacs_diff_session_groups(group_index)%sessions(n))
                if (s%status /= from) cycle
                marked_count = marked_count + 1

                if (to == FORTMACS_DIFF_SESSION_STATUS_HIDDEN .and. is_buffer_live(s%buffer)) then
                    ! Shouldn't hide active sessions.
                    active_sessions_exist = .true.
                else
                    s%status = to
                end if
            end associate
        end do

        if (marked_count > 0) then
            call update_fortmacs_diff_meta_buffer(fortmacs_diff_session_groups(group_index)%buffer, status, must_redraw=.true.)
            if (.not. status%ok()) return
        else
            call ding()

            if (unhide) then
                call message("Nothing to reveal...")
            else
                call message("Nothing to hide...")
            end if
        end if

        if (active_sessions_exist) call message("Note: Ediff didn't hide active sessions!")
    end subroutine hide_fortmacs_diff_marked_sessions

    !> Unmark all sessions marked for operation.  Port of
    !> ediff-mult.el:ediff-unmark-all-for-operation.
    recursive subroutine unmark_fortmacs_diff_all_for_operation(status)
        type(status_type), intent(out) :: status
        integer :: group_index, n

        call check_meta_buffer(group_index, status)
        if (.not. status%ok()) return

        do n = 1, size(fortmacs_diff_session_groups(group_index)%sessions)
            call fortmacs_diff_session_groups(group_index)%mark_session_for_operation(n, MARK_REMOVE)
        end do

        call update_fortmacs_diff_meta_buffer(current_buffer(), status, must_redraw=.true.)
    end subroutine unmark_fortmacs_diff_all_for_operation

    !> Unmark all sessions marked for hiding.  Port of
    !> ediff-mult.el:ediff-unmark-all-for-hiding.
    recursive subroutine unmark_fortmacs_diff_all_for_hiding(status)
        type(status_type), intent(out) :: status
        integer :: group_index, n

        call check_meta_buffer(group_index, status)
        if (.not. status%ok()) return

        do n = 1, size(fortmacs_diff_session_groups(group_index)%sessions)
            call fortmacs_diff_session_groups(group_index)%mark_session_for_hiding(n, MARK_REMOVE)
        end do

        call update_fortmacs_diff_meta_buffer(current_buffer(), status, must_redraw=.true.)
    end subroutine unmark_fortmacs_diff_all_for_hiding

    !> Run through the session list and mark identical files.
    !> This is used only for sessions that involve 2 or 3 files at the same
    !> time.  The key decides the action: == just indicates which files are
    !> equal, =h also marks the sessions for hiding, =m for operation.
    !> Port of ediff-mult.el:ediff-meta-mark-equal-files (with
    !> ediff-mark-if-equal; ACTION is the last key, as upstream's default).
    recursive subroutine mark_fortmacs_diff_meta_equal_files(status)
        type(status_type), intent(out) :: status
        character :: action
        logical :: marked1, marked2, marked3
        integer :: group_index, n, t

        call check_meta_buffer(group_index, status)
        if (.not. status%ok()) return
        action = "="

        if (vtag(last_command_event) == V_INTEGER) then
            if (xfixnum(last_command_event) >= 0 .and. xfixnum(last_command_event) <= 127) then
                action = achar(int(xfixnum(last_command_event)))
            end if
        end if

        call message("Comparing files...")

        do n = 1, size(fortmacs_diff_session_groups(group_index)%sessions)
            do t = 1, 3
                fortmacs_diff_session_groups(group_index)%sessions(n)%files(t)%is_equal = .false.
            end do

            marked1 = .true.
            marked2 = .true.
            marked3 = .true.
            call mark_if_equal(n, 1, 2, marked1)
            if (.not. status%ok()) return

            if (is_fortmacs_diff_metajob3(fortmacs_diff_session_groups(group_index)%job_name)) then
                call mark_if_equal(n, 1, 3, marked2)
                if (.not. status%ok()) return
                call mark_if_equal(n, 2, 3, marked3)
                if (.not. status%ok()) return
            end if

            if (marked1 .and. marked2 .and. marked3) then
                if (action == "h") then
                    call fortmacs_diff_session_groups(group_index)%mark_session_for_hiding(n, MARK_SET)
                else if (action == "m") then
                    call fortmacs_diff_session_groups(group_index)%mark_session_for_operation(n, MARK_SET)
                end if
            end if
        end do

        call message("Comparing files... Done")
        fortmacs_diff_session_groups(group_index)%recursion_asked = .false.
        fortmacs_diff_session_groups(group_index)%recurse_to_subdirectories = .false.
        call update_fortmacs_diff_meta_buffer(current_buffer(), status, must_redraw=.true.)

    contains

        ! Mark files I and J of session N as equal, if they are; MARKED is
        ! whether they were.
        recursive subroutine mark_if_equal(n, i, j, marked)
            integer, intent(in) :: n, i, j
            logical, intent(out) :: marked

            marked = .false.

            associate (s => fortmacs_diff_session_groups(group_index)%sessions(n))
                if (.not. allocated(s%files(i)%name) .or. .not. allocated(s%files(j)%name)) return
                call compare_fortmacs_diff_contents(s%files(i)%name, s%files(j)%name, "", &
                                                    fortmacs_diff_session_groups(group_index)%recursion_asked, &
                                                    fortmacs_diff_session_groups(group_index)%recurse_to_subdirectories, marked, &
                                                    status)
                if (.not. status%ok()) return

                if (marked) then
                    s%files(i)%is_equal = .true.
                    s%files(j)%is_equal = .true.
                end if
            end associate
        end subroutine mark_if_equal
    end subroutine mark_fortmacs_diff_meta_equal_files

    ! ------------------------------------------------------------------
    ! Custom diffs
    ! ------------------------------------------------------------------

    !> Collect the custom diffs of the sessions of the group marked for an
    !> operation, and of those of its subdirectories' live groups;
    !> COUNT is the number of marked sessions.  Port of
    !> ediff-mult.el:ediff-operate-on-marked-sessions (OPERATION is
    !> ediff-append-custom-diff, its one use).
    recursive subroutine operate_on_marked_sessions(self, count, status)
        class(fortmacs_diff_session_group_type), intent(inout), target :: self
        integer, intent(out) :: count
        type(status_type), intent(out) :: status
        type(handle_type) :: group_buffer, diff_buffer, session_buffer, old
        type(status_type) :: st
        integer :: n, c, child
        integer :: group_index

        call self%get_index(group_index, status)
        if (.not. status%ok()) return
        count = 0
        group_buffer = fortmacs_diff_session_groups(group_index)%buffer
        diff_buffer = fortmacs_diff_session_groups(group_index)%meta_diff_buffer

        do n = 1, size(fortmacs_diff_session_groups(group_index)%sessions)
            session_buffer = fortmacs_diff_session_groups(group_index)%sessions(n)%buffer

            if (fortmacs_diff_session_groups(group_index)%sessions(n)%status == FORTMACS_DIFF_SESSION_STATUS_MARKED) then
                count = count + 1
                call fortmacs_diff_session_groups(group_index)%append_custom_diff(n, status)
                if (.not. status%ok()) return
            else if (is_fortmacs_diff_meta_session(fortmacs_diff_session_groups(group_index)%sessions(n)) &
                     .and. is_buffer_live(session_buffer)) then
                ! Go into a session represented by a subdirectory and collect
                ! the diffs of its marked sessions.
                child = fortmacs_diff_group_index(session_buffer)

                if (child > 0) then
                    ! Pass meta-diff along.
                    fortmacs_diff_session_groups(child)%meta_diff_buffer = diff_buffer
                    old = current_buffer()
                    call set_buffer(session_buffer, status)
                    if (.not. status%ok()) return
                    ! Collect diffs in child group.
                    call fortmacs_diff_session_groups(child)%operate_on_marked_sessions(c, status)
                    if (is_buffer_live(old)) call set_buffer(old, st)
                    if (.not. status%ok()) return
                    count = count + c
                end if
            end if
        end do

        ! Just in case.
        call update_fortmacs_diff_meta_buffer(group_buffer, status, must_redraw=.true.)
    end subroutine operate_on_marked_sessions

    !> Append the custom diff of session SESSION_NUMBER of the group to the group's
    !> multifile diff buffer: from its live session's buffers, or else from
    !> its files.  Port of ediff-mult.el:ediff-append-custom-diff.
    recursive subroutine append_custom_diff(self, session_number, status)
        class(fortmacs_diff_session_group_type), intent(inout), target :: self
        integer, intent(in) :: session_number
        type(status_type), intent(out) :: status
        type(handle_type) :: session_buffer, meta_diff_buffer, custom_diff_buffer, temporary_buffer, old
        type(status_type) :: st
        integer :: session_index
        integer :: group_index

        call self%get_index(group_index, status)
        if (.not. status%ok()) return

        if (.not. is_fortmacs_diff_collect_diffs_metajob(fortmacs_diff_session_groups(group_index)%job_name)) then
            call set_error(status, GENERIC_ERROR_CODE, "Can't compute multifile patch in this context")
            return
        end if

        session_buffer = fortmacs_diff_session_groups(group_index)%sessions(session_number)%buffer
        meta_diff_buffer = fortmacs_diff_session_groups(group_index)%meta_diff_buffer
        custom_diff_buffer = null_handle()
        session_index = fortmacs_diff_session_index(session_buffer)

        if (session_index > 0) then
            ! Individual session.
            call fortmacs_diff_sessions(session_index)%compute_custom_diffs(status)
            if (.not. status%ok()) return
            custom_diff_buffer = fortmacs_diff_sessions(session_index)%custom_diff_buffer
        end if

        if (.not. is_buffer_live(meta_diff_buffer)) then
            call set_error(status, USER_ERROR_CODE, "Ediff: Something wrong--killed multiple diff's buffer")
            return
        end if

        old = current_buffer()

        if (is_buffer_live(custom_diff_buffer)) then
            ! For live session buffers we do them first because the user may
            ! have changed them with respect to the underlying files.
            call append_to_meta_diff(custom_diff_buffer)
        else
            ! If ediff session is not live, run diff directly on the files:
            ! get diffs by calling shell command on ediff-custom-diff-program.
            call get_or_make_buffer(" *ediff-tmp*", temporary_buffer, status)
            if (.not. status%ok()) return
            call set_buffer(temporary_buffer, status)
            if (.not. status%ok()) return
            call erase_buffer(status)
            if (.not. status%ok()) return

            associate (files => fortmacs_diff_session_groups(group_index)%sessions(session_number)%files)
                call shell_command(shell_quote_argument(fortmacs_diff_custom_diff_program)//" "//fortmacs_diff_custom_diff_options &
                                   //" "// &
                                   shell_quote_argument(files(1)%name)//" "//shell_quote_argument(files(2)%name), status, &
                                   insert_here=.true.)
            end associate

            if (.not. status%ok()) return
            call append_to_meta_diff(temporary_buffer)
        end if

        if (is_buffer_live(old)) call set_buffer(old, st)

    contains

        ! Insert the text of BUFFER and a newline at the end of the
        ! multifile diff buffer.
        recursive subroutine append_to_meta_diff(buffer)
            type(handle_type), intent(in) :: buffer

            call set_buffer(meta_diff_buffer, status)
            if (.not. status%ok()) return
            call goto_character(point_maximum())
            call insert_buffer_substring(buffer, status)
            if (.not. status%ok()) return
            call insert(LF, status)
        end subroutine append_to_meta_diff
    end subroutine append_custom_diff

    !> Collect custom diffs of marked sessions in buffer
    !> `*Ediff Multifile Diffs*'.
    !> This operation is defined only for `fortmacs_diff_directories' and the
    !> directory merge jobs, since its intent is to produce multifile
    !> patches.  Port of ediff-mult.el:ediff-collect-custom-diffs (the
    !> revision jobs are Deferred).
    recursive subroutine collect_fortmacs_diff_custom_diffs(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, window, old
        type(status_type) :: st
        logical :: reuse
        integer :: group_index, count

        call check_meta_buffer(group_index, status)
        if (.not. status%ok()) return
        buffer = fortmacs_diff_session_groups(group_index)%meta_diff_buffer
        ! We assume the buffer doesn't visit any file.  If the user saves it
        ! to a file, create a new buffer rather than reusing it.
        reuse = is_buffer_live(buffer)

        if (reuse) then
            reuse = .not. allocated(buffers(buffer%index)%filename)
            if (.not. reuse) reuse = len(buffers(buffer%index)%filename) == 0
        end if

        if (.not. reuse) then
            call get_or_make_buffer(fortmacs_diff_unique_buffer_name("*Ediff Multifile Diffs", "*"), buffer, status)
            if (.not. status%ok()) return
            fortmacs_diff_session_groups(group_index)%meta_diff_buffer = buffer
        end if

        old = current_buffer()
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        buffers(current)%read_only = .false.
        call erase_buffer(status)
        call set_buffer(old, st)
        if (.not. status%ok()) return
        call fortmacs_diff_session_groups(group_index)%operate_on_marked_sessions(count, status)
        if (.not. status%ok()) return

        if (count > 0) then
            ! Did something.
            call display_buffer(buffer, window, status, table=display_action_parameters_type(inhibit_same_window=.true.))
            if (.not. status%ok()) return
            call set_buffer(buffer, status)
            if (.not. status%ok()) return
            call restore_buffer_modified(.false.)
            buffers(current)%read_only = .true.
            call diff_mode(status)
            call set_buffer(old, st)
            if (status%ok()) status = st
        else
            call ding()
            call message("No marked sessions found")
        end if
    end subroutine collect_fortmacs_diff_custom_diffs

    ! ------------------------------------------------------------------
    ! Starting sessions
    ! ------------------------------------------------------------------

    !> Give the session just set up (the control buffer is current) its
    !> session group, its number there and the file to store a merge in,
    !> and tell the group its control buffer.  Not a port: the startup
    !> hook ediff-mult.el:ediff-filegroup-action gives the sessions it
    !> starts.
    recursive subroutine attach_fortmacs_diff_session_to_group(status)
        type(status_type), intent(out) :: status
        integer :: session_index, group_index

        status = status_type()
        session_index = fortmacs_diff_session_index()
        group_index = fortmacs_diff_group_index(pending_meta_buffer)
        if (session_index == 0 .or. group_index == 0) return
        fortmacs_diff_sessions(session_index)%meta_buffer = pending_meta_buffer
        fortmacs_diff_sessions(session_index)%meta_session_number = pending_session_number
        if (allocated(pending_merge_store_file)) fortmacs_diff_sessions(session_index)%merge_store_file = pending_merge_store_file
        ! Make ediff-startup pass ediff-control-buffer back to the meta
        ! level.
        fortmacs_diff_session_groups(group_index)%sessions(pending_session_number)%buffer = &
            fortmacs_diff_sessions(session_index)%control_buffer
    end subroutine attach_fortmacs_diff_session_to_group

    !> Make the group just set up (its buffer is current) know its parent
    !> group and its number there, and the parent its buffer.  Not a port:
    !> the startup hook ediff-mult.el:ediff-filegroup-action gives the
    !> group of a subdirectory.
    recursive subroutine attach_fortmacs_diff_group_to_parent(status)
        type(status_type), intent(out) :: status
        integer :: group_index, parent_index

        status = status_type()
        group_index = fortmacs_diff_group_index(current_buffer())
        parent_index = fortmacs_diff_group_index(pending_meta_buffer)
        if (group_index == 0 .or. parent_index == 0) return
        ! Child session group should know its parent.
        fortmacs_diff_session_groups(group_index)%parent_meta_buffer = pending_meta_buffer
        fortmacs_diff_session_groups(group_index)%meta_session_number = pending_session_number
        ! And parent will know its child.
        fortmacs_diff_session_groups(parent_index)%sessions(pending_session_number)%buffer = &
            fortmacs_diff_session_groups(group_index)%buffer
    end subroutine attach_fortmacs_diff_group_to_parent

    !> Execute appropriate action for a selected session.  Port of
    !> ediff-mult.el:ediff-filegroup-action (the revision jobs are
    !> Deferred).
    recursive subroutine execute_fortmacs_diff_filegroup_action(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: meta, session_buffer, target
        type(hook_type), target :: hooks
        character(len=:), allocatable :: file1, file2, file3, regular_expression, default_regular_expression
        type(command_type), pointer :: job
        integer(int64) :: position
        logical :: answer
        integer :: group_index, n

        call check_meta_buffer(group_index, status)
        if (.not. status%ok()) return
        position = point()
        meta = current_buffer()
        ! get_fortmacs_diff_meta_information gives error if meta-buf or position are
        ! invalid.
        call get_fortmacs_diff_meta_information(meta, position, n, target, status)
        if (.not. status%ok()) return
        session_buffer = fortmacs_diff_session_groups(group_index)%sessions(n)%buffer
        default_regular_expression = get_fortmacs_diff_default_filtering_regular_expression()
        job => fortmacs_diff_session_groups(group_index)%job_name

        associate (files => fortmacs_diff_session_groups(group_index)%sessions(n)%files)
            file1 = ""
            if (allocated(files(1)%name)) file1 = files(1)%name
            file2 = ""
            if (allocated(files(2)%name)) file2 = files(2)%name
            file3 = ""
            if (allocated(files(3)%name)) file3 = files(3)%name
        end associate

        ! Make sure we don't start on hidden sessions.
        if (fortmacs_diff_session_groups(group_index)%sessions(n)%status == FORTMACS_DIFF_SESSION_STATUS_HIDDEN) then
            call ding()
            call read_y_or_n("This session is marked as hidden, unmark? ", answer, status)
            if (.not. status%ok()) return

            if (.not. answer) then
                call set_error(status, USER_ERROR_CODE, "Aborted")
                return
            end if

            fortmacs_diff_session_groups(group_index)%sessions(n)%status = FORTMACS_DIFF_SESSION_STATUS_NONE
            call update_fortmacs_diff_meta_buffer(meta, status, session_number=n)
            if (.not. status%ok()) return
        end if

        ! If the user clicked on session--move point there.
        call goto_character(position)
        pending_meta_buffer = meta
        pending_session_number = n
        if (allocated(pending_merge_store_file)) deallocate (pending_merge_store_file)

        if (allocated(fortmacs_diff_session_groups(group_index)%merge_autostore_directory)) then
            if (len(fortmacs_diff_session_groups(group_index)%merge_autostore_directory) > 0) then
                pending_merge_store_file = fortmacs_diff_session_groups(group_index)%merge_autostore_directory &
                                           //fortmacs_diff_merge_filename_prefix// &
                                           file_name_nondirectory(file1)
            end if
        end if

        ! First handle sessions involving directories (which are themselves
        ! session groups).  After that handle individual sessions.
        if (is_fortmacs_diff_meta_session(fortmacs_diff_session_groups(group_index)%sessions(n))) then
            ! Do ediff/ediff-merge on subdirectories.
            if (is_buffer_live(session_buffer)) then
                call show_fortmacs_diff_meta_buffer(status, session_buffer)
            else
                call read_string(format_prompt("Filter filenames through regular expression", default_regular_expression), &
                                 regular_expression, &
                                 status, history="fortmacs_diff_filtering_regular_expression_history", &
                                 default=default_regular_expression)
                if (.not. status%ok()) return
                ! Make it update the session's buffer after startup.
                call hooks%append(attach_fortmacs_diff_group_to_parent)
                call fortmacs_diff_directories_internal(file1, file2, file3, regular_expression, job, status, startup_hooks=hooks)
            end if
        else if (is_buffer_live(session_buffer)) then
            ! Handle an individual session with a live control buffer.
            call set_buffer(session_buffer, status)
            if (status%ok()) call recenter_fortmacs_diff(status, no_rehighlight=.true.)
        else if (fortmacs_diff_session_groups(group_index)%is_problematic_session( &
                 fortmacs_diff_session_groups(group_index)%sessions(n))) then
            call ding()
            call read_y_or_n("This session has no ancestor.  Merge without the ancestor? ", answer, status)
            if (.not. status%ok()) return

            if (answer) then
                call hooks%append(attach_fortmacs_diff_session_to_group)
                call fortmacs_diff_merge_files(file1, file2, status, startup_hooks=hooks)
            else
                call set_error(status, USER_ERROR_CODE, "Aborted")
            end if
        else if (.not. is_fortmacs_diff_metajob3(job)) then
            ! Need 2 file args.
            call hooks%append(attach_fortmacs_diff_session_to_group)

            if (associated(job, commands%fortmacs_diff_merge_directories)) then
                call fortmacs_diff_merge_files(file1, file2, status, startup_hooks=hooks)
            else
                call fortmacs_diff_files(file1, file2, status, startup_hooks=hooks)
            end if
        else
            ! Need 3 file args.
            call hooks%append(attach_fortmacs_diff_session_to_group)

            if (associated(job, commands%fortmacs_diff_merge_directories_with_ancestor)) then
                call fortmacs_diff_merge_files_with_ancestor(file1, file2, file3, status, startup_hooks=hooks)
            else
                call fortmacs_diff_files3(file1, file2, file3, status, startup_hooks=hooks)
            end if
        end if
    end subroutine execute_fortmacs_diff_filegroup_action

    ! ------------------------------------------------------------------
    ! Setup
    ! ------------------------------------------------------------------

    ! Delete the other windows of the selected window (the default
    ! function of `fortmacs_diff_show_session_group_hook').  Not a port.
    recursive subroutine delete_other_windows_of_selected(status)
        type(status_type), intent(out) :: status

        call delete_other_windows(selected_frame%selected_window, status)
    end subroutine delete_other_windows_of_selected

    !> Define Ediff Meta mode and the directory difference keymap, register
    !> the commands of ediff-mult.el and give the options their defaults.
    !> Not a port.
    subroutine initialize_fortmacs_diff_multiple()
        type(status_type) :: st

        if (associated(commands%show_fortmacs_diff_registry%execute)) return
        call major_modes%fortmacs_diff_meta_mode%define("fortmacs_diff_meta_mode", "MetaEdiff", fortmacs_diff_meta_mode, &
                                                        fortmacs_diff_meta_mode_hook)
        call forward_setting(fortmacs_diff_filtering_regular_expression_history)
        call fortmacs_diff_show_session_group_hook%prepend(delete_other_windows_of_selected)
        call make_sparse_keymap(fortmacs_diff_directory_diffs_buffer_map)
        call keymap_set(fortmacs_diff_directory_diffs_buffer_map, "<remap> <self_insert_command>", "undefined", st)
        call keymap_set(fortmacs_diff_directory_diffs_buffer_map, "q", "bury_fortmacs_diff_directory_diffs_buffer", st)
        call keymap_set(fortmacs_diff_directory_diffs_buffer_map, "SPC", "next_line", st)
        call keymap_set(fortmacs_diff_directory_diffs_buffer_map, "n", "next_line", st)
        call keymap_set(fortmacs_diff_directory_diffs_buffer_map, "DEL", "previous_line", st)
        call keymap_set(fortmacs_diff_directory_diffs_buffer_map, "p", "previous_line", st)
        call keymap_set(fortmacs_diff_directory_diffs_buffer_map, "C", "copy_fortmacs_diff_directory_diff_file", st)
        call keymap_set(fortmacs_diff_directory_diffs_buffer_map, "<delete>", "previous_line", st)
        call keymap_set(fortmacs_diff_directory_diffs_buffer_map, "<backspace>", "previous_line", st)
        commands%fortmacs_diff_meta_mode%execute => fortmacs_diff_meta_mode
        commands%show_fortmacs_diff_registry%execute => show_fortmacs_diff_registry
        commands%eregistry%execute => show_fortmacs_diff_registry
        commands%show_fortmacs_diff_meta_buffer%execute => execute_show_fortmacs_diff_meta_buffer
        commands%show_fortmacs_diff_current_session_meta_buffer%execute => show_fortmacs_diff_current_session_meta_buffer
        commands%show_fortmacs_diff_meta_buffer_from_registry%execute => show_fortmacs_diff_meta_buffer_from_registry
        commands%next_fortmacs_diff_meta_item%execute => next_fortmacs_diff_meta_item
        commands%previous_fortmacs_diff_meta_item%execute => previous_fortmacs_diff_meta_item
        commands%toggle_fortmacs_diff_verbose_help_meta_buffer%execute => toggle_fortmacs_diff_verbose_help_meta_buffer
        commands%toggle_fortmacs_diff_filename_truncation%execute => toggle_fortmacs_diff_filename_truncation
        commands%quit_fortmacs_diff_meta_buffer%execute => quit_fortmacs_diff_meta_buffer
        commands%execute_fortmacs_diff_filegroup_action%execute => execute_fortmacs_diff_filegroup_action
        commands%execute_fortmacs_diff_registry_action%execute => execute_fortmacs_diff_registry_action
        commands%mark_fortmacs_diff_for_hiding_at_position%execute => mark_fortmacs_diff_for_hiding_at_position
        commands%mark_fortmacs_diff_for_operation_at_position%execute => mark_fortmacs_diff_for_operation_at_position
        commands%hide_fortmacs_diff_marked_sessions%execute => hide_fortmacs_diff_marked_sessions
        commands%unmark_fortmacs_diff_all_for_operation%execute => unmark_fortmacs_diff_all_for_operation
        commands%unmark_fortmacs_diff_all_for_hiding%execute => unmark_fortmacs_diff_all_for_hiding
        commands%collect_fortmacs_diff_custom_diffs%execute => collect_fortmacs_diff_custom_diffs
        commands%up_fortmacs_diff_meta_hierarchy%execute => up_fortmacs_diff_meta_hierarchy
        commands%show_fortmacs_diff_directory_diffs%execute => show_fortmacs_diff_directory_diffs
        commands%copy_fortmacs_diff_directory_diff_file%execute => copy_fortmacs_diff_directory_diff_file
        commands%bury_fortmacs_diff_directory_diffs_buffer%execute => bury_fortmacs_diff_directory_diffs_buffer
        commands%mark_fortmacs_diff_meta_equal_files%execute => mark_fortmacs_diff_meta_equal_files
    end subroutine initialize_fortmacs_diff_multiple

    !> Show the session group buffer.  Port of
    !> ediff-mult.el:ediff-show-meta-buffer (the command).
    recursive subroutine execute_show_fortmacs_diff_meta_buffer(status)
        type(status_type), intent(out) :: status

        call show_fortmacs_diff_meta_buffer(status)
    end subroutine execute_show_fortmacs_diff_meta_buffer
end module fortmacs_diff_multiple_mod
