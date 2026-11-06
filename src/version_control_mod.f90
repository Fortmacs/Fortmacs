! version_control_mod.f90 -- drive a version-control system from within Fortmacs.
! Ported from GNU Emacs 31.1 lisp/vc/vc.el (with dired-aux.el's
! dired-vc-deduce-fileset and diff-mode.el's diff-vc-deduce-fileset).
!
! Copyright (C) 1992-1998, 2000-2026 Free Software Foundation, Inc.
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

!> VERSION_CONTROL's commands: deducing the fileset and backend of the current buffer
!> (a file under version control, a Dired buffer, a Diff buffer or a
!> directory), `version_control_next_action' (C-x v v), registering (C-x v i),
!> ignoring (C-x v G), diffs (C-x v =, C-u C-x v =, C-x v D), reverting
!> (C-x v u, C-x v @), visiting another revision (C-x v ~), deleting
!> (C-x v x) and renaming (C-x v R) files, and the switches options.
!> dired-aux.el's `directory_editor_version_control_deduce_fileset' and diff-mode.el's
!> `diff_version_control_deduce_fileset' are here, with the fileset type.
!>
!> Tags, branches, pulling and pushing are in the submodule
!> version_control_branches_mod, the logs in version_control_log_mod.
!>
!> Deferred: committing (`version_control_checkin', and so C-x v v on changed files
!> and on a patch: Log Edit mode), the log commands (Log View mode), VC-Dir,
!> annotating, merging, the incoming/outgoing diffs and unintegrated
!> changes, working trees, cherry-picks
!> and the history-rewriting commands, `version_control_find_revision_no_save' (the
!> revision is saved as FILE.~REVISION~), `version_control_coding_system_for_diff' (output
!> is decoded as UTF-8), restoring point in a reverted *vc-diff* buffer
!> (vc-diff-restore-buffer), `version_control_diff_finish_functions' other than
!> shrinking the window, the locking backends' operations (stealing locks,
!> checking out, unlocked changes), transferring files between backends
!> (C-x v v with a backend name), `version_control_user_edit_command', and C-x v v from
!> Log View and VC-Dir buffers.
module version_control_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use execute_interactively_mod, only: PREFIX_NONE
    use commands_mod, only: commands
    use error_mod, only: status_type, set_error, format_error, condition_matches, GENERIC_ERROR_CODE, USER_ERROR_CODE, &
                         VERSION_CONTROL_NOT_SUPPORTED_CODE, FILE_ALREADY_EXISTS_CODE
    use handle_mod, only: handle_type, null_handle
    use value_mod, only: value_type, make_list
    use data_mod, only: symbol_value, set, append_locals_clearer
    use hooks_mod, only: hook_type
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_or_make_buffer, get_buffer, is_buffer_live, &
                          get_file_buffer, buffer_list, close_buffer, generate_new_buffer_name, restore_buffer_modified, &
                          buffer_serial_live
    use editing_functions_mod, only: point, goto_character, point_minimum, point_maximum, is_beginning_of_line, is_end_of_buffer, &
                                     buffer_string, is_buffer_narrowed
    use insert_delete_mod, only: insert
    use search_mod, only: regular_expression_search_forward, replace_match, regular_expression_quote, SEARCH_NOERROR
    use file_input_output_mod, only: expand_file_name, file_name_directory, file_name_nondirectory, file_name_as_directory, &
                                     directory_file_name, file_exists, is_file_directory, is_file_readable, is_file_name_absolute, &
                                     is_file_symlink, file_attributes, file_status_type, rename_file, copy_file, write_region, &
                                     is_directory_name
    use files_mod, only: file_relative_name, find_file_noselect, save_some_buffers, set_visited_file_name, &
                         revert_buffer_function, revert_buffer_in_progress, create_backup_files, is_backup_file_name, &
                         delete_file, delete_directory, save_buffer, is_file_in_directory, backup_buffer
    use execute_interactively_mod, only: current_prefix_argument
    use minibuffer_reading_mod, only: read_string, completion_list_table
    use minibuffer_mod, only: read_file_name, completing_read, REQUIRE_MATCH_YES, REQUIRE_MATCH_NO
    use basic_subprograms_mod, only: read_y_or_n
    use utility_functions_mod, only: read_yes_or_no
    use display_engine_mod, only: message
    use window_mod, only: pop_to_buffer, switch_to_buffer_other_window, quit_windows_on, &
                          shrink_window_if_larger_than_buffer, get_buffer_window
    use diff_mod, only: diff_switches
    use diff_mode_mod, only: diff_mode, diff_setup_buffer_type, set_diff_version_control_backend, diff_version_control_backend, &
                             diff_file_next, diff_find_file_name
    use directory_editor_mod, only: directory_files
    use directory_editor_mode_mod, only: directory_editor_get_marked_files
    use version_control_hooks_mod, only: version_control_switches_type, version_control_backends, version_control_backend_named, &
                                         version_control_backend_name, version_control_handled_backends, &
                                         version_control_directory_exclusions, version_control_create_backup_files, &
                                         version_control_backend, version_control_registered, version_control_state, &
                                         version_control_checkout_model, is_version_control_up_to_date, &
                                         version_control_working_revision, version_control_symbolic_working_revision, &
                                         version_control_file_set_backend, version_control_file_set_state, &
                                         version_control_file_get_state, version_control_file_set_checkout_time, &
                                         version_control_file_get_backend, version_control_file_clearprops, &
                                         version_control_mode_line, version_control_version_backup_file_name, &
                                         version_control_delete_automatic_version_backups, version_control_make_version_backup, &
                                         version_control_execute_root, version_control_execute_responsible, &
                                         is_version_control_make_version_backups, version_control_not_supported, &
                                         version_control_clear_context, version_control_touch_reset, version_control_touched, &
                                         version_control_state_name, VERSION_CONTROL_SWITCHES_UNSPECIFIED, &
                                         VERSION_CONTROL_SWITCHES_NONE, VERSION_CONTROL_SWITCHES_LIST, &
                                         VERSION_CONTROL_STATE_UNKNOWN, VERSION_CONTROL_UP_TO_DATE, VERSION_CONTROL_EDITED, &
                                         VERSION_CONTROL_LOCKED, VERSION_CONTROL_NEEDS_UPDATE, VERSION_CONTROL_NEEDS_MERGE, &
                                         VERSION_CONTROL_UNLOCKED_CHANGES, VERSION_CONTROL_ADDED, VERSION_CONTROL_REMOVED, &
                                         VERSION_CONTROL_CONFLICT, VERSION_CONTROL_MISSING, VERSION_CONTROL_IGNORED, &
                                         VERSION_CONTROL_UNREGISTERED, VERSION_CONTROL_MODEL_UNKNOWN, &
                                         VERSION_CONTROL_MODEL_IMPLICIT, VERSION_CONTROL_BACKEND_NONE, &
                                         VERSION_CONTROL_PROPERTY_BACKEND, VERSION_CONTROL_PROPERTY_STATE, &
                                         VERSION_CONTROL_PROPERTY_CHECKOUT_TIME
    use version_control_hooks_mod, only: VERSION_CONTROL_PROPERTY_WORKING_REVISION, version_control_file_set_working_revision, &
                                         version_control_delete_automatic_version_backups
    use version_control_hooks_mod, only: version_control_log_limit_type, VERSION_CONTROL_LIMIT_NONE, VERSION_CONTROL_LIMIT_NUMBER, &
                                         VERSION_CONTROL_LIMIT_REVISION
    use log_edit_mod, only: log_edit_mode, log_edit_version_control_backend
    use version_control_dispatcher_mod, only: version_control_action_type, version_control_setup_buffer, &
                                              version_control_exec_after, version_control_delistify, &
                                              version_control_resynch_buffer, version_control_buffer_synchronize, &
                                              version_control_parent_buffer, set_version_control_parent_buffer, &
                                              version_control_mode_line_hook, version_control_do_command, &
                                              initialize_version_control_dispatcher, VERSION_CONTROL_OKSTATUS_ASYNCHRONOUS, &
                                              VERSION_CONTROL_DESTINATION_CURRENT, VERSION_CONTROL_NOQUERY_YES, &
                                              version_control_start_logentry, version_control_log_operation_type, &
                                              version_control_dispatcher_browsing
    use process_mod, only: get_buffer_process
    use string_type_mod, only: string_type, append_string
    use data_mod, only: choice_setting_type
    implicit none(type, external)
    private

    public :: initialize_version_control, version_control_switches, version_control_root_directory, &
              version_control_responsible_backend, version_control_backend_for_registration, &
              version_control_deduce_backend, &
              version_control_deduce_fileset, version_control_ensure_version_control_buffer, version_control_next_action, &
              version_control_register, version_control_ignore, version_control_checkout, version_control_mark_resolved, &
              version_control_diff_internal, version_control_read_revision, version_control_version_diff, version_control_checkin, &
              version_control_root_version_diff, version_control_diff, version_control_root_diff, &
              version_control_buffer_synchronize_fileset, version_control_revision_other_window, version_control_find_revision, &
              version_control_revert, version_control_revert_file, &
              version_control_revert_files, &
              version_control_delete_file, version_control_rename_file, version_control_file_tree_walk, &
              version_control_create_repository, version_control_fileset_type, version_control_buffer_revision, &
              version_control_version_backup_file, version_control_print_log_internal, version_control_print_log, &
              version_control_print_root_log, version_control_log_view_type, version_control_shortlog, &
              log_buffer_index, make_log_buffer_entry, version_control_shrink_buffer_window, with_backend_in_rootdir, &
              set_version_control_buffer_overriding_fileset, version_control_only_files_state_and_model, &
              version_control_ignore_base_directory, &
              message_version_control_unresolved_conflicts
    public :: version_control_buffer_overriding_fileset, set_version_control_buffer_revision
    public :: version_control_create_tag, version_control_create_branch, version_control_retrieve_tag, &
              version_control_switch_branch, version_control_pull, version_control_push, version_control_incoming_revision, &
              version_control_root_log_incoming, version_control_root_log_outgoing, version_control_print_fileset_branch_log, &
              version_control_print_root_branch_log

    !> What `version_control_deduce_fileset' returns: (BACKEND FILESET
    !> FILESET-ONLY-FILES STATE CHECKOUT-MODEL), the last three only with
    !> STATE-MODEL-ONLY-FILES (HAS_STATE); a PATCH (the model `patch' of a
    !> Diff buffer).
    type :: version_control_fileset_type
        integer :: backend = 0
        type(string_type), allocatable :: files(:), only_files(:)
        integer :: state = VERSION_CONTROL_STATE_UNKNOWN
        integer :: model = VERSION_CONTROL_MODEL_UNKNOWN
        logical :: has_state = .false.
        logical :: patch = .false.
    end type version_control_fileset_type

    ! ------------------------------------------------------------------
    ! Customization variables
    ! ------------------------------------------------------------------

    !> A string or list of strings specifying extra switches for checkin.
    !> These are passed to the checkin program by `version_control_checkin'.  Port of
    !> vc.el vc-checkin-switches.
    type(version_control_switches_type), public :: version_control_checkin_switches

    !> A string or list of strings specifying extra switches for checkout.
    !> These are passed to the checkout program by `version_control_checkout'.
    !> Port of vc.el vc-checkout-switches.
    type(version_control_switches_type), public :: version_control_checkout_switches

    !> A string or list of strings; extra switches for registering a file.
    !> These are passed to the checkin program by `version_control_register'.  Port of
    !> vc.el vc-register-switches.
    type(version_control_switches_type), public :: version_control_register_switches

    !> A string or list of strings specifying switches for diff under VERSION_CONTROL.
    !> When running diff under a given BACKEND, VERSION_CONTROL uses the first
    !> value that is not VERSION_CONTROL_SWITCHES_UNSPECIFIED of `version_control_BACKEND_diff_switches',
    !> `version_control_diff_switches',
    !> and `diff_switches', in that order.  Since VERSION_CONTROL_SWITCHES_UNSPECIFIED means to check the
    !> next variable in the sequence, either of the first two may use
    !> the value VERSION_CONTROL_SWITCHES_NONE to mean no switches at all.  `version_control_diff_switches'
    !> should contain switches that are specific to version control, but
    !> not specific to any particular backend.  Port of vc.el
    !> vc-diff-switches.
    type(version_control_switches_type), public :: version_control_diff_switches

    !> A string or list of strings specifying switches for annotate under VERSION_CONTROL.
    !> When running annotate under a given BACKEND, VERSION_CONTROL uses the first
    !> value that is not VERSION_CONTROL_SWITCHES_UNSPECIFIED of `version_control_BACKEND_annotate_switches' and
    !> `version_control_annotate_switches', in that order.  Since VERSION_CONTROL_SWITCHES_UNSPECIFIED means to check
    !> the next variable in the sequence, setting the first to the value
    !> VERSION_CONTROL_SWITCHES_NONE means no switches at all.  `version_control_annotate_switches' should
    !> contain switches that are specific to version control, but not
    !> specific to any particular backend.
    !>
    !> As very few switches (if any) are used across different VERSION_CONTROL tools,
    !> please consider using the specific `version_control_BACKEND_annotate_switches'
    !> for the backend you use.  Port of vc.el vc-annotate-switches.
    type(version_control_switches_type), public :: version_control_annotate_switches

    !> Limit the number of items shown by the VERSION_CONTROL log commands.
    !> Zero means unlimited.
    !> Not all VERSION_CONTROL backends are able to support this feature.  Port of
    !> vc.el vc-log-show-limit.
    integer, public :: version_control_log_show_limit = 500

    !> Specifies whether the diff during C-x v u may be asynchronous.
    !> Enabling this option means that you can confirm a revert operation even
    !> if the local changes in the file have not been found and displayed yet.
    !> Port of vc.el vc-allow-async-revert.
    logical, public :: version_control_allow_asynchronous_revert = .false.

    !> True to allow asynchronous diff process.
    !> Enabling this means the buffer will be displayed before the diff is
    !> generated, and so might only say "No changes ...".  Port of vc.el
    !> vc-allow-async-diff.
    logical, public :: version_control_allow_asynchronous_diff = .false.

    !> Normal hook (list of functions) run after checking out a file.
    !> See `run_hooks'.  Port of vc.el vc-checkout-hook.
    type(hook_type), target, public :: version_control_checkout_hook = hook_type(name="version_control_checkout_hook")

    !> Normal hook (list of functions) run after commit or file checkin.
    !> See also `log_edit_done_hook'.  Port of vc.el vc-checkin-hook.
    type(hook_type), target, public :: version_control_checkin_hook = hook_type(name="version_control_checkin_hook")

    !> Normal hook (list of functions) run before a commit or a file checkin.
    !> See `run_hooks'.  Port of vc.el vc-before-checkin-hook.
    type(hook_type), target, public :: version_control_before_checkin_hook = hook_type(name="version_control_before_checkin_hook")

    !> Values of `version_control_revert_show_diff' besides .true. and .false.
    integer, parameter, public :: VERSION_CONTROL_REVERT_SHOW_DIFF_KILL = 2

    !> If true, `version_control_revert' shows a `version_control_diff' buffer before querying.
    !> .true. shows it and buries it afterwards,
    !> VERSION_CONTROL_REVERT_SHOW_DIFF_KILL shows it and kills it afterwards, and
    !> .false. doesn't show it.  Port of vc.el
    !> vc-revert-show-diff.
    type(choice_setting_type), public :: version_control_revert_show_diff = &
                                         choice_setting_type(name="version_control_revert_show_diff", value=.true.)

    !> If true, `version_control_find_revision' doesn't write the created buffer to file.
    !> Port of vc.el vc-find-revision-no-save.
    logical, public :: version_control_find_revision_no_save = .false.

    !> List of modes not supported by VERSION_CONTROL where backend should be deduced.
    !> In these modes the backend is deduced based on `default_directory'.
    !> If `version_control_deduce_backend_all_modes' is true, the backend is deduced in
    !> all modes.  Port of vc.el vc-deduce-backend-nonvc-modes (its t is
    !> `version_control_deduce_backend_all_modes').
    character(len=32), allocatable, public :: version_control_deduce_backend_nonvc_modes(:)

    !> The value t of `version_control_deduce_backend_nonvc_modes': deduce the backend
    !> in all modes.
    logical, public :: version_control_deduce_backend_all_modes = .false.

    !> If true, diff added files by comparing them to /dev/null.  Port of
    !> vc.el vc-diff-added-files.
    logical, public :: version_control_diff_added_files = .false.

    ! ------------------------------------------------------------------
    ! Buffer-local state
    ! ------------------------------------------------------------------

    !> A buffer's VERSION_CONTROL variables, by serial number: `version_control_buffer_overriding_fileset'
    !> (a backend and files), `version_control_buffer_revision', and the arguments its
    !> `revert_buffer_function' runs `version_control_diff_internal' with.
    type :: version_control_locals_type
        integer(int64) :: serial = 0
        logical :: has_override = .false.
        type(version_control_fileset_type) :: override
        character(len=:), allocatable :: revision
        logical :: has_diff = .false.
        logical :: diff_asynchronous = .false., diff_verbose = .false.
        type(version_control_fileset_type) :: diff_fileset
        character(len=:), allocatable :: diff_rev1, diff_rev2
    end type version_control_locals_type

    type(version_control_locals_type), allocatable :: locals(:)

    ! ------------------------------------------------------------------
    ! Logs
    ! ------------------------------------------------------------------

    !> The value of `version_control_log_short_style': which filesets get a short log.
    type, public :: version_control_log_short_style_type
        !> Show a short log if the fileset contains a directory.
        logical :: directory = .true.
        !> Show short logs for files.
        logical :: file = .false.
    end type version_control_log_short_style_type

    !> Whether or not to show a short log.
    !> If `directory' is true, show a short log if the fileset
    !> contains a directory.
    !> If `file' is true, show short logs for files.
    !> Not all VERSION_CONTROL backends support short logs!  Port of vc.el
    !> vc-log-short-style.
    type(version_control_log_short_style_type), public :: version_control_log_short_style

    !> Values of `version_control_log_view_type': the type of VERSION_CONTROL log shown in a buffer.
    !> short: short log form, one line for each commit; long: long log
    !> form, including full log message and author; with_diff: log
    !> including diffs; log_outgoing: log of changes to be pushed to
    !> upstream; log_incoming: log of changes to be brought by pulling from
    !> upstream; log_unintegrated: log of changes you've not yet finished
    !> sharing; log_search: log entries matching a pattern, shown in long
    !> format; mergebase: log created by `version_control_log_mergebase'.
    integer, parameter, public :: VERSION_CONTROL_LOG_VIEW_NONE = 0, VERSION_CONTROL_LOG_VIEW_SHORT = 1, &
                                  VERSION_CONTROL_LOG_VIEW_LONG = 2, &
                                  VERSION_CONTROL_LOG_VIEW_WITH_DIFF = 3, VERSION_CONTROL_LOG_VIEW_LOG_OUTGOING = 4, &
                                  VERSION_CONTROL_LOG_VIEW_LOG_INCOMING = 5, VERSION_CONTROL_LOG_VIEW_LOG_UNINTEGRATED = 6, &
                                  VERSION_CONTROL_LOG_VIEW_LOG_SEARCH = 7, VERSION_CONTROL_LOG_VIEW_MERGEBASE = 8

    !> Functions run at the end of the log command.
    !> Each function runs in the log output buffer without args.  Port of
    !> vc.el vc-log-finish-functions.
    type(hook_type), target, public :: version_control_log_finish_functions = hook_type(name="version_control_log_finish_functions")

    ! A log buffer's permanent-local `version_control_log_view_type' and `vc--shortlog',
    ! and the arguments its `revert_buffer_function' runs
    ! `version_control_print_log_internal' with, by serial number.
    type :: version_control_log_buffer_type
        integer(int64) :: serial = 0
        integer :: view_type = VERSION_CONTROL_LOG_VIEW_NONE
        logical :: shortlog = .false.
        logical :: has_arguments = .false.
        integer :: backend = 0
        type(string_type), allocatable :: files(:)
        character(len=:), allocatable :: working_revision
        logical :: is_start_revision = .false.
        type(version_control_log_limit_type) :: limit
        integer :: type = VERSION_CONTROL_LOG_VIEW_NONE
        ! The UPSTREAM-LOCATION of an incoming or outgoing log.
        character(len=:), allocatable :: upstream_location
    end type version_control_log_buffer_type

    type(version_control_log_buffer_type), allocatable :: log_buffers(:)

    interface
        !> The backend of a VC-Dir, Log View or Log Edit buffer (HANDLED
        !> false in other buffers).  (In version_control_deduce_mod.)  Not a port.
        module subroutine mode_backend(b, handled)
            implicit none(type, external)
            integer, intent(out) :: b
            logical, intent(out) :: handled
        end subroutine mode_backend

        !> The fileset of a VC-Dir buffer (HANDLED false in other
        !> buffers).  (In version_control_deduce_mod.)  Not a port.
        recursive module subroutine version_control_directory_fileset(fs, status, state_model_only_files, handled)
            implicit none(type, external)
            type(version_control_fileset_type), intent(out) :: fs
            type(status_type), intent(out) :: status
            logical, intent(in) :: state_model_only_files
            logical, intent(out) :: handled
        end subroutine version_control_directory_fileset

        !> The fileset of a Log View buffer (HANDLED false in other
        !> buffers).  (In version_control_deduce_mod.)  Not a port.
        module subroutine log_view_fileset(fs, handled)
            implicit none(type, external)
            type(version_control_fileset_type), intent(out) :: fs
            logical, intent(out) :: handled
        end subroutine log_view_fileset

        !> In a VC-Dir buffer, make the buffer visiting the file at point
        !> current (HANDLED false in other buffers).  (In version_control_deduce_mod.)
        !> Not a port.
        recursive module subroutine version_control_directory_ensure_buffer(status, handled)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            logical, intent(out) :: handled
        end subroutine version_control_directory_ensure_buffer
    end interface

    interface
        !> For specified BACKEND and FILES, show the VERSION_CONTROL log.
        !> Leave point at WORKING_REVISION, if it is not "".
        !> If IS_START_REVISION is true, start the log from WORKING_REVISION
        !> (not all backends support this); i.e., show only WORKING_REVISION and
        !> earlier revisions.  Show up to LIMIT entries (none means unlimited).
        !> LIMIT can also be a string, which means the revision before which to
        !> stop.  Port of vc.el:vc-print-log-internal.  (In version_control_log_mod.)
        recursive module subroutine version_control_print_log_internal(backend, files, working_revision, status, &
                                                                       is_start_revision, limit, type)
            implicit none(type, external)
            integer, intent(in) :: backend
            type(string_type), intent(in) :: files(:)
            character(len=*), intent(in) :: working_revision
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: is_start_revision
            type(version_control_log_limit_type), intent(in), optional :: limit
            integer, intent(in), optional :: type
        end subroutine version_control_print_log_internal

        !> Show in another window the VERSION_CONTROL change history of the current fileset.
        !> If WORKING_REVISION is not "", it should be a revision ID; position
        !> point in the change history buffer at that revision.
        !> If LIMIT is given, it should be a number specifying the maximum
        !> number of revisions to show; the default for interactive calls is
        !> `version_control_log_show_limit'.
        !>
        !> When called interactively with a prefix argument, prompt for
        !> WORKING_REVISION and LIMIT.
        !>
        !> This shows a short log (one line for each commit) if the current
        !> fileset includes directories and the VERSION_CONTROL backend supports that;
        !> otherwise it shows the detailed log of each commit, which includes
        !> the full log message and the author.  Additional control of the
        !> shown log style is available via `version_control_log_short_style'.  Port of
        !> vc.el:vc-print-log.  (In version_control_log_mod.)
        recursive module subroutine version_control_print_log(status, working_revision, limit)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: working_revision
            integer(int64), intent(in), optional :: limit
        end subroutine version_control_print_log

        !> Show in another window VERSION_CONTROL change history of the current VERSION_CONTROL controlled tree.
        !> If LIMIT is given, it should be a number specifying the maximum
        !> number of revisions to show; the default is `version_control_log_show_limit'.
        !> When called interactively with a prefix argument, prompt for LIMIT, but
        !> if the prefix argument is a number, use it as LIMIT.
        !> A special case is when the prefix argument is 1: in this case
        !> the command prompts for the id of a REVISION, and shows that revision
        !> with its diffs (if the underlying VERSION_CONTROLS backend supports that).  Port of
        !> vc.el:vc-print-root-log.  (In version_control_log_mod.)
        recursive module subroutine version_control_print_root_log(status, limit, revision)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            integer(int64), intent(in), optional :: limit
            character(len=*), intent(in), optional :: revision
        end subroutine version_control_print_root_log

        !> The commands C-x v l and C-x v L, reading their arguments.  (In
        !> version_control_log_mod.)
        recursive module subroutine execute_version_control_print_log(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine execute_version_control_print_log

        recursive module subroutine execute_version_control_print_root_log(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine execute_version_control_print_root_log
    end interface

    ! ------------------------------------------------------------------
    ! Tags, branches, pulling and pushing, incoming and outgoing
    ! ------------------------------------------------------------------

    !> Normal hook (list of functions) run after retrieving a tag.
    !> Port of vc.el vc-retrieve-tag-hook.
    type(hook_type), target, public :: version_control_retrieve_tag_hook = hook_type(name="version_control_retrieve_tag_hook")

    interface
        !> Descending recursively from DIRECTORY, make a tag called NAME.
        !> For each registered file, the working revision becomes part of
        !> the configuration identified by the tag.
        !> If IS_BRANCH is true (interactively, the prefix argument), the
        !> tag NAME is a new branch, and the files are checked out and
        !> updated to reflect their revisions on that branch.
        !> In interactive use, DIRECTORY is `default_directory' for repository-granular
        !> VCSes (all the modern decentralized VCSes belong to this group),
        !> otherwise the command will prompt for DIRECTORY.  Port of
        !> vc.el:vc-create-tag.  (In version_control_branches_mod.)
        recursive module subroutine version_control_create_tag(directory, name, is_branch, status)
            implicit none(type, external)
            character(len=*), intent(in) :: directory, name
            logical, intent(in) :: is_branch
            type(status_type), intent(out) :: status
        end subroutine version_control_create_tag

        !> Make a branch called NAME in directory DIRECTORY.
        !> After making the new branch, check out the branch, i.e. update the
        !> files in the tree to their revisions on the branch.
        !>
        !> Interactively, prompt for the NAME of the branch.
        !>
        !> With VCSes that maintain version information per file, this command also
        !> prompts for the directory DIRECTORY whose files, recursively, will be tagged
        !> with the NAME of new branch.  For VCSes that maintain version
        !> information for the entire repository (all the modern decentralized
        !> VCSes belong to this group), DIRECTORY is always the `default_directory'.
        !>
        !> Finally, this command might prompt for the branch or tag from which to
        !> start ("fork") the new branch, with completion candidates including
        !> all the known branches and tags in the repository.
        !>
        !> This command invokes `version_control_create_tag' with IS_BRANCH true.  Port of
        !> vc.el:vc-create-branch.  (In version_control_branches_mod.)
        recursive module subroutine version_control_create_branch(directory, name, status)
            implicit none(type, external)
            character(len=*), intent(in) :: directory, name
            type(status_type), intent(out) :: status
        end subroutine version_control_create_branch

        !> For each file in or below DIRECTORY, retrieve their version identified by tag NAME.
        !> NAME can name a branch, in which case this command will switch to the
        !> named branch in the directory DIRECTORY.
        !> Interactively, prompt for DIRECTORY only for VERSION_CONTROLS that works at file level;
        !> otherwise use the root directory of the current buffer's VERSION_CONTROL tree.
        !> If NAME is empty, it refers to the latest revisions of the current branch.
        !> If locking is used for the files in DIRECTORY, then there must not be any
        !> locked files at or below DIRECTORY (but if NAME is empty, locked files are
        !> allowed and simply skipped).
        !> If IS_BRANCH is true (interactively, the prefix argument), switch to the
        !> branch and check out and update the files to their version on that branch.
        !> In this case NAME may not be empty.
        !> This function runs the hook `version_control_retrieve_tag_hook' when finished.
        !> Port of vc.el:vc-retrieve-tag.  (In version_control_branches_mod.)
        recursive module subroutine version_control_retrieve_tag(directory, name, status, is_branch)
            implicit none(type, external)
            character(len=*), intent(in) :: directory, name
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: is_branch
        end subroutine version_control_retrieve_tag

        !> Switch to the branch NAME in the directory DIRECTORY.
        !> Interactively, prompt for DIRECTORY only for VERSION_CONTROLS that works at file level;
        !> otherwise use the root directory of the current buffer's VERSION_CONTROL tree.
        !> Interactively, prompt for the NAME of the branch.
        !> After switching to the branch, check out and update the files to their
        !> version on that branch.
        !> Uses `version_control_retrieve_tag' with IS_BRANCH true.  Port of
        !> vc.el:vc-switch-branch.  (In version_control_branches_mod.)
        recursive module subroutine version_control_switch_branch(directory, name, status)
            implicit none(type, external)
            character(len=*), intent(in) :: directory, name
            type(status_type), intent(out) :: status
        end subroutine version_control_switch_branch

        !> Update the current fileset or branch.
        !> You must be visiting a version controlled file, or in a `version_control_directory' buffer.
        !> On a distributed version control system, this runs a "pull"
        !> operation to update the current branch, prompting for an argument
        !> list if required.  PROMPT (the prefix argument) forces a prompt
        !> for the VERSION_CONTROLS command to run.
        !>
        !> On a non-distributed version control system, update the current
        !> fileset to the tip revisions.  For each unchanged and unlocked
        !> file, this simply replaces the work file with the latest revision
        !> on its branch.  If the file contains changes, any changes in the
        !> tip revision are merged into the working file.  Port of
        !> vc.el:vc-pull.  (In version_control_branches_mod.)
        recursive module subroutine version_control_pull(prompt, status)
            implicit none(type, external)
            logical, intent(in) :: prompt
            type(status_type), intent(out) :: status
        end subroutine version_control_pull

        !> Push the current branch.
        !> You must be visiting a version controlled file, or in a `version_control_directory' buffer.
        !> On a distributed version control system, this runs a "push"
        !> operation on the current branch, prompting for the precise command
        !> if required.  PROMPT true (the prefix argument) forces a prompt for
        !> the VERSION_CONTROLS command to run.
        !>
        !> On a non-distributed version control system, an error condition
        !> occurs.  It also occurs in a Bazaar bound branch.  Port of
        !> vc.el:vc-push.  (In version_control_branches_mod.)
        recursive module subroutine version_control_push(prompt, status)
            implicit none(type, external)
            logical, intent(in) :: prompt
            type(status_type), intent(out) :: status
        end subroutine version_control_push

        !> The REVISION at the head of the branch at UPSTREAM_LOCATION ("" for
        !> where `version_control_pull' pulls from) of BACKEND's repository: the last one
        !> known, unless REFRESH.  A `user_error' error condition occurs if
        !> there is none.  Port of vc.el:vc--incoming-revision.  (In
        !> version_control_branches_mod.)
        recursive module subroutine version_control_incoming_revision(backend, upstream_location, revision, status, refresh)
            implicit none(type, external)
            integer, intent(in) :: backend
            character(len=*), intent(in) :: upstream_location
            character(len=:), allocatable, intent(out) :: revision
            type(status_type), intent(out) :: status
            logical, intent(in), optional :: refresh
        end subroutine version_control_incoming_revision

        !> The commands C-x v s, C-x v b c, C-x v r, C-x v b s, C-x v + and
        !> C-x v P, reading their arguments.  (In version_control_branches_mod.)
        recursive module subroutine execute_version_control_create_tag(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine execute_version_control_create_tag

        recursive module subroutine execute_version_control_create_branch(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine execute_version_control_create_branch

        recursive module subroutine execute_version_control_retrieve_tag(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine execute_version_control_retrieve_tag

        recursive module subroutine execute_version_control_switch_branch(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine execute_version_control_switch_branch

        recursive module subroutine execute_version_control_pull(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine execute_version_control_pull

        recursive module subroutine execute_version_control_push(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine execute_version_control_push

        !> Show log of changes that will be received with pull from UPSTREAM_LOCATION.
        !> When unspecified UPSTREAM_LOCATION is the place \[version_control_update] would pull
        !> from.  When called interactively with a prefix argument, prompt for
        !> UPSTREAM_LOCATION.  In some version control systems UPSTREAM_LOCATION
        !> can be a remote branch name.  Port of vc.el:vc-root-log-incoming.
        !> (In version_control_log_mod.)
        recursive module subroutine version_control_root_log_incoming(status, upstream_location)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: upstream_location
        end subroutine version_control_root_log_incoming

        !> Show log of changes that will be sent with a push to UPSTREAM_LOCATION.
        !> When unspecified UPSTREAM_LOCATION is the place \[version_control_push] would push
        !> to.  When called interactively with a prefix argument, prompt for
        !> UPSTREAM_LOCATION.  In some version control systems UPSTREAM_LOCATION
        !> can be a remote branch name.  Port of vc.el:vc-root-log-outgoing.
        !> (In version_control_log_mod.)
        recursive module subroutine version_control_root_log_outgoing(status, upstream_location)
            implicit none(type, external)
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: upstream_location
        end subroutine version_control_root_log_outgoing

        !> Show log of VERSION_CONTROL changes on BRANCH, limited to the current fileset.
        !> When called interactively, prompts for BRANCH.
        !> In addition to logging branches, for VERSION_CONTROLS for which it makes sense you
        !> can specify a revision ID instead of a branch name to produce a log
        !> starting at that revision.  Tags and remote references also work.
        !> Port of vc.el:vc-print-fileset-branch-log.  (In version_control_log_mod.)
        recursive module subroutine version_control_print_fileset_branch_log(branch, status)
            implicit none(type, external)
            character(len=*), intent(in) :: branch
            type(status_type), intent(out) :: status
        end subroutine version_control_print_fileset_branch_log

        !> Show root log of VERSION_CONTROL changes on BRANCH in another window.
        !> When called interactively, prompts for BRANCH.
        !> In addition to logging branches, for VERSION_CONTROLS for which it makes sense you
        !> can specify a revision ID instead of a branch name to produce a log
        !> starting at that revision.  Tags and remote references also work.
        !> Port of vc.el:vc-print-root-branch-log.  (In version_control_log_mod.)
        recursive module subroutine version_control_print_root_branch_log(branch, status)
            implicit none(type, external)
            character(len=*), intent(in) :: branch
            type(status_type), intent(out) :: status
        end subroutine version_control_print_root_branch_log

        !> The commands C-x v I, C-x v O, C-x v b l and C-x v b L, reading
        !> their arguments.  (In version_control_log_mod.)
        recursive module subroutine execute_version_control_root_log_incoming(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine execute_version_control_root_log_incoming

        recursive module subroutine execute_version_control_root_log_outgoing(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine execute_version_control_root_log_outgoing

        recursive module subroutine execute_version_control_print_fileset_branch_log(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine execute_version_control_print_fileset_branch_log

        recursive module subroutine execute_version_control_print_root_branch_log(status)
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine execute_version_control_print_root_branch_log
    end interface

contains

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    !> A list of the one string S.  Not a port.
    subroutine one(list, s)
        type(string_type), allocatable, intent(out) :: list(:)
        character(len=*), intent(in) :: s

        call append_string(list, s)
    end subroutine one

    !> Whether S is one of LIST.  Not a port.
    logical function has_file(s, list)
        character(len=*), intent(in) :: s
        type(string_type), intent(in) :: list(:)
        integer :: k

        has_file = .false.

        do k = 1, size(list)
            if (list(k)%text == s .and. len(list(k)%text) == len(s)) then
                has_file = .true.
                return
            end if
        end do
    end function has_file

    !> FILE expanded.  Not a port.
    function expand(file, directory) result(full)
        character(len=*), intent(in) :: file
        character(len=*), intent(in), optional :: directory
        character(len=:), allocatable :: full
        type(status_type) :: st

        call expand_file_name(file, full, st, directory)
        if (.not. st%ok()) full = file
    end function expand

    !> The current buffer's `default_directory'.  Not a port.
    function default_directory() result(directory)
        character(len=:), allocatable :: directory

        directory = "./"
        if (current == 0) return
        if (allocated(buffers(current)%directory)) directory = buffers(current)%directory
    end function default_directory

    !> The current buffer's visited file ("" for none).  Not a port.
    function buffer_file_name() result(file)
        character(len=:), allocatable :: file

        file = ""
        if (current == 0) return
        if (allocated(buffers(current)%filename)) file = buffers(current)%filename
    end function buffer_file_name

    !> Whether the current buffer is modified.  Not a port.
    logical function is_buffer_modified(h)
        type(handle_type), intent(in) :: h

        is_buffer_modified = .false.
        if (.not. is_buffer_live(h)) return
        is_buffer_modified = buffers(h%index)%text%save_modiff < buffers(h%index)%text%modiff
    end function is_buffer_modified

    !> Whether the current buffer's major mode is MODE or derives from it.
    !> Not a port.
    logical function is_derived_mode(mode)
        use derived_mod, only: provided_mode_derived_p
        character(len=*), intent(in) :: mode

        is_derived_mode = .false.
        if (current == 0) return
        if (.not. allocated(buffers(current)%major_mode)) return
        is_derived_mode = provided_mode_derived_p(buffers(current)%major_mode, mode)
    end function is_derived_mode

    ! The index of the current buffer's entry in `locals' (0 if none).
    ! Not a port.
    integer function local_index() result(k)
        k = 0
        if (current == 0) return
        if (.not. allocated(locals)) return
        k = findloc(locals%serial, buffers(current)%serial, dim=1)
    end function local_index

    ! Find or make the current buffer's entry in `locals', its index K (0
    ! without a current buffer).  Not a port.
    subroutine make_local_entry(k)
        integer, intent(out) :: k
        type(version_control_locals_type) :: entry

        k = local_index()
        if (k > 0 .or. current == 0) return
        if (.not. allocated(locals)) allocate (locals(0))
        entry%serial = buffers(current)%serial
        locals = [locals, entry]
        k = size(locals)
    end subroutine make_local_entry

    !> Forget the current buffer's VERSION_CONTROL variables (kill-all-local-variables).
    !> Not a port.
    subroutine clear_version_control_locals()
        logical, allocatable :: keep(:)
        integer :: k

        if (.not. allocated(locals) .or. current == 0) return
        allocate (keep(size(locals)))

        do k = 1, size(locals)
            keep(k) = locals(k)%serial /= buffers(current)%serial
        end do

        locals = pack(locals, keep)
    end subroutine clear_version_control_locals

    !> Set the current buffer's `version_control_buffer_overriding_fileset' to FS, or
    !> clear it when FS is absent: upstream binds it, here it is saved and restored.
    !> Not a port.
    subroutine set_version_control_buffer_overriding_fileset(fs)
        type(version_control_fileset_type), intent(in), optional :: fs
        integer :: k

        call make_local_entry(k)
        locals(k)%has_override = present(fs)
        if (present(fs)) locals(k)%override = fs
    end subroutine set_version_control_buffer_overriding_fileset

    !> The index of the current buffer's entry in `log_buffers' (0 if
    !> none).  (Public for version_control_log_mod.)  Not a port.
    integer function log_buffer_index() result(k)
        k = 0
        if (current == 0) return
        if (.not. allocated(log_buffers)) return
        k = findloc(log_buffers%serial, buffers(current)%serial, dim=1)
    end function log_buffer_index

    !> Find or make the current buffer's entry in `log_buffers', reusing
    !> the entry of a closed buffer; K is its index (0 without a current
    !> buffer).  (Public for version_control_log_mod.)  Not a port.
    subroutine make_log_buffer_entry(k)
        integer, intent(out) :: k
        type(version_control_log_buffer_type) :: entry

        k = log_buffer_index()
        if (k > 0 .or. current == 0) return
        if (.not. allocated(log_buffers)) allocate (log_buffers(0))

        do k = 1, size(log_buffers)
            if (.not. buffer_serial_live(log_buffers(k)%serial)) exit
        end do

        if (k > size(log_buffers)) log_buffers = [log_buffers, entry]
        log_buffers(k) = version_control_log_buffer_type(serial=buffers(current)%serial)
    end subroutine make_log_buffer_entry

    !> The current buffer's `version_control_log_view_type' (VERSION_CONTROL_LOG_VIEW_NONE if not
    !> set): the type of VERSION_CONTROL log shown in it.  Not a port.
    integer function version_control_log_view_type()
        integer :: k

        version_control_log_view_type = VERSION_CONTROL_LOG_VIEW_NONE
        k = log_buffer_index()
        if (k > 0) version_control_log_view_type = log_buffers(k)%view_type
    end function version_control_log_view_type

    !> The current buffer's `vc--shortlog': whether this is a shortlog.
    !> Not a port.
    logical function version_control_shortlog()
        integer :: k

        version_control_shortlog = .false.
        k = log_buffer_index()
        if (k > 0) version_control_shortlog = log_buffers(k)%shortlog
    end function version_control_shortlog

    !> Call `shrink_window_if_larger_than_buffer' only when the current
    !> buffer is visible.  Port of vc.el:vc-shrink-buffer-window (as a
    !> function of `version_control_log_finish_functions').
    recursive subroutine version_control_shrink_buffer_window(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: window

        status = status_type()
        window = get_buffer_window(current_buffer())
        if (window%index /= 0) call shrink_window_if_larger_than_buffer(window)
    end subroutine version_control_shrink_buffer_window

    !> The current buffer's `version_control_buffer_revision' ("" for none): the VERSION_CONTROLS
    !> revision to which this buffer's contents corresponds.  Port of vc.el
    !> vc-buffer-revision.
    function version_control_buffer_revision() result(revision)
        character(len=:), allocatable :: revision
        integer :: k

        revision = ""
        k = local_index()
        if (k == 0) return
        if (allocated(locals(k)%revision)) revision = locals(k)%revision
    end function version_control_buffer_revision

    !> Set the current buffer's `version_control_buffer_revision' to REVISION (set_local).
    !> Not a port.
    subroutine set_version_control_buffer_revision(revision)
        character(len=*), intent(in) :: revision
        integer :: k

        call make_local_entry(k)
        if (k > 0) locals(k)%revision = revision
    end subroutine set_version_control_buffer_revision

    !> The current buffer's `version_control_buffer_overriding_fileset' in FS (HAS false
    !> when it has none).  Not a port.
    subroutine version_control_buffer_overriding_fileset(fs, has)
        type(version_control_fileset_type), intent(out) :: fs
        logical, intent(out) :: has
        integer :: k

        has = .false.
        k = local_index()
        if (k == 0) return
        has = locals(k)%has_override
        if (has) fs = locals(k)%override
    end subroutine version_control_buffer_overriding_fileset

    ! ------------------------------------------------------------------
    ! Switches
    ! ------------------------------------------------------------------

    !> Return a list of vc-BACKEND switches for operation OP.
    !> B is the backend.  OP is an operation such as "diff".
    !>
    !> In decreasing order of preference, return the value of:
    !> vc-BACKEND-OP-switches (e.g. `version_control_git_diff_switches');
    !> vc-OP-switches (e.g. `version_control_diff_switches'); or, in the case of
    !> diff only, `diff_switches'.
    !>
    !> If the chosen value is t, return no switches.
    !> This is so that you may set, e.g. `version_control_git_diff_switches' to t in order
    !> to override the value of `version_control_diff_switches' and `diff_switches'.
    !> Port of vc.el:vc-switches (vc-BACKEND-OP-switches only for diff,
    !> register and annotate).
    subroutine version_control_switches(b, op, switches)
        integer, intent(in) :: b
        character(len=*), intent(in) :: op
        type(string_type), allocatable, intent(out) :: switches(:)
        type(version_control_switches_type) :: sw

        allocate (switches(0))
        sw = version_control_switches_type()

        if (b > 0 .and. op == "diff") then
            if (associated(version_control_backends(b)%diff_switches)) sw = version_control_backends(b)%diff_switches
        end if

        if (b > 0 .and. op == "annotate") then
            if (associated(version_control_backends(b)%annotate_switches)) sw = version_control_backends(b)%annotate_switches
        end if

        if (b > 0 .and. op == "register") then
            if (associated(version_control_backends(b)%register_switches)) sw = version_control_backends(b)%register_switches
        end if

        if (sw%kind == VERSION_CONTROL_SWITCHES_UNSPECIFIED) then
            select case (op)
            case ("diff")
                sw = version_control_diff_switches
            case ("checkin")
                sw = version_control_checkin_switches
            case ("checkout")
                sw = version_control_checkout_switches
            case ("register")
                sw = version_control_register_switches
            case ("annotate")
                sw = version_control_annotate_switches
            case default
                ! Not an operation with switches options.
            end select
        end if

        if (sw%kind == VERSION_CONTROL_SWITCHES_UNSPECIFIED .and. op == "diff") then
            if (allocated(diff_switches)) call one(switches, diff_switches)
            return
        end if

        if (sw%kind == VERSION_CONTROL_SWITCHES_LIST) then
            if (allocated(sw%list)) switches = sw%list
        end if
    end subroutine version_control_switches

    ! ------------------------------------------------------------------
    ! Deducing the fileset and backend
    ! ------------------------------------------------------------------

    !> Return the root directory for the current VERSION_CONTROL tree (FOUND false if
    !> the root directory cannot be identified).
    !> B is the VERSION_CONTROL backend.  Port of vc.el:vc-root-dir.
    subroutine version_control_root_directory(root, found, status, backend)
        character(len=:), allocatable, intent(out) :: root
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: backend
        integer :: b

        root = ""
        found = .false.
        status = status_type()
        b = 0
        if (present(backend)) b = backend
        if (b == 0) b = version_control_deduce_backend()
        if (b == 0) return
        call version_control_execute_root(b, default_directory(), root, found, status)
        if (status%code == VERSION_CONTROL_NOT_SUPPORTED_CODE) status = status_type()
    end subroutine version_control_root_directory

    !> Return a backend that can be used for registering FILE.
    !>
    !> If no backend declares itself responsible for FILE, then FILE
    !> must not be in a version controlled directory, so try to create a
    !> repository, prompting for the directory and the VERSION_CONTROL backend to
    !> use.  Port of vc.el:vc-backend-for-registration.
    recursive integer function version_control_backend_for_registration(file, status) result(bk)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: names(:)
        type(value_type) :: item
        character(len=:), allocatable :: directory, answer, def_directory, repository_directory, saved
        logical :: found
        integer :: k, b, maxlen, n

        status = status_type()
        bk = 0
        ! First try: find a responsible backend, it must be a backend
        ! under which FILE is not yet registered and with the most
        ! specific path to FILE.
        maxlen = 0

        do k = 1, size(version_control_handled_backends)
            b = version_control_backend_named(version_control_handled_backends(k))
            if (b == 0) cycle

            if (associated(version_control_backends(b)%registered)) then
                if (version_control_backends(b)%registered(file)) cycle
            end if

            call version_control_execute_responsible(b, file, directory, found)
            if (.not. found) cycle
            n = count_components(expand(directory))

            if (n > maxlen) then
                maxlen = n
                bk = b
            end if
        end do

        if (bk /= 0) return
        ! no responsible backend
        allocate (names(0))

        do k = size(version_control_handled_backends), 1, -1
            b = version_control_backend_named(version_control_handled_backends(k))
            if (b == 0) cycle
            if (.not. associated(version_control_backends(b)%create_repository)) cycle
            item = value_type(text=version_control_backends(b)%name)
            names = [names, item]
        end do

        ! Read the VERSION_CONTROL backend from the user, only complete with the
        ! backends that have the 'create-repo method.
        call completing_read(file//" is not in a version controlled directory."//achar(10)//"Use VC backend: ", &
                             completion_list_table(make_list(names)), answer, status, require_match=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        bk = version_control_backend_named(answer)

        if (bk == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "Unknown backend "//answer)
            return
        end if

        ! read the directory where to create the repository, make sure
        ! it's a parent of file.
        def_directory = file_name_directory(file)
        call read_file_name("create "//answer//" repository in: ", repository_directory, status, directory=default_directory(), &
                            default_filename=def_directory, mustmatch=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        saved = default_directory()
        buffers(current)%directory = file_name_as_directory(expand(repository_directory))
        call version_control_backends(bk)%create_repository(status)
        buffers(current)%directory = saved

    contains

        !> The length of (file-name-split NAME).
        integer function count_components(name) result(c)
            character(len=*), intent(in) :: name
            integer :: i

            c = 1

            do i = 1, len(name)
                if (name(i:i) == "/") c = c + 1
            end do
        end function count_components
    end function version_control_backend_for_registration

    !> Return the backend that is responsible for FILE.
    !>
    !> If FILE is already registered, return the
    !> backend of FILE.  If FILE is not registered, then the
    !> first backend in `version_control_handled_backends' that declares itself
    !> responsible for FILE is returned.
    !>
    !> Note that if FILE is a symbolic link, it will not be resolved --
    !> the responsible backend system for the symbolic link itself will
    !> be reported.
    !>
    !> If NO_ERROR is false or absent, an error condition occurs saying that no VERSION_CONTROL backend is
    !> responsible for the given file.  Port of vc.el:vc-responsible-backend.
    integer function version_control_responsible_backend(file, status, no_error) result(bk)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_error
        character(len=:), allocatable :: full, directory, best
        logical :: found, quiet
        integer :: k, b

        status = status_type()
        bk = 0
        if (.not. is_file_directory(file)) bk = version_control_backend(file)
        if (bk /= 0) return
        ! Several roots; we seem to have one vc inside another's directory.
        ! Choose the most specific.
        full = expand(file)
        best = ""

        do k = 1, size(version_control_handled_backends)
            b = version_control_backend_named(version_control_handled_backends(k))
            if (b == 0) cycle
            call version_control_execute_responsible(b, full, directory, found)
            if (.not. found) cycle
            ! We run DIRECTORY through `expand-file-name' so that abbreviated
            ! directories, such as "~/", wouldn't look "less specific" due
            ! to their artificially shorter length.
            directory = expand(directory)

            if (bk == 0 .or. len(directory) > len(best)) then
                bk = b
                best = directory
            end if
        end do

        if (bk /= 0) return
        quiet = .false.
        if (present(no_error)) quiet = no_error
        if (.not. quiet) call set_error(status, GENERIC_ERROR_CODE, "No VC backend is responsible for "//full)
    end function version_control_responsible_backend

    !> The backend of the current buffer: of its overriding fileset, of a
    !> Diff buffer made by VERSION_CONTROL, of `default_directory' in the modes of
    !> `version_control_deduce_backend_nonvc_modes', or of the visited file.  Port of
    !> vc.el:vc-deduce-backend (VC-Dir and Log buffers are deferred).
    integer function version_control_deduce_backend() result(b)
        type(status_type) :: st
        integer :: k
        logical :: nonvc
        logical :: handled

        b = 0
        k = local_index()

        if (k > 0) then
            if (locals(k)%has_override) then
                b = locals(k)%override%backend
                return
            end if
        end if

        call mode_backend(b, handled)
        if (handled) return

        if (is_derived_mode("diff_mode")) then
            b = version_control_backend_named(diff_version_control_backend())
            return
        end if

        nonvc = version_control_deduce_backend_all_modes

        if (.not. nonvc .and. allocated(version_control_deduce_backend_nonvc_modes)) then
            do k = 1, size(version_control_deduce_backend_nonvc_modes)
                if (is_derived_mode(trim(version_control_deduce_backend_nonvc_modes(k)))) nonvc = .true.
            end do
        end if

        if (nonvc) then
            b = version_control_responsible_backend(default_directory(), st)
            if (.not. st%ok()) b = 0
            return
        end if

        if (allocated(buffers(current)%version_control_mode)) b = version_control_backend(buffer_file_name())
    end function version_control_deduce_backend

    !> Deduce a set of files and a backend to which to apply an operation.
    !> Gives FS, of the form:
    !>
    !>   (BACKEND FILESET FILESET-ONLY-FILES STATE CHECKOUT-MODEL)
    !>
    !> where the last 3 members are optional, and must be present only if
    !> STATE_MODEL_ONLY_FILES is true.
    !>
    !> NOT_STATE_CHANGING, if true, means that the operation
    !> requesting the fileset doesn't intend to change the VERSION_CONTROL state,
    !> such as when printing the log or showing the diffs.
    !>
    !> If the current buffer is in Dired mode, FILESET is the
    !> list of marked files, or the file under point if no files are
    !> marked.
    !> Otherwise, if the current buffer is visiting a version-controlled
    !> file, FILESET is a single-file list containing
    !> that file's name.
    !> Otherwise, if ALLOW_UNREGISTERED is true and the visited file
    !> is unregistered, FILESET is a single-file list containing the
    !> name of the visited file.
    !> Otherwise, an error condition occurs.
    !>
    !> STATE_MODEL_ONLY_FILES, if true, means that the caller needs
    !> the FILESET-ONLY-FILES, STATE, and CHECKOUT-MODEL info, where
    !> FILESET-ONLY-FILES means only files in similar VERSION_CONTROL states,
    !> possible values of STATE are explained in `version_control_state', and MODEL in
    !> `version_control_checkout_model'.  Otherwise, these 3 members may be omitted from
    !> the returned list.
    !>
    !> BEWARE: this function may change the current buffer.  Port of
    !> vc.el:vc-deduce-fileset.
    recursive subroutine version_control_deduce_fileset(fs, status, not_state_changing, allow_unregistered, state_model_only_files)
        type(version_control_fileset_type), intent(out) :: fs
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: not_state_changing, allow_unregistered, state_model_only_files
        type(handle_type) :: parent
        type(status_type) :: st
        character(len=:), allocatable :: file
        logical :: nsc, au, smo, ok, handled
        integer :: k, b

        status = status_type()
        nsc = .false.
        au = .false.
        smo = .false.
        if (present(not_state_changing)) nsc = not_state_changing
        if (present(allow_unregistered)) au = allow_unregistered
        if (present(state_model_only_files)) smo = state_model_only_files
        k = local_index()

        if (k > 0) then
            if (locals(k)%has_override .and. .not. smo) then
                fs = locals(k)%override
                return
            end if
        end if

        call version_control_directory_fileset(fs, status, smo, handled)
        if (handled) return
        file = buffer_file_name()

        if (is_derived_mode("directory_editor_mode")) then
            call directory_editor_version_control_deduce_fileset(fs, status, smo, nsc)
            return
        end if

        if (is_derived_mode("diff_mode") .and. len(file) == 0) then
            call diff_version_control_deduce_fileset(fs, status)
            return
        end if

        b = version_control_backend(file)

        if (b /= 0) then
            fs%backend = b
            call one(fs%files, file)

            if (smo) then
                call one(fs%only_files, file)
                fs%state = version_control_state(file)
                fs%model = version_control_checkout_model(b, fs%files)
                fs%has_state = .true.
            end if

            return
        end if

        call log_view_fileset(fs, handled)
        if (handled) return
        parent = version_control_parent_buffer()

        if (is_buffer_live(parent)) then
            ok = allocated(buffers(parent%index)%filename)

            if (.not. ok) then
                call set_buffer(parent, st)
                ok = is_derived_mode("version_control_directory_mode") .or. is_derived_mode("directory_editor_mode") &
                     .or. is_derived_mode("diff_mode")
            end if

            if (ok) then
                call set_buffer(parent, st)
                call version_control_deduce_fileset(fs, status, not_state_changing, allow_unregistered, state_model_only_files)
                return
            end if
        end if

        if (len(file) == 0) then
            b = version_control_responsible_backend(default_directory(), st)

            if (st%ok() .and. b /= 0) then
                fs%backend = b
                call one(fs%files, default_directory())
                return
            end if
        end if

        if (au .and. len(file) > 0) then
            if (.not. version_control_registered(file)) then
                fs%backend = version_control_backend_for_registration(file, status)
                if (.not. status%ok()) return
                call one(fs%files, file)

                if (smo) then
                    call one(fs%only_files, file)
                    fs%state = VERSION_CONTROL_UNREGISTERED
                    fs%model = VERSION_CONTROL_MODEL_UNKNOWN
                    fs%has_state = .true.
                end if

                return
            end if
        end if

        call set_error(status, GENERIC_ERROR_CODE, "File is not under version control")
    end subroutine version_control_deduce_fileset

    !> The fileset of a Dired buffer: the marked files (or the file at
    !> point) and the backend responsible for the directory (or one to
    !> register them with).  Port of dired-aux.el:dired-vc-deduce-fileset.
    recursive subroutine directory_editor_version_control_deduce_fileset(fs, status, state_model_only_files, not_state_changing)
        type(version_control_fileset_type), intent(out) :: fs
        type(status_type), intent(out) :: status
        logical, intent(in) :: state_model_only_files, not_state_changing
        type(string_type), allocatable :: files(:)
        type(string_type), allocatable :: names(:)
        integer, allocatable :: states(:)
        type(status_type) :: st
        integer :: k

        call directory_editor_get_marked_files(files, status, error="")
        if (.not. status%ok()) return
        allocate (names(size(files)))

        do k = 1, size(files)
            names(k)%text = files(k)%text
        end do

        fs%backend = version_control_responsible_backend(default_directory(), st, no_error=.true.)

        if (fs%backend == 0 .and. size(names) > 0) then
            fs%backend = version_control_backend_for_registration(names(1)%text, status)
            if (.not. status%ok()) return
        end if

        if (.not. not_state_changing) then
            do k = 1, size(names)
                if (is_file_directory(names(k)%text)) then
                    call set_error(status, USER_ERROR_CODE, &
                                   "State-changing VC operations on directories supported only from VC-Dir")
                    return
                end if
            end do
        end if

        fs%files = names

        if (state_model_only_files) then
            allocate (states(size(names)))

            do k = 1, size(names)
                states(k) = version_control_state(names(k)%text)
            end do

            call version_control_only_files_state_and_model(names, states, fs%backend, fs, status)
        end if
    end subroutine directory_editor_version_control_deduce_fileset

    !> The fileset of a Diff buffer not visiting a file: the files the
    !> patch changes, and the model `patch' (C-x v v applies it).  Port of
    !> diff-mode.el:diff-vc-deduce-fileset.
    recursive subroutine diff_version_control_deduce_fileset(fs, status)
        type(version_control_fileset_type), intent(out) :: fs
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name
        integer(int64) :: saved
        logical :: found

        if (is_buffer_narrowed()) then
            ! If user used `diff-restrict-view' then we may not have the
            ! file header, and the commit will not succeed (bug#73387).
            call set_error(status, USER_ERROR_CODE, &
                           "Cannot commit patch when narrowed; consider C-x n w C-c RET n C-x v v")
            return
        end if

        fs%backend = version_control_responsible_backend(default_directory(), status)
        if (.not. status%ok()) return
        allocate (fs%files(0))
        saved = point()
        call goto_character(point_minimum())

        do
            call diff_file_next(status)
            if (.not. status%ok()) exit
            if (is_end_of_buffer()) exit
            call diff_find_file_name(name, found, status, noprompt=.true.)
            if (.not. status%ok()) exit
            if (found) call append_string(fs%files, name)
        end do

        call goto_character(saved)
        if (.not. status%ok()) return
        fs%patch = .true.
        fs%has_state = .true.
        allocate (fs%only_files(0))
    end subroutine diff_version_control_deduce_fileset

    !> Make sure that the current buffer visits a version-controlled file.
    !> Port of vc.el:vc-ensure-vc-buffer.
    recursive subroutine version_control_ensure_version_control_buffer(status)
        use directory_editor_mode_mod, only: directory_editor_get_filename
        type(status_type), intent(out) :: status
        type(handle_type) :: parent, buffer
        type(status_type) :: st
        character(len=:), allocatable :: file
        logical :: found
        logical :: handled

        status = status_type()
        call version_control_directory_ensure_buffer(status, handled)
        if (.not. status%ok()) return

        if (handled) then
            continue
        else if (is_derived_mode("directory_editor_mode")) then
            call directory_editor_get_filename(file, found, status)
            if (.not. status%ok()) return
            call find_file_noselect(file, buffer, status)
            if (.not. status%ok()) return
            call set_buffer(buffer, st)
        else
            do
                parent = version_control_parent_buffer()
                if (.not. is_buffer_live(parent)) exit
                ! Avoid infinite looping when vc-parent-buffer and
                ! current buffer are the same buffer.
                if (parent%index == current) exit
                call set_buffer(parent, st)
            end do
        end if

        if (len(buffer_file_name()) == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "Buffer "//buffers(current)%name//" is not associated with a file")
        else if (version_control_backend(buffer_file_name()) == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "File "//buffer_file_name()//" is not under version control")
        end if
    end subroutine version_control_ensure_version_control_buffer

    !> Compute the last three `version_control_deduce_fileset' return value elements
    !> for FILES in STATES into FS.  B is the VERSION_CONTROL backend responsible for
    !> FILES.  Port of vc.el:vc-only-files-state-and-model.
    recursive subroutine version_control_only_files_state_and_model(files, states, b, fs, status)
        type(string_type), intent(in) :: files(:)
        integer, intent(in) :: states(:)
        integer, intent(in) :: b
        type(version_control_fileset_type), intent(inout) :: fs
        type(status_type), intent(out) :: status
        integer, allocatable :: distinct(:)
        integer :: k, first, second, s
        logical :: yes, compat, compat2

        status = status_type()
        allocate (distinct(0))

        do k = 1, size(states)
            if (.not. any(distinct == states(k))) distinct = [distinct, states(k)]
        end do

        fs%only_files = files
        fs%has_state = .true.

        ! Check that all files are in a consistent state, since we use that
        ! state to decide which operation to perform.
        if (size(distinct) == 1) then
            s = distinct(1)
        else if (all(distinct == VERSION_CONTROL_ADDED .or. distinct == VERSION_CONTROL_MISSING .or. distinct &
                     == VERSION_CONTROL_REMOVED .or. &
                     distinct == VERSION_CONTROL_EDITED)) then
            s = VERSION_CONTROL_EDITED
        else if (all(distinct == VERSION_CONTROL_ADDED .or. distinct == VERSION_CONTROL_MISSING .or. distinct &
                     == VERSION_CONTROL_REMOVED .or. &
                     distinct == VERSION_CONTROL_EDITED .or. distinct == VERSION_CONTROL_UNREGISTERED)) then
            ! Special, but common case: checking in both changes and new
            ! files at once.  The actual registration is delayed until
            ! `vc-checkin' so that if the user changes their mind while
            ! entering the log message, we leave things as we found them.
            call read_y_or_n("Some files are unregistered; register them before checking in?", yes, status)
            if (.not. status%ok()) return

            if (.not. yes) then
                call clash()
                return
            end if

            s = VERSION_CONTROL_EDITED
        else
            call clash()
            return
        end if

        fs%state = s
        fs%model = VERSION_CONTROL_MODEL_UNKNOWN
        if (s /= VERSION_CONTROL_STATE_UNKNOWN .and. s /= VERSION_CONTROL_UNREGISTERED) fs%model = version_control_checkout_model( &
                                                                                                   b, files)

    contains

        logical function compatible(state)
            integer, intent(in) :: state

            compatible = state == VERSION_CONTROL_ADDED .or. state == VERSION_CONTROL_MISSING .or. state &
                         == VERSION_CONTROL_REMOVED .or. state == VERSION_CONTROL_EDITED
        end function compatible

        !> The error naming two files in clashing states.
        subroutine clash()
            use files_mod, only: abbreviate_file_name
            integer :: a, c

            ! The first state of the compatible ones (or of the others),
            ! and the first other one.
            first = 0
            second = 0
            compat = .false.

            do k = 1, size(distinct)
                if (compatible(distinct(k))) then
                    compat = .true.
                    exit
                end if
            end do

            do k = 1, size(distinct)
                compat2 = compatible(distinct(k))

                if (compat .eqv. compat2) then
                    if (first == 0) first = distinct(k)
                else if (second == 0) then
                    second = distinct(k)
                end if
            end do

            if (.not. compat) then
                second = 0
                k = findloc(distinct /= first, .true., dim=1)
                if (k > 0) second = distinct(k)
            end if

            a = max(1, findloc(states, first, dim=1))
            c = max(1, findloc(states, second, dim=1))
            call set_error(status, GENERIC_ERROR_CODE, &
                           "To apply VC operations to multiple files, the files must be in similar VC states."// &
                           achar(10)//abbreviate_file_name(files(a)%text)//" in state `"//version_control_state_name(first)// &
                           "' clashes with"//achar(10)//abbreviate_file_name(files(c)%text)//" in state `"// &
                           version_control_state_name(second)//"'")
        end subroutine clash
    end subroutine version_control_only_files_state_and_model

    !> Read a backend name with PROMPT (completing over
    !> `version_control_handled_backends').  Port of vc.el:vc-read-backend.
    recursive integer function version_control_read_backend(prompt, status) result(b)
        character(len=*), intent(in) :: prompt
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: names(:)
        character(len=:), allocatable :: answer
        type(value_type) :: saved
        type(status_type) :: st
        integer :: k

        b = 0
        allocate (names(size(version_control_handled_backends)))

        do k = 1, size(version_control_handled_backends)
            names(k) = value_type(text=trim(version_control_handled_backends(k)))
        end do

        call symbol_value(value_type(name="completion_ignore_case"), saved, st)
        call set(value_type(name="completion_ignore_case"), value_type(), st)
        call completing_read(prompt, completion_list_table(make_list(names)), answer, status, &
                             require_match=REQUIRE_MATCH_YES)
        call set(value_type(name="completion_ignore_case"), saved, st)
        if (.not. status%ok()) return
        b = version_control_backend_named(answer)
        if (b == 0) call set_error(status, GENERIC_ERROR_CODE, "VC backend "//answer//" is not ported yet")
    end function version_control_read_backend

    ! ------------------------------------------------------------------
    ! C-x v v
    ! ------------------------------------------------------------------

    !> Do the next logical version control operation on the current fileset.
    !> This requires that all files in the current VERSION_CONTROL fileset be in the
    !> sufficiently similar states.  If they are not, an error condition occurs.
    !> An error condition also occurs if files in the fileset are ignored by the VERSION_CONTROLS.
    !>
    !> For modern merging-based version control systems:
    !>   If every file in the fileset is not registered for version
    !>    control, register the fileset (but don't commit).  If VERBOSE is
    !>    true (interactively, the prefix argument), ask for the VERSION_CONTROL
    !>    backend with which to register the fileset.
    !>   If every work file in the VERSION_CONTROL fileset is either added or modified,
    !>    pop up a *vc-log* buffer to commit the fileset changes.
    !>   (If some are added or modified and some are unregistered, offer to
    !>    register the unregistered ones, first.)
    !>   For a centralized version control system, if any work file in
    !>    the VERSION_CONTROL fileset is out of date, offer to update the fileset.
    !>
    !> For old-style locking-based version control systems, like RCS:
    !>   If every file is not registered, register the file(s); with a prefix
    !>    argument, allow to specify the VERSION_CONTROL backend for registration.
    !>   If every file is registered and unlocked, check out (lock)
    !>    the file(s) for editing.
    !>   If every file is locked by you and has changes, pop up a
    !>    *vc-log* buffer to check in the changes.  Leave a
    !>    read-only copy of each changed file after checking in.
    !>   If every file is locked by you and unchanged, unlock them.
    !>   If every file is locked by someone else, offer to steal the lock.
    !>   If files are unlocked, but have changes, offer to either claim the
    !>    lock or revert to the last checked-in version.
    !>
    !> If this command is invoked from a patch buffer under `diff_mode', it
    !> will apply the diffs from the patch and pop up a *vc-log* buffer to
    !> check-in the resulting changes.
    !>
    !> When using this command to register a new file (or files), it
    !> will automatically deduce which VERSION_CONTROL repository to register it
    !> with, using the most specific one.
    !>
    !> If VERBOSE is true (interactively, the prefix argument),
    !> you can specify another VERSION_CONTROL backend for the file(s),
    !> or (for centralized VERSION_CONTROLS only) the revision ID or branch ID
    !> from which to check out the file(s).  Port of vc.el:vc-next-action
    !> (committing is deferred: Log Edit mode).
    recursive subroutine version_control_next_action(verbose, status)
        logical, intent(in) :: verbose
        type(status_type), intent(out) :: status
        type(version_control_fileset_type) :: fs, reg
        character(len=:), allocatable :: revision
        integer :: state, model, b, k

        status = status_type()
        call version_control_deduce_fileset(fs, status, .false., .true., .true.)
        if (.not. status%ok()) return
        state = fs%state
        model = fs%model

        ! If a buffer has unsaved changes, a checkout would discard those
        ! changes, so treat the buffer as having unlocked changes.
        if (model /= VERSION_CONTROL_MODEL_IMPLICIT .and. state == VERSION_CONTROL_UP_TO_DATE .and. .not. fs%patch) then
            do k = 1, size(fs%files)
                if (is_buffer_modified(get_file_buffer(fs%files(k)%text))) state = VERSION_CONTROL_UNLOCKED_CHANGES
            end do
        end if

        ! Do the right thing.
        if (state == VERSION_CONTROL_IGNORED) then
            call set_error(status, GENERIC_ERROR_CODE, "Fileset files are ignored by the version-control system")
        else if (fs%patch) then
            ! Fileset comes from a diff-mode buffer, see
            ! 'diff-vc-deduce-fileset', and the buffer is the patch to apply.
            call checkin_deferred()
        else if (state == VERSION_CONTROL_STATE_UNKNOWN .or. state == VERSION_CONTROL_UNREGISTERED) then
            if (verbose) then
                b = version_control_read_backend("Backend to register to: ", status)
                if (.not. status%ok()) return
                reg = fs
                reg%backend = b
                call version_control_register(status, reg)
            else
                call version_control_register(status, fs)
            end if
        else if (state == VERSION_CONTROL_MISSING) then
            call version_control_delete_file(fs%only_files, status)
        else if (state == VERSION_CONTROL_UP_TO_DATE .or. (verbose .and. state == VERSION_CONTROL_NEEDS_UPDATE)) then
            ! Files are up-to-date, or need a merge and user specified a
            ! revision
            if (verbose) then
                ! Go to a different revision.
                call read_string("Branch, revision, or backend to move to: ", revision, status)
                if (.not. status%ok()) return

                if (is_backend_name(revision)) then
                    call set_error(status, GENERIC_ERROR_CODE, "Transferring files between backends is not ported yet")
                    return
                end if

                do k = 1, size(fs%files)
                    call version_control_checkout(fs%files(k)%text, status, revision)
                    if (.not. status%ok()) return
                end do
            else if (model /= VERSION_CONTROL_MODEL_IMPLICIT) then
                ! check the files out
                do k = 1, size(fs%files)
                    call version_control_checkout(fs%files(k)%text, status)
                    if (.not. status%ok()) return
                end do
            else
                ! do nothing
                call message("Fileset is up-to-date")
            end if
        else if (state == VERSION_CONTROL_ADDED .or. state == VERSION_CONTROL_MISSING .or. state == VERSION_CONTROL_REMOVED &
                 .or. state == VERSION_CONTROL_EDITED) then
            ! Files have local changes
            call commit()
        else if (state == VERSION_CONTROL_LOCKED) then
            call set_error(status, GENERIC_ERROR_CODE, "Stealing locks (version_control_steal_lock) is not ported yet")
        else if (state == VERSION_CONTROL_CONFLICT) then
            call version_control_mark_resolved(fs%backend, fs%files, status)
        else if (state == VERSION_CONTROL_NEEDS_UPDATE .or. state == VERSION_CONTROL_NEEDS_MERGE .or. state &
                 == VERSION_CONTROL_UNLOCKED_CHANGES) then
            call set_error(status, GENERIC_ERROR_CODE, "VC operations of centralized and locking backends are not ported yet")
        else
            call set_error(status, GENERIC_ERROR_CODE, "Fileset is in an unknown state "//version_control_state_name(state))
        end if

    contains

        subroutine checkin_deferred()
            call set_error(status, GENERIC_ERROR_CODE, &
                           "Checking in a patch (version_control_checkin with a patch) is not ported yet")
        end subroutine checkin_deferred

        ! Files have local changes: commit them (the RCS and SCCS
        ! read-only check is deferred with those backends; the revert of
        ! unchanged locked files never happens in this branch, as upstream
        ! notes).
        subroutine commit()
            type(string_type), allocatable :: register(:)
            character(len=:), allocatable :: revision_name
            integer :: i

            ! Remaining files need to be committed
            if (size(fs%files) == 0) then
                call message("No files remain to be committed")
                return
            end if

            ! In the case there actually are any unregistered files then
            ! `vc-deduce-backend', via `vc-only-files-state-and-model',
            ! has already prompted the user to approve registering them.
            allocate (register(0))

            if (allocated(fs%only_files)) then
                do i = 1, size(fs%only_files)
                    if (.not. version_control_registered(fs%only_files(i)%text)) call append_string(register, fs%only_files(i)%text)
                end do
            end if

            if (.not. verbose) then
                call version_control_checkin(fs%files, fs%backend, status, register=register)
            else
                call read_string("New revision or backend: ", revision_name, status)
                if (.not. status%ok()) return

                if (is_backend_name(revision_name)) then
                    call set_error(status, GENERIC_ERROR_CODE, "Transferring files between backends is not ported yet")
                else
                    call version_control_checkin(fs%files, fs%backend, status, revision=revision_name, register=register)
                end if
            end if
        end subroutine commit

        !> Whether S names a handled backend, ignoring case.
        logical function is_backend_name(s)
            use casefiddle_mod, only: downcase_string
            character(len=*), intent(in) :: s
            integer :: i

            is_backend_name = .false.

            do i = 1, size(version_control_handled_backends)
                if (downcase_string(trim(version_control_handled_backends(i))) == downcase_string(s)) is_backend_name = .true.
            end do
        end function is_backend_name
    end subroutine version_control_next_action

    !> Check in FILES.
    !>
    !> There are three calling conventions for the COMMENT and INITIAL_CONTENTS
    !> optional arguments:
    !> - COMMENT a string, INITIAL_CONTENTS false means use that comment string
    !>   without prompting the user to edit it.
    !> - COMMENT a string, INITIAL_CONTENTS true means use that comment
    !>   string as the initial contents of the log entry buffer but stop for
    !>   editing.
    !> - COMMENT_T true (and no COMMENT) means check in immediately with an
    !>   empty comment, and ignore INITIAL_CONTENTS.
    !>
    !> The optional argument REVISION may be a string specifying the new revision
    !> level (only supported for some older VCSes, like RCS and CONCURRENT_VERSIONS_SYSTEM).
    !> If the optional argument REGISTER is present, it should be a list of
    !> files to register before checking in; if any of these are already
    !> registered the checkin will abort.
    !>
    !> Runs the normal hooks `version_control_before_checkin_hook' and `version_control_checkin_hook'.
    !> Port of vc.el:vc-checkin (no PATCH_STRING: checking in patches is
    !> deferred; `version_control_asynchronous_checkin' is deferred, so checkins are synchronous).
    recursive subroutine version_control_checkin(files, b, status, comment, comment_t, initial_contents, revision, register)
        type(string_type), intent(in) :: files(:)
        integer, intent(in) :: b
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: comment, revision
        logical, intent(in), optional :: comment_t, initial_contents
        type(string_type), intent(in), optional :: register(:)
        type(version_control_log_operation_type) :: action

        call version_control_before_checkin_hook%execute(status)
        if (.not. status%ok()) return
        action%execute => checkin_operation
        action%backend = b
        if (present(revision)) action%revision = revision

        if (present(register)) then
            action%register = register
        else
            allocate (action%register(0))
        end if

        call version_control_start_logentry(files, "Enter a change comment.", "*vc-log*", action, status, comment=comment, &
                                            comment_t=comment_t, initial_contents=initial_contents, mode=backend_log_edit_mode, &
                                            after_hook=version_control_checkin_hook, backend=b)

    contains

        ! (lambda () (vc-call-backend backend 'log-edit-mode)).
        recursive subroutine backend_log_edit_mode(st)
            type(status_type), intent(out) :: st

            if (associated(version_control_backends(b)%log_edit_mode)) then
                call version_control_backends(b)%log_edit_mode(st)
            else
                ! vc-default-log-edit-mode.
                call log_edit_mode(st)
            end if
        end subroutine backend_log_edit_mode
    end subroutine version_control_checkin

    ! The log operation of `version_control_checkin': (lambda (files comment) ...),
    ! run in the parent buffer.  Port of vc.el:vc-checkin in part.
    recursive subroutine checkin_operation(operation, files, comment_in, status)
        use help_mod, only: substitute_command_keys
        type(version_control_log_operation_type), intent(in) :: operation
        type(string_type), intent(in) :: files(:)
        character(len=*), intent(in) :: comment_in
        type(status_type), intent(out) :: status
        type(version_control_fileset_type) :: fs, current_fileset
        type(string_type), allocatable :: flist(:)
        type(file_status_type) :: attribute
        type(status_type) :: st
        character(len=:), allocatable :: comment, start_again, instructions
        integer :: b, k
        logical :: ok, found

        status = status_type()
        b = operation%backend
        ! Check the user isn't likely to be surprised by what is included
        ! in the checkin.  Once a log operation is started, the fileset
        ! or patch string is locked in.  In particular, it's probably too
        ! late to offer to change it now -- checks in hooks and/or the
        ! backend's Log Edit derived mode have all already okayed the
        ! checkin.  Restarting with the new fileset or patch is easy.
        start_again = substitute_command_keys("\[version_control_next_action] to check in again")
        instructions = substitute_command_keys("type \<log_edit_mode_map>\[log_edit_close_buffer] to cancel, "// &
                                               start_again//", "// &
                                               "\[log_edit_previous_comment] to recall your message")

        if (version_control_dispatcher_browsing()) then
            ok = size(files) == 1 .and. .not. has_explicit_marks()

            if (.not. ok) then
                call version_control_deduce_fileset(current_fileset, st)
                if (st%ok()) ok = same_files(files, current_fileset%files)
            end if

            if (.not. ok) then
                call read_yes_or_no("Selected file(s) in buffer """//buffers(current)%name//""" have changed; "// &
                                    "continue with old fileset?", ok, status)
                if (.not. status%ok()) return
            end if

            if (.not. ok) then
                call set_error(status, USER_ERROR_CODE, "To use the new fileset "//instructions)
                return
            end if
        end if

        ! "This log message intentionally left almost blank".
        ! RCS 5.7 gripes about whitespace-only comments too.
        comment = comment_in
        if (verify(comment, " "//achar(9)//achar(10)) == 0) comment = "*** empty log message ***"
        ! Must not pass NOT-ESSENTIAL .true. because we will shortly
        ! call (in `vc-finish-logentry') `vc-resynch-buffer' with its
        ! NOQUERY parameter t (unless `vc-async-checkin').
        fs%backend = b
        fs%files = files
        call version_control_buffer_synchronize_fileset(fs, status)
        if (.not. status%ok()) return

        if (size(operation%register) > 0) then
            fs%files = operation%register
            call version_control_register(status, fs)
            if (.not. status%ok()) return
        end if

        if (.not. associated(version_control_backends(b)%checkin)) then
            call version_control_not_supported(status, "checkin", b)
            return
        end if

        call message("Checking in "//version_control_delistify(files)//"...")
        call with_properties_begin(files, flist)

        if (allocated(operation%revision)) then
            call version_control_backends(b)%checkin(files, comment, status, operation%revision)
        else
            call version_control_backends(b)%checkin(files, comment, status)
        end if

        if (.not. status%ok()) return

        do k = 1, size(files)
            call version_control_delete_automatic_version_backups(files(k)%text)
        end do

        do k = 1, size(flist)
            if (.not. version_control_touched(VERSION_CONTROL_PROPERTY_STATE)) call version_control_file_set_state(flist(k)%text, &
                                                                                                         VERSION_CONTROL_UP_TO_DATE)

            if (.not. version_control_touched(VERSION_CONTROL_PROPERTY_CHECKOUT_TIME)) then
                call file_attributes(flist(k)%text, attribute, found)

                if (found) then
                    call version_control_file_set_checkout_time(flist(k)%text, attribute%mtime_second, attribute%mtime_nsec)
                else
                    call version_control_file_set_checkout_time(flist(k)%text)
                end if
            end if

            if (.not. version_control_touched(VERSION_CONTROL_PROPERTY_WORKING_REVISION)) then
                call version_control_file_set_working_revision(flist(k)%text)
            end if
        end do

        call message("Checking in "//version_control_delistify(files)//"...done")

    contains

        ! Are any files in the directory browser explicitly marked?
        ! Port of vc-dispatcher.el:vc-dispatcher--explicit-marks-p (VC-Dir is
        ! not ported).
        logical function has_explicit_marks()
            type(string_type), allocatable :: marked(:)
            type(status_type) :: s
            logical :: one

            has_explicit_marks = .false.
            if (.not. is_derived_mode("directory_editor_mode")) return
            one = .false.
            call directory_editor_get_marked_files(marked, s, distinguish_one_marked=.true., one_marked=one)
            if (.not. s%ok()) return
            has_explicit_marks = one .or. size(marked) > 1
        end function has_explicit_marks

        logical function same_files(a, c)
            type(string_type), intent(in) :: a(:), c(:)
            integer :: i

            same_files = .false.
            if (size(a) /= size(c)) return

            do i = 1, size(a)
                if (a(i)%text /= c(i)%text) return
            end do

            same_files = .true.
        end function same_files
    end subroutine checkin_operation

    !> Create an empty repository in the current directory, with backend B.
    !> Port of vc.el:vc-create-repo.
    recursive subroutine version_control_create_repository(b, status)
        integer, intent(in) :: b
        type(status_type), intent(out) :: status

        status = status_type()

        if (.not. associated(version_control_backends(b)%create_repository)) then
            call version_control_not_supported(status, "create-repo", b)
            return
        end if

        call version_control_backends(b)%create_repository(status)
    end subroutine version_control_create_repository

    !> Register into a version control system.
    !> If FILESET is given, register the files in that fileset.
    !> Otherwise register the current file.
    !> If COMMENT is present, use that as an initial comment.
    !>
    !> The version control system to use is found by cycling through the list
    !> `version_control_handled_backends'.  The first backend in that list which declares
    !> itself responsible for the file (usually because other files in that
    !> directory are already registered under that backend) will be used to
    !> register the file.  If no backend declares itself responsible, the
    !> first backend that could register the file is used.  Port of
    !> vc.el:vc-register.
    recursive subroutine version_control_register(status, fileset, comment)
        type(status_type), intent(out) :: status
        type(version_control_fileset_type), intent(in), optional :: fileset
        character(len=*), intent(in), optional :: comment
        type(version_control_fileset_type) :: fs
        type(handle_type) :: bname, old
        type(status_type) :: st
        character(len=:), allocatable :: root
        logical :: found
        integer :: b, k

        status = status_type()

        if (present(fileset)) then
            fs = fileset
        else
            call version_control_deduce_fileset(fs, status, .false., .true.)
            if (.not. status%ok()) return
        end if

        b = fs%backend

        ! We used to operate on `only-files', but VERSION_CONTROL wants to provide the
        ! possibility to register directories rather than files only, since
        ! many VERSION_CONTROLS allow that as well.
        do k = 1, size(fs%files)
            if (associated(version_control_backends(b)%registered)) then
                if (version_control_backends(b)%registered(fs%files(k)%text)) then
                    call set_error(status, GENERIC_ERROR_CODE, "This file is already registered: "//fs%files(k)%text)
                    return
                end if
            end if

            ! Watch out for new buffers of size 0: the corresponding file
            ! does not exist yet, even though buffer-modified-p is nil.
            bname = get_file_buffer(fs%files(k)%text)

            if (bname%index /= 0) then
                old = current_buffer()
                call set_buffer(bname, st)
                if (.not. is_buffer_modified(bname) .and. point_maximum() == point_minimum() .and. &
                    .not. file_exists(buffers(current)%filename)) call restore_buffer_modified(.true.)
                call version_control_buffer_synchronize(status)
                call set_buffer(old, st)
                if (.not. status%ok()) return
            end if
        end do

        call message("Registering ("//version_control_delistify(fs%files)//")... ")

        do k = 1, size(fs%files)
            call version_control_file_clearprops(fs%files(k)%text)
        end do

        if (.not. associated(version_control_backends(b)%register)) then
            call version_control_not_supported(status, "register", b)
            return
        end if

        call version_control_backends(b)%register(fs%files, status, comment)
        if (.not. status%ok()) return
        call version_control_root_directory(root, found, st, b)

        do k = 1, size(fs%files)
            call version_control_file_set_backend(fs%files(k)%text, b)
            bname = get_file_buffer(fs%files(k)%text)

            if (bname%index /= 0) then
                if (.not. version_control_create_backup_files) buffers(bname%index)%backup_inhibited = .true.
            end if

            call version_control_resynch_buffer(fs%files(k)%text, root, st, keep=.true., noquery=VERSION_CONTROL_NOQUERY_YES)
        end do

        call message("Registering ("//version_control_delistify(fs%files)//")... done")
    end subroutine version_control_register

    ! ------------------------------------------------------------------
    ! Ignoring
    ! ------------------------------------------------------------------

    !> Ignore FILE under the VERSION_CONTROLS of DIRECTORY.
    !>
    !> Normally, FILE is a wildcard specification that matches the files
    !> to be ignored.  When REMOVE is true, remove FILE from the list
    !> of ignored files.
    !>
    !> DIRECTORY defaults to `default_directory' and is used to
    !> determine the responsible VERSION_CONTROL backend.
    !>
    !> When called interactively, prompt for a FILE to ignore, unless a
    !> prefix argument is given, in which case prompt for a file FILE to
    !> remove from the list of ignored files.  Port of vc.el:vc-ignore
    !> (with vc-default-ignore; no backend has its own).
    recursive subroutine version_control_ignore(file, status, directory, remove)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: directory
        logical, intent(in), optional :: remove
        character(len=:), allocatable :: directory_name, ignore
        logical :: found, rm
        integer :: b

        status = status_type()
        directory_name = default_directory()
        if (present(directory)) directory_name = directory
        b = version_control_responsible_backend(default_directory(), status)

        if (.not. status%ok()) then
            call set_error(status, GENERIC_ERROR_CODE, "Unknown backend")
            return
        end if

        rm = .false.
        if (present(remove)) rm = remove

        ! vc-default-ignore: FILE is a wildcard specification relative to
        ! DIRECTORY.
        if (.not. associated(version_control_backends(b)%find_ignore_file)) then
            call version_control_not_supported(status, "find-ignore-file", b)
            return
        end if

        call version_control_backends(b)%find_ignore_file(directory_name, ignore, found)

        if (rm) then
            call version_control_remove_regular_expression("^"//regular_expression_quote(file)//"\("//achar(10)//"\|$\)", ignore, &
                                                           status)
        else
            call version_control_add_line(file, ignore, status)
        end if
    end subroutine version_control_ignore

    !> The directory `version_control_ignore' reads file names relative to: that of the
    !> ignore file of `default_directory''s backend.  Port of
    !> vc.el:vc--ignore-base-dir.
    function version_control_ignore_base_directory(status) result(directory)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: directory
        character(len=:), allocatable :: ignore
        logical :: found
        integer :: b

        directory = default_directory()
        b = version_control_responsible_backend(default_directory(), status)
        if (.not. status%ok()) return
        if (.not. associated(version_control_backends(b)%find_ignore_file)) return
        call version_control_backends(b)%find_ignore_file(default_directory(), ignore, found)
        if (found) directory = file_name_directory(ignore)
    end function version_control_ignore_base_directory

    !> Add STRING as a line to FILE.  Port of vc.el:vc--add-line.
    recursive subroutine version_control_add_line(string, file, status)
        character(len=*), intent(in) :: string, file
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, old
        type(status_type) :: st
        integer(int64) :: found

        call find_file_noselect(file, buffer, status)
        if (.not. status%ok()) return
        old = current_buffer()
        call set_buffer(buffer, st)
        call goto_character(point_minimum())
        call regular_expression_search_forward("^"//regular_expression_quote(string)//"$", found, st, noerror=SEARCH_NOERROR)

        if (found == 0) then
            call goto_character(point_maximum())
            if (.not. is_beginning_of_line()) call insert(achar(10), status)
            call insert(string//achar(10), status)
            if (status%ok()) call save_buffer(1_int64, .false., status)
        end if

        call set_buffer(old, st)
    end subroutine version_control_add_line

    !> Remove all matching for REGULAR_EXPRESSION in FILE.  Port of
    !> vc.el:vc--remove-regexp.
    recursive subroutine version_control_remove_regular_expression(regular_expression, file, status)
        character(len=*), intent(in) :: regular_expression, file
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer, old
        type(status_type) :: st
        integer(int64) :: found

        status = status_type()
        if (.not. file_exists(file)) return
        call find_file_noselect(file, buffer, status)
        if (.not. status%ok()) return
        old = current_buffer()
        call set_buffer(buffer, st)
        call goto_character(point_minimum())

        do
            call regular_expression_search_forward(regular_expression, found, st, noerror=SEARCH_NOERROR)
            if (found == 0) exit
            call replace_match("", status)
            if (.not. status%ok()) exit
        end do

        if (status%ok()) call save_buffer(1_int64, .false., status)
        call set_buffer(old, st)
    end subroutine version_control_remove_regular_expression

    ! ------------------------------------------------------------------
    ! Checking out, resolving
    ! ------------------------------------------------------------------

    !> Retrieve a copy of the revision REVISION of FILE.
    !> REVISION defaults to the latest revision.
    !>
    !> After check-out, runs the normal hook `version_control_checkout_hook'.  Port of
    !> vc.el:vc-checkout.
    recursive subroutine version_control_checkout(file, status, revision)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: revision
        type(string_type), allocatable :: flist(:)
        type(file_status_type) :: attribute
        type(status_type) :: st
        character(len=:), allocatable :: root
        logical :: found
        integer :: b

        status = status_type()

        if (.not. present(revision)) then
            b = version_control_backend(file)

            if (b /= 0) then
                if (is_version_control_make_version_backups(b, &
                                     file) .and. is_version_control_up_to_date(file)) call version_control_make_version_backup(file)
            end if
        end if

        b = version_control_backend(file)

        if (b == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "File "//file//" is not under version control")
            return
        end if

        if (.not. associated(version_control_backends(b)%checkout)) then
            call version_control_not_supported(status, "checkout", b)
            return
        end if

        call with_properties_begin(one_string(file), flist)
        call version_control_backends(b)%checkout(file, status, revision)
        if (.not. status%ok()) return
        call file_attributes(file, attribute, found)

        do b = 1, size(flist)
            if (.not. version_control_touched(VERSION_CONTROL_PROPERTY_STATE)) then
                if (version_control_checkout_model(version_control_backend(file), flist) == VERSION_CONTROL_MODEL_IMPLICIT) then
                    call version_control_file_set_state(flist(b)%text, VERSION_CONTROL_UP_TO_DATE)
                else
                    call version_control_file_set_state(flist(b)%text, VERSION_CONTROL_EDITED)
                end if
            end if

            if (.not. version_control_touched(VERSION_CONTROL_PROPERTY_CHECKOUT_TIME)) then
                if (found) then
                    call version_control_file_set_checkout_time(flist(b)%text, attribute%mtime_second, attribute%mtime_nsec)
                else
                    call version_control_file_set_checkout_time(flist(b)%text)
                end if
            end if
        end do

        call version_control_root_directory(root, found, st)
        call version_control_resynch_buffer(file, root, status, keep=.true., noquery=VERSION_CONTROL_NOQUERY_YES)
        if (.not. status%ok()) return
        call version_control_checkout_hook%execute(status)
    end subroutine version_control_checkout

    ! The one-element array of S (an array constructor holding a function
    ! result would leak).  Not a port.
    function one_string(s) result(list)
        character(len=*), intent(in) :: s
        type(string_type), allocatable :: list(:)

        call append_string(list, s)
    end function one_string

    !> The files of with-vc-properties for FILES: each file, and for a
    !> directory the visited files of buffers in it; start recording the
    !> properties set.  Port of vc.el:with-vc-properties in part.
    subroutine with_properties_begin(files, flist)
        type(string_type), intent(in) :: files(:)
        type(string_type), allocatable, intent(out) :: flist(:)
        type(handle_type), allocatable :: all(:)
        integer :: k, j

        allocate (flist(0))
        call version_control_touch_reset()

        do k = 1, size(files)
            if (is_file_directory(files(k)%text)) then
                all = buffer_list()

                do j = 1, size(all)
                    if (.not. allocated(buffers(all(j)%index)%filename)) cycle

                    if (index(buffers(all(j)%index)%filename, files(k)%text) == 1) then
                        call append_string(flist, buffers(all(j)%index)%filename)
                    end if
                end do
            else
                call append_string(flist, files(k)%text)
            end if
        end do
    end subroutine with_properties_begin

    !> Mark the conflicts in FILES of backend B as resolved.  Port of
    !> vc.el:vc-mark-resolved.
    recursive subroutine version_control_mark_resolved(b, files, status)
        integer, intent(in) :: b
        type(string_type), intent(in) :: files(:)
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: flist(:)
        character(len=80) :: text
        integer :: k

        status = status_type()

        if (.not. associated(version_control_backends(b)%mark_resolved)) then
            call version_control_not_supported(status, "mark-resolved", b)
            return
        end if

        call with_properties_begin(files, flist)
        call version_control_backends(b)%mark_resolved(files, status)
        if (.not. status%ok()) return

        ! FIXME: Is this TRTD?  Might not be.
        if (.not. version_control_touched(VERSION_CONTROL_PROPERTY_STATE)) then
            do k = 1, size(flist)
                call version_control_file_set_state(flist(k)%text, VERSION_CONTROL_EDITED)
            end do
        end if

        ! Recompute mode lines.
        do k = 1, size(files)
            call version_control_mode_line(files(k)%text, b)
        end do

        if (size(files) > 1) then
            write (text, '("Conflicts have been resolved in ", i0, " files.  Type C-x v v to check in changes.")') size(files)
            call message(trim(text))
        else
            call message("Conflicts have been resolved in this file.  Type C-x v v to check in changes.")
        end if
    end subroutine version_control_mark_resolved

    !> Display a message indicating unresolved conflicts in FILENAME.
    !> Port of vc.el:vc-message-unresolved-conflicts.
    subroutine message_version_control_unresolved_conflicts(filename)
        character(len=*), intent(in) :: filename

        ! This enables all VERSION_CONTROL backends to give a standard, recognizable
        ! conflict message that indicates which file is conflicted.
        call message("There are unresolved conflicts in "//filename)
    end subroutine message_version_control_unresolved_conflicts

    ! ------------------------------------------------------------------
    ! Diffs
    ! ------------------------------------------------------------------

    !> The end of a diff: say there were no changes (in a buffer the
    !> asynchronous process left empty), set up the Diff buffer type, go
    !> to the start and run `version_control_diff_finish_functions'.  Port of
    !> vc.el:vc-diff-finish (as the action BUFFER, with S1 and S2 the
    !> messages and FLAG for having them).
    recursive subroutine version_control_diff_finish(action, status)
        type(version_control_action_type), intent(in) :: action
        type(status_type), intent(out) :: status
        type(handle_type) :: old, window
        type(status_type) :: st
        logical :: emptyp

        status = status_type()
        if (.not. is_buffer_live(action%buffer)) return
        old = current_buffer()
        call set_buffer(action%buffer, st)
        emptyp = point_maximum() == point_minimum()

        if (action%flag .and. emptyp) then
            call bind_read_only_insert(action%s2//"."//achar(10))
            call message(action%s2)
        end if

        call diff_setup_buffer_type()
        call goto_character(point_minimum())
        ! vc-diff-finish-functions: vc-shrink-buffer-window.
        window = get_buffer_window(action%buffer)
        if (window%index /= 0) call shrink_window_if_larger_than_buffer(window)
        call set_buffer(old, st)
        if (action%flag .and. .not. emptyp) call message(action%s1//"done")
    end subroutine version_control_diff_finish

    !> Insert TEXT ignoring read-only (`inhibit_read_only' .true., saved and restored).
    !> Not a port.
    subroutine bind_read_only_insert(text)
        use value_mod, only: value_type
        character(len=*), intent(in) :: text
        type(value_type) :: saved
        type(status_type) :: st

        call symbol_value(value_type(name="inhibit_read_only"), saved, st)
        call set(value_type(name="inhibit_read_only"), value_type(.true.), st)
        call insert(text, st)
        call set(value_type(name="inhibit_read_only"), saved, st)
    end subroutine bind_read_only_insert

    !> Report diffs between revisions REV1 and REV2 of a fileset in FS.
    !> ASYNCHRONOUS true means run the backend's commands asynchronously if possible.
    !> FS should have the format described in `version_control_deduce_fileset'.
    !> Output goes to the buffer BUFFER, which defaults to *vc-diff*.
    !> CHANGES is true if the buffer had changes, false otherwise.  Port of
    !> vc.el:vc-diff-internal.
    recursive subroutine version_control_diff_internal(asynchronous, fs, changes, status, rev1, rev2, verbose, buffer)
        logical, intent(in) :: asynchronous
        type(version_control_fileset_type), intent(in) :: fs
        logical, intent(out) :: changes
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: rev1, rev2
        logical, intent(in), optional :: verbose
        type(handle_type), intent(in), optional :: buffer
        type(handle_type) :: buffer_handle, old, process
        type(version_control_action_type) :: finish
        type(string_type), allocatable :: files(:), filtered(:), sw(:)
        type(status_type) :: st
        character(len=:), allocatable :: msg1, msg2, wrev
        logical :: verb, found
        integer :: k

        status = status_type()
        changes = .false.
        verb = .false.
        if (present(verbose)) verb = verbose

        if (present(buffer)) then
            buffer_handle = buffer
        else
            call get_or_make_buffer("*vc-diff*", buffer_handle, status)
            if (.not. status%ok()) return
        end if

        files = fs%files
        msg1 = "Finding changes in "//version_control_delistify(files)//"..."
        msg2 = "No changes between "

        if (present(rev1)) then
            msg2 = msg2//rev1
        else
            msg2 = msg2//"working revision"
        end if

        msg2 = msg2//" and "

        if (present(rev2)) then
            msg2 = msg2//rev2
        else
            msg2 = msg2//"workfile"
        end if

        old = current_buffer()
        ! (vc-setup-buffer leaves BUFFER current, so the backend's commands
        ! insert there without setting it up again.)
        call version_control_setup_buffer(buffer_handle, status)
        if (.not. status%ok()) return
        call message(msg1)

        ! Many backends don't handle well the case of a file that has been
        ! added but not yet committed to the repo (notably CONCURRENT_VERSIONS_SYSTEM and Subversion).
        if (version_control_diff_added_files) then
            allocate (filtered(0))

            do k = 1, size(files)
                call version_control_working_revision(files(k)%text, wrev, found)

                if (is_file_directory(files(k)%text) .or. .not. (found .and. wrev == "0")) then
                    call append_string(filtered, files(k)%text)
                else if (present(rev1) .or. present(rev2)) then
                    ! This file is added but not yet committed; there is
                    ! no repository version to diff against.
                    call set_error(status, GENERIC_ERROR_CODE, "No revisions of "//files(k)%text//" exist")
                    return
                else
                    ! We regard this as "changed".  Diff it against
                    ! /dev/null.
                    call version_control_switches(0, "diff", sw)
                    call append_string(sw, "/dev/null")

                    if (asynchronous) then
                        call version_control_do_command(VERSION_CONTROL_DESTINATION_CURRENT, &
                                                        VERSION_CONTROL_OKSTATUS_ASYNCHRONOUS, "diff", one_string(files(k)%text), &
                                                        sw, status, buffer=buffer_handle)
                    else
                        call version_control_do_command(VERSION_CONTROL_DESTINATION_CURRENT, 1, "diff", one_string(files(k)%text), &
                                                        sw, status, &
                                                        buffer=buffer_handle)
                    end if

                    if (.not. status%ok()) return
                end if
            end do

            files = filtered
        end if

        call set_buffer(buffer_handle, st)
        ! Make the *vc-diff* buffer read only, the diff-mode key
        ! bindings are nicer for read only buffers. pcl-cvs does the
        ! same thing.
        buffers(current)%read_only = .true.
        ! Set the major mode and some local variables before calling into
        ! the backend.
        call diff_mode(st)

        if (present(rev1) .and. present(rev2)) then
            call set_diff_version_control_backend(version_control_backend_name(fs%backend), rev1, rev2)
        else if (present(rev1)) then
            call set_diff_version_control_backend(version_control_backend_name(fs%backend), rev1=rev1)
        else if (present(rev2)) then
            call set_diff_version_control_backend(version_control_backend_name(fs%backend), rev2=rev2)
        else
            call set_diff_version_control_backend(version_control_backend_name(fs%backend))
        end if

        call remember_diff_arguments(asynchronous, fs, verb, rev1, rev2)
        call revert_buffer_function%set_local(version_control_diff_revert)

        if (.not. associated(version_control_backends(fs%backend)%diff)) then
            call version_control_not_supported(status, "diff", fs%backend)
            return
        end if

        call version_control_backends(fs%backend)%diff(files, buffer_handle, asynchronous, status, rev1, rev2)
        if (.not. status%ok()) return
        call set_buffer(buffer_handle, st)
        process = get_buffer_process(buffer_handle)

        if (point_maximum() == point_minimum() .and. process%index == 0) then
            ! Treat this case specially so as not to pop the buffer.
            call message(msg2)
            return
        end if

        ! Display the buffer, but at the end because it can change point.
        call pop_to_buffer(buffer_handle, status)
        if (.not. status%ok()) return
        ! The diff process may finish early, so call `vc-diff-finish'
        ! after `pop-to-buffer'; the former assumes the diff buffer is
        ! shown in some window.
        finish%execute => version_control_diff_finish
        finish%buffer = buffer_handle
        finish%flag = verb
        finish%s1 = msg1
        finish%s2 = msg2
        call version_control_exec_after(finish, status)
        ! In the async case, we return t even if there are no differences
        ! because we don't know that yet.
        changes = .true.
    end subroutine version_control_diff_internal

    !> Remember the arguments `version_control_diff_revert' runs `version_control_diff_internal' with
    !> in the current buffer.  Not a port.
    subroutine remember_diff_arguments(asynchronous, fs, verbose, rev1, rev2)
        logical, intent(in) :: asynchronous, verbose
        type(version_control_fileset_type), intent(in) :: fs
        character(len=*), intent(in), optional :: rev1, rev2
        integer :: k

        call make_local_entry(k)
        locals(k)%has_diff = .true.
        locals(k)%diff_asynchronous = asynchronous
        locals(k)%diff_verbose = verbose
        locals(k)%diff_fileset = fs
        if (allocated(locals(k)%diff_rev1)) deallocate (locals(k)%diff_rev1)
        if (allocated(locals(k)%diff_rev2)) deallocate (locals(k)%diff_rev2)
        if (present(rev1)) locals(k)%diff_rev1 = rev1
        if (present(rev2)) locals(k)%diff_rev2 = rev2
    end subroutine remember_diff_arguments

    !> The `revert_buffer_function' of *vc-diff* buffers: make the diff
    !> again.  Port of vc.el:vc-diff-internal in part.
    recursive subroutine version_control_diff_revert(ignore_auto, noconfirm, status)
        logical, intent(in) :: ignore_auto, noconfirm
        type(status_type), intent(out) :: status
        type(version_control_locals_type) :: l
        type(handle_type) :: buffer
        logical :: changes
        integer :: k

        status = status_type()
        if (ignore_auto .and. noconfirm .and. .false.) return
        k = local_index()
        if (k == 0) return
        l = locals(k)
        buffer = current_buffer()

        if (allocated(l%diff_rev1) .and. allocated(l%diff_rev2)) then
            call version_control_diff_internal(l%diff_asynchronous, l%diff_fileset, changes, status, l%diff_rev1, l%diff_rev2, &
                                               l%diff_verbose, buffer)
        else if (allocated(l%diff_rev1)) then
            call version_control_diff_internal(l%diff_asynchronous, l%diff_fileset, changes, status, rev1=l%diff_rev1, &
                                               verbose=l%diff_verbose, buffer=buffer)
        else if (allocated(l%diff_rev2)) then
            call version_control_diff_internal(l%diff_asynchronous, l%diff_fileset, changes, status, rev2=l%diff_rev2, &
                                               verbose=l%diff_verbose, buffer=buffer)
        else
            call version_control_diff_internal(l%diff_asynchronous, l%diff_fileset, changes, status, verbose=l%diff_verbose, &
                                               buffer=buffer)
        end if
    end subroutine version_control_diff_revert

    !> Query the user for a revision using PROMPT.
    !> FILES may specify a file set to restrict the revisions to.  B is a VERSION_CONTROL
    !> backend.  DEFAULT and INITIAL_INPUT are handled as defined by
    !> `completing_read'.  Port of vc.el:vc-read-revision (one revision).
    recursive subroutine version_control_read_revision(prompt, revision, status, files, b, default, initial_input)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: revision
        type(status_type), intent(out) :: status
        type(string_type), intent(in), optional :: files(:)
        integer, intent(in), optional :: b
        character(len=*), intent(in), optional :: default, initial_input
        type(version_control_fileset_type) :: fs
        type(string_type), allocatable :: fl(:), table(:)
        type(value_type), allocatable :: names(:)
        integer :: backend, k

        status = status_type()
        backend = 0
        if (present(b)) backend = b

        if (present(files)) then
            fl = files
            if (backend == 0 .and. size(fl) > 0) backend = version_control_backend(fl(1)%text)
        else
            call version_control_deduce_fileset(fs, status, .true.)
            if (.not. status%ok()) return
            fl = fs%files
            backend = fs%backend
        end if

        if (backend /= 0) then
            if (associated(version_control_backends(backend)%revision_completion_table)) then
                call version_control_backends(backend)%revision_completion_table(fl, table)
                allocate (names(size(table)))

                do k = 1, size(table)
                    names(k) = value_type(text=table(k)%text)
                end do

                call completing_read(prompt, completion_list_table(make_list(names)), revision, status, &
                                     initial_input=initial_input, history="version_control_revision_history", default=default)
                return
            end if
        end if

        call read_string(prompt, revision, status, initial_input=initial_input, default=default)
    end subroutine version_control_read_revision

    !> Read the revisions to compare for `version_control_version_diff' from the
    !> minibuffer: REV1 (default: the previous revision of a single file)
    !> and REV2 (default: the current source), each "" for none.  Port of
    !> vc.el:vc-diff-build-argument-list-internal.
    recursive subroutine version_control_diff_build_argument_list_internal(fs, rev1, rev2, status)
        type(version_control_fileset_type), intent(in) :: fs
        character(len=:), allocatable, intent(out) :: rev1, rev2
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: rev1_default, symbol_revision, rev1_prompt
        logical :: found, has_default

        has_default = .false.
        rev1_default = ""

        ! someday we may be able to do revision completion on non-singleton
        ! filesets, but not yet.
        if (size(fs%files) == 1) then
            ! if the file is not locked, use previous revision and current
            ! source as defaults
            if (associated(version_control_backends(fs%backend)%previous_revision)) then
                call version_control_symbolic_working_revision(fs%files(1)%text, symbol_revision, found, fs%backend)

                if (found) then
                    call version_control_backends(fs%backend)%previous_revision(fs%files(1)%text, symbol_revision, rev1_default, &
                                                                                has_default)
                    if (has_default .and. len(rev1_default) == 0) has_default = .false.
                end if
            end if
        end if

        ! construct argument list
        if (has_default) then
            rev1_prompt = "Older revision (default "//rev1_default//"): "
            call version_control_read_revision(rev1_prompt, rev1, status, fs%files, fs%backend, default=rev1_default)
        else
            call version_control_read_revision("Older revision: ", rev1, status, fs%files, fs%backend)
        end if

        if (.not. status%ok()) return
        call version_control_read_revision("Newer revision (default current source): ", rev2, status, fs%files, fs%backend)
    end subroutine version_control_diff_build_argument_list_internal

    !> Report diffs between revisions REV1 and REV2 in the repository history.
    !> This compares two revisions of the current fileset.
    !> If REV1 is "", it defaults to the previous revision, i.e. revision
    !> before the last commit.
    !> If REV2 is "", it defaults to the work tree, i.e. the current
    !> state of each file in the fileset.  Port of vc.el:vc-version-diff.
    recursive subroutine version_control_version_diff(rev1, rev2, status, interactive)
        character(len=*), intent(in) :: rev1, rev2
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: interactive
        type(version_control_fileset_type) :: fs
        logical :: changes

        status = status_type()

        if (len(rev1) == 0 .and. len(rev2) > 0) then
            call set_error(status, GENERIC_ERROR_CODE, "Not a valid revision range")
            return
        end if

        call version_control_deduce_fileset(fs, status, .true.)
        if (.not. status%ok()) return
        call diff_revisions(version_control_allow_asynchronous_diff, fs, rev1, rev2, option(interactive), changes, status)
    end subroutine version_control_version_diff

    !> `version_control_diff_internal' with REV1 and REV2 ("" for none).  Not a port.
    recursive subroutine diff_revisions(asynchronous, fs, rev1, rev2, verbose, changes, status, buffer)
        logical, intent(in) :: asynchronous, verbose
        type(version_control_fileset_type), intent(in) :: fs
        character(len=*), intent(in) :: rev1, rev2
        logical, intent(out) :: changes
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: buffer

        if (len(rev1) > 0 .and. len(rev2) > 0) then
            call version_control_diff_internal(asynchronous, fs, changes, status, rev1, rev2, verbose, buffer)
        else if (len(rev1) > 0) then
            call version_control_diff_internal(asynchronous, fs, changes, status, rev1=rev1, verbose=verbose, buffer=buffer)
        else if (len(rev2) > 0) then
            call version_control_diff_internal(asynchronous, fs, changes, status, rev2=rev2, verbose=verbose, buffer=buffer)
        else
            call version_control_diff_internal(asynchronous, fs, changes, status, verbose=verbose, buffer=buffer)
        end if
    end subroutine diff_revisions

    ! Not a port.
    logical function option(flag)
        logical, intent(in), optional :: flag

        option = .false.
        if (present(flag)) option = flag
    end function option

    !> The fileset of the root directory of the current VERSION_CONTROL tree: the
    !> backend deduced, or the one responsible for a directory read with
    !> DESCRIPTION (vc--with-backend-in-rootdir), and that directory.  Sets
    !> `default_directory' to the root, as the macro binds it (the caller
    !> restores SAVED_DIRECTORY).  Port of vc.el:vc--with-backend-in-rootdir.
    recursive subroutine with_backend_in_rootdir(description, fs, saved_directory, status)
        character(len=*), intent(in) :: description
        type(version_control_fileset_type), intent(out) :: fs
        character(len=:), allocatable, intent(out) :: saved_directory
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: rootdir
        logical :: found

        saved_directory = default_directory()
        fs%backend = version_control_deduce_backend()

        if (fs%backend /= 0) then
            call version_control_execute_root(fs%backend, default_directory(), rootdir, found, status)
            if (.not. status%ok()) return
        else
            call read_file_name("Directory for "//description//": ", rootdir, status, default_filename=default_directory(), &
                                mustmatch=REQUIRE_MATCH_NO)
            if (.not. status%ok()) return
            rootdir = file_name_as_directory(expand(rootdir))
            fs%backend = version_control_responsible_backend(rootdir, status)

            if (.not. status%ok() .or. fs%backend == 0) then
                call set_error(status, GENERIC_ERROR_CODE, "Directory is not version controlled")
                return
            end if
        end if

        buffers(current)%directory = rootdir
        call one(fs%files, rootdir)
    end subroutine with_backend_in_rootdir

    !> Report diffs between REV1 and REV2 revisions of the whole tree.
    !> Port of vc.el:vc-root-version-diff.
    recursive subroutine version_control_root_version_diff(rev1, rev2, status, interactive)
        character(len=*), intent(in) :: rev1, rev2
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: interactive
        type(version_control_fileset_type) :: fs
        type(handle_type) :: old
        type(status_type) :: st
        character(len=:), allocatable :: saved
        logical :: changes

        status = status_type()

        ! This is a mix of `vc-root-diff' and `vc-version-diff'
        if (len(rev1) == 0 .and. len(rev2) > 0) then
            call set_error(status, GENERIC_ERROR_CODE, "Not a valid revision range")
            return
        end if

        old = current_buffer()
        call with_backend_in_rootdir("VC root-diff", fs, saved, status)
        if (status%ok()) call diff_revisions(version_control_allow_asynchronous_diff, fs, rev1, rev2, option(interactive), &
                                             changes, status)

        if (is_buffer_live(old)) then
            call set_buffer(old, st)
            buffers(current)%directory = saved
        end if
    end subroutine version_control_root_version_diff

    !> Display diffs between file revisions.
    !> Normally this compares the currently selected fileset with their
    !> working revisions.  With a prefix argument HISTORIC, it reads two revision
    !> designators specifying which revisions to compare.
    !>
    !> Optional argument NOT_ESSENTIAL true means it is okay to say no to
    !> saving the buffer.
    !> Optional argument FILESET, if present, overrides the fileset.
    !> Port of vc.el:vc-diff.
    recursive subroutine version_control_diff(status, historic, not_essential, fileset, interactive)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: historic, not_essential, interactive
        type(version_control_fileset_type), intent(in), optional :: fileset
        type(version_control_fileset_type) :: fs
        character(len=:), allocatable :: rev1, rev2
        logical :: changes

        status = status_type()

        if (option(historic)) then
            call version_control_deduce_fileset(fs, status, .true.)
            if (.not. status%ok()) return
            call version_control_diff_build_argument_list_internal(fs, rev1, rev2, status)
            if (.not. status%ok()) return
            call version_control_version_diff(rev1, rev2, status, .true.)
            return
        end if

        if (present(fileset)) then
            fs = fileset
        else
            call version_control_deduce_fileset(fs, status, .true.)
            if (.not. status%ok()) return
        end if

        call version_control_buffer_synchronize_fileset(fs, status, not_essential)
        if (.not. status%ok()) return
        call version_control_diff_internal(version_control_allow_asynchronous_diff, fs, changes, status, &
                                           verbose=option(interactive))
    end subroutine version_control_diff

    !> Call `version_control_buffer_synchronize' for most buffers visiting files in FILESET.
    !> NOT_ESSENTIAL means it is okay to continue if the user says not to save.
    !>
    !> For files named explicitly in FILESET, this function always syncs their
    !> buffers.  By contrast, for directories named in FILESET, its behavior
    !> depends on MISSING_IN_DIRECTORIES.  For each directory named in FILESET, it
    !> considers buffers visiting any file contained within that directory or
    !> its subdirectories.  If MISSING_IN_DIRECTORIES is false, it syncs only those
    !> buffers whose files exist on disk.  Otherwise it syncs all of them.
    !> Port of vc.el:vc-buffer-sync-fileset.
    recursive subroutine version_control_buffer_synchronize_fileset(fs, status, not_essential, missing_in_directories)
        type(version_control_fileset_type), intent(in) :: fs
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: not_essential, missing_in_directories
        type(string_type), allocatable :: directories(:)
        type(handle_type), allocatable :: buffer_handles(:), all(:)
        type(handle_type) :: buffer, old
        type(status_type) :: st
        character(len=:), allocatable :: file
        integer :: k, j
        logical :: in_directory

        status = status_type()
        allocate (directories(0), buffer_handles(0))

        do k = 1, size(fs%files)
            if (is_file_directory(fs%files(k)%text)) then
                call append_string(directories, file_name_as_directory(fs%files(k)%text))
            else
                buffer = get_file_buffer(fs%files(k)%text)
                if (buffer%index /= 0) buffer_handles = [buffer_handles, buffer]
            end if
        end do

        if (size(directories) > 0) then
            all = buffer_list()

            do k = 1, size(all)
                if (.not. allocated(buffers(all(k)%index)%filename)) cycle
                file = buffers(all(k)%index)%filename
                in_directory = .false.

                do j = 1, size(directories)
                    if (option(not_essential)) then
                        ! For speed (bug#79137).
                        if (index(file, directories(j)%text) == 1) in_directory = .true.
                    else
                        if (is_file_in_directory(file, directories(j)%text)) in_directory = .true.
                    end if
                end do

                if (.not. in_directory) cycle
                if (.not. (option(missing_in_directories) .or. file_exists(file))) cycle
                if (any(buffer_handles%index == all(k)%index)) cycle
                buffer_handles = [buffer_handles, all(k)]
            end do
        end if

        old = current_buffer()

        do k = 1, size(buffer_handles)
            call set_buffer(buffer_handles(k), st)
            call version_control_buffer_synchronize(status, not_essential)
            if (.not. status%ok()) exit
        end do

        if (is_buffer_live(old)) call set_buffer(old, st)
    end subroutine version_control_buffer_synchronize_fileset

    !> Display diffs between VC-controlled whole tree revisions.
    !> Normally, this compares the tree corresponding to the current
    !> fileset with the working revision.
    !> With a prefix argument HISTORIC, prompt for two revision
    !> designators specifying which revisions to compare.
    !>
    !> Optional argument NOT_ESSENTIAL true means it is okay to say no to
    !> saving the buffer.  Port of vc.el:vc-root-diff.
    recursive subroutine version_control_root_diff(historic, status, not_essential, interactive)
        logical, intent(in) :: historic
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: not_essential, interactive
        type(version_control_fileset_type) :: fs, arguments
        type(handle_type) :: old
        type(status_type) :: st
        character(len=:), allocatable :: saved, rev1, rev2, root
        logical :: changes, found

        status = status_type()

        if (historic) then
            ! We want the diff for the VERSION_CONTROL root dir.
            arguments%backend = version_control_deduce_backend()

            if (arguments%backend == 0) then
                arguments%backend = version_control_responsible_backend(default_directory(), status)
                if (.not. status%ok()) return
            end if

            call version_control_execute_root(arguments%backend, default_directory(), root, found, status)
            if (.not. status%ok()) return
            call one(arguments%files, root)
            call version_control_diff_build_argument_list_internal(arguments, rev1, rev2, status)
            if (.not. status%ok()) return
            call version_control_root_version_diff(rev1, rev2, status, .true.)
            return
        end if

        old = current_buffer()
        ! VERSION_CONTROL diff for the root directory produces output that is relative
        ! to it.  Bind default-directory to the root directory here, this
        ! way the *vc-diff* buffer is setup correctly, so relative file
        ! names work.
        call with_backend_in_rootdir("VC root-diff", fs, saved, status)

        if (status%ok()) then
            call version_control_buffer_synchronize_fileset(fs, status, not_essential)
            if (status%ok()) call version_control_diff_internal(version_control_allow_asynchronous_diff, fs, changes, status, &
                                                                verbose=option(interactive))
        end if

        if (is_buffer_live(old)) then
            call set_buffer(old, st)
            buffers(current)%directory = saved
        end if
    end subroutine version_control_root_diff

    ! ------------------------------------------------------------------
    ! Other revisions
    ! ------------------------------------------------------------------

    !> Visit revision REVISION of the current file in another window.
    !> If the current file is named `F', the revision is named `F.~REVISION~'.
    !> If `F.~REVISION~' already exists, use it instead of checking it out again.
    !>
    !> If this command needs to split the current window, it by default obeys
    !> the user options `split_height_threshold' and `split_width_threshold',
    !> when it decides whether to split the window horizontally or vertically.
    !> Port of vc.el:vc-revision-other-window.
    recursive subroutine version_control_revision_other_window(revision_name, status)
        character(len=*), intent(in) :: revision_name
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file, revision
        type(handle_type) :: buffer
        logical :: found

        call version_control_ensure_version_control_buffer(status)
        if (.not. status%ok()) return
        file = buffers(current)%filename

        if (len(revision_name) == 0) then
            call version_control_symbolic_working_revision(file, revision, found)
        else
            revision = revision_name
        end if

        call version_control_find_revision(file, revision, buffer, status)
        if (.not. status%ok()) return
        call switch_to_buffer_other_window(buffer, status)
    end subroutine version_control_revision_other_window

    !> Read REVISION of FILE into a buffer and give the buffer as RESULT.
    !> Use B as the VERSION_CONTROL backend if specified.  Port of vc.el:vc-find-revision
    !> with vc-find-revision-save (saves the buffer to the file).
    recursive subroutine version_control_find_revision(file, revision, result, status, b)
        character(len=*), intent(in) :: file, revision
        type(handle_type), intent(out) :: result
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: b
        character(len=:), allocatable :: automatic_backup, filename, message_text
        type(handle_type) :: filebuf, old, outbuf
        type(status_type) :: st
        integer :: backend, k

        result = null_handle()
        status = status_type()

        if (version_control_find_revision_no_save) then
            call set_error(status, GENERIC_ERROR_CODE, "version_control_find_revision_no_save is not ported yet")
            return
        end if

        automatic_backup = version_control_version_backup_file_name(file, revision)
        filebuf = get_file_buffer(file)
        if (filebuf%index == 0) filebuf = current_buffer()
        filename = version_control_version_backup_file_name(file, revision, manual=.true.)
        backend = 0
        if (present(b)) backend = b
        if (backend == 0) backend = version_control_backend(file)

        if (.not. file_exists(filename)) then
            if (file_exists(automatic_backup)) then
                call rename_file(automatic_backup, filename, status)
                if (.not. status%ok()) return
            else
                call message("Checking out "//filename//"...")
                old = current_buffer()
                call set_buffer(filebuf, st)
                ! with-temp-file FILENAME
                call get_or_make_buffer(generate_new_buffer_name(" *temp file*"), outbuf, status)
                if (.not. status%ok()) return

                if (.not. associated(version_control_backends(backend)%find_revision)) then
                    call version_control_not_supported(status, "find-revision", backend)
                else
                    ! Change buffer to get local value of
                    ! vc-checkout-switches.
                    call version_control_backends(backend)%find_revision(file, revision, outbuf, status)
                end if

                if (status%ok()) then
                    call set_buffer(outbuf, st)
                    call write_region(filename, .false., status, point_minimum(), point_maximum(), message_text)
                end if

                call close_buffer(outbuf, st)

                if (.not. status%ok()) then
                    if (file_exists(filename)) call delete_file(filename, st)
                    call set_buffer(old, st)
                    return
                end if

                call set_buffer(filebuf, st)
                call version_control_mode_line(file)
                call set_buffer(old, st)
                call message("Checking out "//filename//"...done")
            end if
        end if

        call find_file_noselect(filename, result, status)
        if (.not. status%ok()) return
        old = current_buffer()
        call set_buffer(result, st)
        call set_version_control_parent_buffer(filebuf, "")
        call make_local_entry(k)
        locals(k)%has_override = .true.
        locals(k)%override%backend = backend
        call one(locals(k)%override%files, expand(file))
        locals(k)%revision = revision
        call set_buffer(old, st)
    end subroutine version_control_find_revision

    ! ------------------------------------------------------------------
    ! Reverting
    ! ------------------------------------------------------------------

    !> Revert working copies of the selected fileset to their repository contents.
    !> This asks for confirmation if the buffer contents are not identical
    !> to the working revision (except for keyword expansion).  Port of
    !> vc.el:vc-revert.
    recursive subroutine version_control_revert(status)
        type(status_type), intent(out) :: status
        type(version_control_fileset_type) :: fs
        type(handle_type) :: diff_buffer, buffer
        type(status_type) :: st
        character(len=:), allocatable :: names, string
        character(len=24) :: count_text
        logical :: queried, yes, changes, show
        integer :: k

        status = status_type()
        call version_control_deduce_fileset(fs, status)
        if (.not. status%ok()) return
        queried = .false.
        diff_buffer = null_handle()

        ! If any of the files is visited by the current buffer, make sure
        ! buffer is saved.  If the user says `no', abort since we cannot
        ! show the changes and ask for confirmation to discard them.
        if (len(buffer_file_name()) > 0) then
            if (size(fs%files) == 0 .or. has_file(buffer_file_name(), fs%files)) then
                call version_control_buffer_synchronize(status)
                if (.not. status%ok()) return
            end if
        end if

        call save_some_buffers(.false., .false., status, in_fileset)
        if (.not. status%ok()) return
        names = ""

        do k = 1, size(fs%files)
            buffer = get_file_buffer(fs%files(k)%text)

            if (is_buffer_modified(buffer)) then
                if (len(names) > 0) then
                    names = buffers(buffer%index)%name//", "//names
                else
                    names = buffers(buffer%index)%name
                end if
            end if

            if (is_version_control_up_to_date(fs%files(k)%text)) then
                call read_yes_or_no(fs%files(k)%text//" seems up-to-date.  Revert anyway? ", yes, status)
                if (.not. status%ok()) return

                if (.not. yes) then
                    call set_error(status, GENERIC_ERROR_CODE, "Revert canceled")
                    return
                end if

                queried = .true.
            end if
        end do

        if (len(names) > 0) then
            call set_error(status, GENERIC_ERROR_CODE, "Cannot revert with these buffers unsaved: "//names)
            return
        end if

        if (.not. version_control_revert_show_diff%is_off()) then
            call get_or_make_buffer(generate_new_buffer_name("*vc-diff*"), diff_buffer, status)
            if (.not. status%ok()) return
            call version_control_diff_internal(version_control_allow_asynchronous_revert, fs, changes, status, buffer=diff_buffer)
            show = changes
        else
            ! Avoid querying the user again.
            show = .not. queried
        end if

        if (status%ok() .and. show) then
            string = version_control_delistify(fs%files)

            if (len(string) >= 50) then
                if (size(fs%files) == 1) then
                    string = "1 file"
                else
                    write (count_text, '(i0, " files")') size(fs%files)
                    string = trim(count_text)
                end if
            end if

            call read_yes_or_no("Discard changes in "//string//"? ", yes, status)
            if (status%ok() .and. .not. yes) call set_error(status, GENERIC_ERROR_CODE, "Revert cancelled")
        end if

        if (diff_buffer%index /= 0) then
            if (is_buffer_live(diff_buffer)) call quit_windows_on(diff_buffer, &
                                                                  version_control_revert_show_diff%choice &
                                                                  == VERSION_CONTROL_REVERT_SHOW_DIFF_KILL, st)
        end if

        if (.not. status%ok()) return
        call version_control_revert_files(fs%backend, fs%files, status)

    contains

        !> save-some-buffers's TEST: the buffer visits one of the files.
        logical function in_fileset()
            in_fileset = .false.
            if (len(buffer_file_name()) > 0) in_fileset = has_file(buffer_file_name(), fs%files)
        end function in_fileset
    end subroutine version_control_revert

    !> Return name of backup file for revision REVISION of FILE.
    !> If version backups should be used for FILE, and there exists
    !> such a backup for REVISION or the working revision of file, give
    !> its name as BACKUP; otherwise FOUND is false.  Port of
    !> vc.el:vc-version-backup-file.
    subroutine version_control_version_backup_file(file, backup, found, revision)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: backup
        logical, intent(out) :: found
        character(len=*), intent(in), optional :: revision
        integer :: b

        backup = ""
        found = .false.
        b = version_control_backend(file)
        if (b == 0) return
        if (.not. is_version_control_make_version_backups(b, file)) return
        backup = version_control_version_backup_file_name(file, revision)

        if (file_exists(backup)) then
            found = .true.
            return
        end if

        ! there is no automatic backup, but maybe the user made one manually
        backup = version_control_version_backup_file_name(file, revision, manual=.true.)
        found = file_exists(backup)
    end subroutine version_control_version_backup_file

    !> Revert FILE back to the repository working revision it was based on.
    !> If FILE is a directory, revert all files inside that directory.
    !> Port of vc.el:vc-revert-file.
    recursive subroutine version_control_revert_file(file, status)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: flist(:)
        type(status_type) :: st
        character(len=:), allocatable :: backup_file, root
        logical :: directory, found
        integer :: b, state, k

        status = status_type()
        call with_properties_begin(one_string(file), flist)
        directory = is_file_directory(file)
        found = .false.
        if (.not. directory) call version_control_version_backup_file(file, backup_file, found)

        if (found) then
            call copy_file(backup_file, file, status, ok_if_already_exists=.true.)
            if (.not. status%ok()) return
            call version_control_delete_automatic_version_backups(file)
        end if

        if (directory) then
            b = version_control_responsible_backend(file, status)
            if (.not. status%ok()) return
        else
            b = version_control_backend(file)
        end if

        if (b == 0) then
            call set_error(status, GENERIC_ERROR_CODE, "File "//file//" is not under version control")
            return
        end if

        if (.not. associated(version_control_backends(b)%revert)) then
            call version_control_not_supported(status, "revert", b)
            return
        end if

        if (found) then
            call version_control_backends(b)%revert(file, status, backup_file)
        else
            call version_control_backends(b)%revert(file, status)
        end if

        if (.not. status%ok()) return
        state = version_control_state(file)

        do k = 1, size(flist)
            call set_after_revert(flist(k)%text, state)
        end do

        call version_control_root_directory(root, found, st)
        call version_control_resynch_buffer(file, root, status, keep=.true., noquery=VERSION_CONTROL_NOQUERY_YES)
    end subroutine version_control_revert_file

    !> Set FILE's properties after reverting it from STATE, those the
    !> backend did not set (with-vc-properties's settings): an added file
    !> becomes unregistered, others up to date; the checkout time is the
    !> modification time.  Not a port.
    subroutine set_after_revert(file, state)
        character(len=*), intent(in) :: file
        integer, intent(in) :: state
        type(file_status_type) :: attribute
        logical :: found

        if (state == VERSION_CONTROL_ADDED .and. .not. version_control_touched(VERSION_CONTROL_PROPERTY_BACKEND)) then
            call version_control_file_set_backend(file, 0)
        end if

        if (.not. version_control_touched(VERSION_CONTROL_PROPERTY_STATE)) then
            if (state == VERSION_CONTROL_ADDED) then
                call version_control_file_set_state(file, VERSION_CONTROL_UNREGISTERED)
            else if (state /= VERSION_CONTROL_STATE_UNKNOWN) then
                call version_control_file_set_state(file, VERSION_CONTROL_UP_TO_DATE)
            end if
        end if

        if (.not. version_control_touched(VERSION_CONTROL_PROPERTY_CHECKOUT_TIME)) then
            call file_attributes(file, attribute, found)

            if (found) then
                call version_control_file_set_checkout_time(file, attribute%mtime_second, attribute%mtime_nsec)
            else
                call version_control_file_set_checkout_time(file)
            end if
        end if
    end subroutine set_after_revert

    !> Revert each of FILES to the repository working version it was based on.
    !> For entries in FILES that are directories, revert all files inside them.
    !> Port of vc.el:vc-revert-files.
    recursive subroutine version_control_revert_files(b, files, status)
        integer, intent(in) :: b
        type(string_type), intent(in) :: files(:)
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: flist(:)
        integer, allocatable :: states(:)
        type(status_type) :: st
        character(len=:), allocatable :: root
        logical :: found
        integer :: k

        status = status_type()
        if (size(files) == 0) return
        call message("Reverting "//version_control_delistify(files)//"...")

        if (.not. associated(version_control_backends(b)%revert_files)) then
            do k = 1, size(files)
                call version_control_revert_file(files(k)%text, status)
                if (.not. status%ok()) return
            end do
        else
            call with_properties_begin(files, flist)
            ! Use `vc-file-getprop' directly here because we may be
            ! handling very many files and do not want to hit the disk.
            allocate (states(size(flist)))

            do k = 1, size(flist)
                states(k) = version_control_file_get_state(flist(k)%text)
            end do

            call version_control_backends(b)%revert_files(files, status)
            if (.not. status%ok()) return

            do k = 1, size(flist)
                call set_after_revert(flist(k)%text, states(k))
            end do

            call version_control_root_directory(root, found, st)

            do k = 1, size(files)
                call version_control_resynch_buffer(files(k)%text, root, status, keep=.true., noquery=VERSION_CONTROL_NOQUERY_YES)
                if (.not. status%ok()) return
            end do
        end if

        call message("Reverting "//version_control_delistify(files)//"...done")
    end subroutine version_control_revert_files

    ! ------------------------------------------------------------------
    ! Deleting and renaming
    ! ------------------------------------------------------------------

    !> Delete file and mark it as such in the version control system.
    !> If called interactively, read FILES, defaulting to the current
    !> buffer's file name if it's under version control.
    !> NOCONFIRM true means don't
    !> prompt to confirm deletion.
    !> For some VERSION_CONTROLS, FILES can include directories.
    !> These are recursively deleted.  Port of vc.el:vc-delete-file.
    recursive subroutine version_control_delete_file(files_in, status, noconfirm)
        type(string_type), intent(in) :: files_in(:)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: noconfirm
        type(string_type), allocatable :: files(:), group(:)
        type(handle_type) :: buffer, old
        type(status_type) :: st
        character(len=:), allocatable :: saved, root
        character(len=64) :: prompt
        logical :: yes, found
        integer, allocatable :: backends(:)
        integer :: k, j, b, state

        status = status_type()
        allocate (files(size(files_in)), backends(size(files_in)))

        do k = 1, size(files_in)
            files(k)%text = expand(files_in(k)%text)
        end do

        do k = 1, size(files)
            buffer = get_file_buffer(files(k)%text)

            if (is_file_directory(files(k)%text)) then
                b = version_control_responsible_backend(files(k)%text, status)
                if (.not. status%ok()) return
            else
                b = version_control_backend(files(k)%text)
            end if

            backends(k) = b

            if (b /= 0) then
                if (.not. associated(version_control_backends(b)%delete_file)) then
                    call set_error(status, GENERIC_ERROR_CODE, "Deleting files under "//version_control_backend_name(b)// &
                                   " is not supported in VC")
                    return
                end if
            end if

            if (is_buffer_modified(buffer)) then
                call set_error(status, GENERIC_ERROR_CODE, "Please save or undo your changes before deleting "// &
                               files(k)%text)
                return
            end if

            state = version_control_state(files(k)%text)

            if (state == VERSION_CONTROL_EDITED) then
                call set_error(status, GENERIC_ERROR_CODE, "Please commit or undo your changes before deleting "// &
                               files(k)%text)
                return
            end if

            if (state == VERSION_CONTROL_CONFLICT) then
                call set_error(status, GENERIC_ERROR_CODE, "Please resolve the conflicts before deleting "//files(k)%text)
                return
            end if
        end do

        if (.not. option(noconfirm)) then
            if (size(files) > 1) then
                write (prompt, '("Really want to delete these ", i0, " files?")') size(files)
                call read_y_or_n(trim(prompt)//" ", yes, status)
            else
                call read_y_or_n("Really want to delete "//file_name_nondirectory(files(1)%text)//"? ", yes, status)
            end if

            if (.not. status%ok()) return

            if (.not. yes) then
                call set_error(status, GENERIC_ERROR_CODE, "Abort!")
                return
            end if
        end if

        do k = 1, size(files)
            buffer = get_file_buffer(files(k)%text)

            if (.not. (is_file_directory(files(k)%text) .or. .not. create_backup_files .or. &
                       is_backup_file_name(files(k)%text) .or. .not. file_exists(files(k)%text))) then
                old = current_buffer()

                if (buffer%index == 0) then
                    call find_file_noselect(files(k)%text, buffer, status)
                    if (.not. status%ok()) return
                end if

                call set_buffer(buffer, st)

                block
                    logical :: saved_inhibited
                    integer(int32) :: modes
                    saved_inhibited = buffers(current)%backup_inhibited
                    buffers(current)%backup_inhibited = .false.
                    call backup_buffer(modes)
                    buffers(current)%backup_inhibited = saved_inhibited
                end block

                call set_buffer(old, st)
            end if

            if (backends(k) == 0) then
                call post_backend_deletion(files(k)%text)
                if (.not. status%ok()) return
            end if
        end do

        ! The files by backend, in order.
        do k = 1, size(files)
            b = backends(k)
            if (b == 0 .or. b == -1) cycle
            allocate (group(0))

            do j = k, size(files)
                if (backends(j) == b) then
                    call append_string(group, files(j)%text)
                    backends(j) = -1
                end if
            end do

            if (associated(version_control_backends(b)%delete_files)) then
                saved = default_directory()
                call version_control_root_directory(root, found, st, b)
                if (found) buffers(current)%directory = root
                call version_control_backends(b)%delete_files(group, status)
                buffers(current)%directory = saved
                if (.not. status%ok()) return

                do j = 1, size(group)
                    call post_backend_deletion(group(j)%text)
                end do
            else
                do j = 1, size(group)
                    ! Changing default directory like this has the problem
                    ! that the containing directory for FILE may no longer
                    ! exists if all files within it are already `missing',
                    ! say.
                    saved = default_directory()
                    buffers(current)%directory = file_name_directory(group(j)%text)
                    call version_control_backends(b)%delete_file(group(j)%text, status)
                    buffers(current)%directory = saved
                    if (.not. status%ok()) return
                    call post_backend_deletion(group(j)%text)
                end do
            end if

            deallocate (group)
        end do

    contains

        !> Things to do after calling the backend's `delete-file' or
        !> `delete-files' function on FILE, or after determining we're not
        !> going to call any such function.
        recursive subroutine post_backend_deletion(file)
            character(len=*), intent(in) :: file
            type(file_status_type) :: attribute
            logical :: exists

            ! For the case of unregistered files, or if the backend didn't
            ! actually delete the file.
            call file_attributes(file, attribute, exists, nofollow=.true.)

            if (exists) then
                if (is_file_directory(file) .and. .not. is_file_symlink(file)) then
                    call delete_directory(file, st, .true.)
                else
                    call delete_file(file, st)
                end if
            end if

            ! Forget what VERSION_CONTROL knew about the file.
            call version_control_file_clearprops(file)
            ! Make sure the buffer is closed and *vc-dir* buffers are
            ! updated after this.
            call version_control_root_directory(root, found, st)
            call version_control_resynch_buffer(file, root, st, keep=.false., noquery=VERSION_CONTROL_NOQUERY_YES)
        end subroutine post_backend_deletion
    end subroutine version_control_delete_file

    !> Rename file OLD to NEW in both working tree and repository.
    !> When called interactively, read OLD and NEW, defaulting OLD to the
    !> current buffer's file name if it's under version control.
    !> If NEW is a directory name, rename FILE to a like-named file under NEW.
    !> For NEW to be recognized as a directory name, it should end in a slash.
    !> Signal a `file_already_exists' error if a file NEW already exists unless
    !> OK_IF_ALREADY_EXISTS is true.  Port of vc.el:vc-rename-file.
    recursive subroutine version_control_rename_file(old_in, new_in, status, ok_if_already_exists)
        character(len=*), intent(in) :: old_in, new_in
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: ok_if_already_exists
        character(len=:), allocatable :: old, new, old_base, saved
        type(handle_type) :: oldbuf, newbuf, cbuf
        type(status_type) :: st
        type(string_type), allocatable :: one_file(:)
        logical :: dirp, any_registered, saved_ro
        integer :: state, b

        status = status_type()
        new = new_in
        ! in CL I would have said (setq new (merge-pathnames new old))
        old_base = file_name_nondirectory(old_in)
        if (len(old_base) > 0 .and. len(file_name_nondirectory(new)) == 0) new = new//old_base
        old = expand(old_in)
        new = expand(new)
        oldbuf = get_file_buffer(old)
        saved = default_directory()
        buffers(current)%directory = file_name_directory(old)
        dirp = is_file_directory(old)

        body: block
            if (is_buffer_modified(oldbuf)) then
                call set_error(status, GENERIC_ERROR_CODE, "Please save files before moving them")
                exit body
            end if

            newbuf = get_file_buffer(new)

            if (newbuf%index /= 0) then
                call set_error(status, GENERIC_ERROR_CODE, "Already editing new file name")
                exit body
            end if

            ! Handle OK-IF-ALREADY-EXISTS here because it's not part of the VERSION_CONTROL
            ! backend `delete-file' API.
            if (file_exists(new)) then
                if (option(ok_if_already_exists)) then
                    call one(one_file, new)
                    call version_control_delete_file(one_file, status, .true.)
                    if (.not. status%ok()) exit body
                else
                    call set_error(status, FILE_ALREADY_EXISTS_CODE, "File exists, "//new)
                    exit body
                end if
            end if

            if (.not. dirp) then
                state = version_control_state(old)

                if (.not. (state == VERSION_CONTROL_UP_TO_DATE .or. state == VERSION_CONTROL_EDITED .or. state &
                           == VERSION_CONTROL_ADDED)) then
                    if (state == VERSION_CONTROL_LOCKED) then
                        call set_error(status, GENERIC_ERROR_CODE, "Please check in files before moving them")
                    else
                        call set_error(status, GENERIC_ERROR_CODE, "Please update files before moving them")
                    end if

                    exit body
                end if
            end if

            ! The rename commands for several VERSION_CONTROLS (at least Bzr, Git and
            ! Mercurial) will fail if asked to move a directory containing
            ! only untracked files.  So skip calling into the backend if OLD
            ! is a directory and we walk through the entirety of it without
            ! finding any VC-managed files.
            any_registered = .true.
            if (dirp) any_registered = version_control_file_tree_walk_any(old)

            if (any_registered) then
                if (dirp) then
                    b = version_control_responsible_backend(old, status)
                    if (.not. status%ok()) exit body
                else
                    b = version_control_backend(old)
                end if

                if (b == 0 .or. .not. associated(version_control_backends(b)%rename_file)) then
                    call version_control_not_supported(status, "rename-file", b)
                    exit body
                end if

                call version_control_backends(b)%rename_file(old, new, status)
                if (.not. status%ok()) exit body
            end if

            call version_control_file_clearprops(old)
            call version_control_file_clearprops(new)

            ! Move the actual file (unless the backend did it already)
            if (file_exists(old)) then
                call rename_file(old, new, status)
                if (.not. status%ok()) exit body
            end if

            ! ?? Renaming a file might change its contents due to keyword
            ! expansion.
            if (oldbuf%index /= 0) then
                cbuf = current_buffer()
                buffers(cbuf%index)%directory = saved
                call set_buffer(oldbuf, st)
                saved_ro = buffers(current)%read_only
                call set_visited_file_name(new, status)
                buffers(current)%read_only = saved_ro
                call version_control_mode_line(new, version_control_backend(new))
                call restore_buffer_modified(.false.)
                call set_buffer(cbuf, st)
                return
            end if
        end block body

        if (current /= 0) buffers(current)%directory = saved
    end subroutine version_control_rename_file

    !> Whether DIRECTORY_NAME contains a VC-managed file (vc-file-tree-walk
    !> returning with `done' at the first).  Not a port.
    recursive logical function version_control_file_tree_walk_any(directory_name) result(found)
        character(len=*), intent(in) :: directory_name
        type(string_type), allocatable :: files(:)

        call version_control_file_tree_walk(directory_name, files, first_only=.true.)
        found = size(files) > 0
    end function version_control_file_tree_walk_any

    !> Walk recursively through DIRECTORY_NAME.
    !> Give the VC-managed files underneath it as FILES (only the first
    !> with FIRST_ONLY).  Port of vc.el:vc-file-tree-walk and
    !> vc-file-tree-walk-internal (FUNCTION collecting the files).
    recursive subroutine version_control_file_tree_walk(directory_name, files, first_only)
        character(len=*), intent(in) :: directory_name
        type(string_type), allocatable, intent(out) :: files(:)
        logical, intent(in), optional :: first_only

        allocate (files(0))
        call walk(expand(directory_name))

    contains

        recursive subroutine walk(file)
            use value_mod, only: list_elements, xstring, value_type
            character(len=*), intent(in) :: file
            type(value_type) :: names
            type(value_type), allocatable :: elements(:)
            type(status_type) :: st
            character(len=:), allocatable :: directory, f, dirf
            integer :: k, j
            logical :: excluded

            if (option(first_only) .and. size(files) > 0) return

            if (.not. is_file_directory(file)) then
                if (version_control_registered(file)) call append_string(files, file)
                return
            end if

            directory = file_name_as_directory(file)
            call directory_files(directory, names, st)
            if (.not. st%ok()) return
            call list_elements(names, elements)

            do k = 1, size(elements)
                f = xstring(elements(k))
                if (f == "." .or. f == "..") cycle
                excluded = .false.

                do j = 1, size(version_control_directory_exclusions)
                    if (trim(version_control_directory_exclusions(j)) == f) excluded = .true.
                end do

                if (excluded) cycle
                dirf = expand(f, directory)
                ! Avoid possible loops.
                if (is_file_symlink(dirf)) cycle
                call walk(dirf)
            end do
        end subroutine walk
    end subroutine version_control_file_tree_walk

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    recursive subroutine execute_version_control_next_action(status)
        type(status_type), intent(out) :: status

        call version_control_next_action(current_prefix_argument%kind /= PREFIX_NONE, status)
    end subroutine execute_version_control_next_action

    recursive subroutine execute_version_control_register(status)
        type(status_type), intent(out) :: status

        call version_control_register(status)
    end subroutine execute_version_control_register

    recursive subroutine execute_version_control_create_repository(status)
        type(status_type), intent(out) :: status
        integer :: b

        b = version_control_read_backend("Create repository for: ", status)
        if (.not. status%ok()) return
        call version_control_create_repository(b, status)
    end subroutine execute_version_control_create_repository

    recursive subroutine execute_version_control_ignore(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: relative_directory, file

        relative_directory = version_control_ignore_base_directory(status)
        if (.not. status%ok()) return
        call read_file_name("File to ignore: ", file, status)
        if (.not. status%ok()) return

        if (is_file_name_absolute(file)) then
            if (is_file_in_directory(file, relative_directory)) file = file_relative_name(file, relative_directory)
        end if

        call version_control_ignore(file, status, relative_directory, current_prefix_argument%kind /= PREFIX_NONE)
    end subroutine execute_version_control_ignore

    recursive subroutine execute_version_control_diff(status)
        type(status_type), intent(out) :: status

        call version_control_diff(status, current_prefix_argument%kind /= PREFIX_NONE, .true., interactive=.true.)
    end subroutine execute_version_control_diff

    recursive subroutine execute_version_control_version_diff(status)
        type(status_type), intent(out) :: status
        type(version_control_fileset_type) :: fs
        character(len=:), allocatable :: rev1, rev2

        call version_control_deduce_fileset(fs, status, .true.)
        if (.not. status%ok()) return
        call version_control_diff_build_argument_list_internal(fs, rev1, rev2, status)
        if (.not. status%ok()) return
        call version_control_version_diff(rev1, rev2, status, .true.)
    end subroutine execute_version_control_version_diff

    recursive subroutine execute_version_control_root_version_diff(status)
        type(status_type), intent(out) :: status
        type(version_control_fileset_type) :: arguments
        character(len=:), allocatable :: rev1, rev2, root
        logical :: found

        arguments%backend = version_control_deduce_backend()

        if (arguments%backend == 0) then
            arguments%backend = version_control_responsible_backend(default_directory(), status)
            if (.not. status%ok()) return
        end if

        call version_control_execute_root(arguments%backend, default_directory(), root, found, status)
        if (.not. status%ok()) return
        call one(arguments%files, root)
        call version_control_diff_build_argument_list_internal(arguments, rev1, rev2, status)
        if (.not. status%ok()) return
        call version_control_root_version_diff(rev1, rev2, status, .true.)
    end subroutine execute_version_control_root_version_diff

    recursive subroutine execute_version_control_root_diff(status)
        type(status_type), intent(out) :: status

        call version_control_root_diff(current_prefix_argument%kind /= PREFIX_NONE, status, .true., .true.)
    end subroutine execute_version_control_root_diff

    recursive subroutine execute_version_control_revision_other_window(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: revision
        type(handle_type) :: old
        type(status_type) :: st

        old = current_buffer()
        call version_control_ensure_version_control_buffer(status)
        if (.not. status%ok()) return
        call version_control_read_revision("Revision to visit (default working revision): ", revision, status, &
                                           one_string(buffers(current)%filename))
        if (.not. status%ok()) return
        call version_control_revision_other_window(revision, status)
        if (.not. status%ok() .and. is_buffer_live(old)) call set_buffer(old, st)
    end subroutine execute_version_control_revision_other_window

    recursive subroutine execute_version_control_revert(status)
        type(status_type), intent(out) :: status

        call version_control_revert(status)
    end subroutine execute_version_control_revert

    recursive subroutine execute_version_control_delete_file(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        type(string_type), allocatable :: files(:)

        if (len(buffer_file_name()) > 0 .and. version_control_backend(buffer_file_name()) /= 0) then
            call read_file_name("VC delete file: ", file, status, default_filename=buffer_file_name(), &
                                mustmatch=REQUIRE_MATCH_YES)
        else
            call read_file_name("VC delete file: ", file, status, mustmatch=REQUIRE_MATCH_YES)
        end if

        if (.not. status%ok()) return
        call one(files, file)
        call version_control_delete_file(files, status)
    end subroutine execute_version_control_delete_file

    recursive subroutine execute_version_control_rename_file(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: old, new

        if (len(buffer_file_name()) > 0 .and. version_control_backend(buffer_file_name()) /= 0) then
            call read_file_name("VC rename file: ", old, status, default_filename=buffer_file_name(), &
                                mustmatch=REQUIRE_MATCH_YES)
        else
            call read_file_name("VC rename file: ", old, status, mustmatch=REQUIRE_MATCH_YES)
        end if

        if (.not. status%ok()) return
        call read_file_name("Rename to: ", new, status)
        if (.not. status%ok()) return
        call version_control_rename_file(old, new, status)
    end subroutine execute_version_control_rename_file

    recursive subroutine execute_version_control_clear_context(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call version_control_clear_context()
    end subroutine execute_version_control_clear_context

    !> `version_control_mode_line' as vc-mode-line-hook.  Not a port.
    subroutine mode_line_hook(file)
        character(len=*), intent(in) :: file

        call version_control_mode_line(file)
    end subroutine mode_line_hook

    !> Register VERSION_CONTROL's commands and options.  Not a port.
    subroutine initialize_version_control()
        if (.not. allocated(version_control_deduce_backend_nonvc_modes)) then
            version_control_deduce_backend_nonvc_modes = [character(len=32) :: "directory_editor_mode", "shell_mode", &
                                                          "eshell_mode", &
                                                          "compilation_mode"]
        end if

        if (.not. allocated(locals)) allocate (locals(0))
        commands%version_control_next_action%execute => execute_version_control_next_action
        commands%version_control_register%execute => execute_version_control_register
        commands%version_control_create_repository%execute => execute_version_control_create_repository
        commands%version_control_ignore%execute => execute_version_control_ignore
        commands%version_control_diff%execute => execute_version_control_diff
        commands%version_control_version_diff%execute => execute_version_control_version_diff
        commands%version_control_root_version_diff%execute => execute_version_control_root_version_diff
        commands%version_control_root_diff%execute => execute_version_control_root_diff
        commands%version_control_revision_other_window%execute => execute_version_control_revision_other_window
        commands%version_control_revert%execute => execute_version_control_revert
        commands%version_control_restore%execute => execute_version_control_revert
        commands%version_control_delete_file%execute => execute_version_control_delete_file
        commands%version_control_rename_file%execute => execute_version_control_rename_file
        commands%version_control_clear_context%execute => execute_version_control_clear_context
        commands%version_control_print_log%execute => execute_version_control_print_log
        commands%version_control_print_root_log%execute => execute_version_control_print_root_log
        ! Tags, branches, pulling and pushing, incoming and outgoing.
        commands%version_control_create_tag%execute => execute_version_control_create_tag
        commands%version_control_create_branch%execute => execute_version_control_create_branch
        commands%version_control_retrieve_tag%execute => execute_version_control_retrieve_tag
        commands%version_control_switch_branch%execute => execute_version_control_switch_branch
        commands%version_control_pull%execute => execute_version_control_pull
        commands%version_control_update%execute => execute_version_control_pull
        commands%version_control_push%execute => execute_version_control_push
        commands%version_control_root_log_incoming%execute => execute_version_control_root_log_incoming
        commands%version_control_log_incoming%execute => execute_version_control_root_log_incoming
        commands%version_control_root_log_outgoing%execute => execute_version_control_root_log_outgoing
        commands%version_control_log_outgoing%execute => execute_version_control_root_log_outgoing
        commands%version_control_print_fileset_branch_log%execute => execute_version_control_print_fileset_branch_log
        commands%version_control_print_root_branch_log%execute => execute_version_control_print_root_branch_log
        commands%version_control_print_branch_log%execute => execute_version_control_print_root_branch_log

        if (.not. version_control_log_finish_functions%member(version_control_shrink_buffer_window)) then
            call version_control_log_finish_functions%prepend(version_control_shrink_buffer_window)
        end if

        version_control_mode_line_hook => mode_line_hook
        call append_locals_clearer(clear_version_control_locals)
        call initialize_version_control_dispatcher()
    end subroutine initialize_version_control
end module version_control_mod
