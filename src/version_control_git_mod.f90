! version_control_git_mod.f90 -- VERSION_CONTROL backend for the git version control system.
! Ported from GNU Emacs 31.1 lisp/vc/vc-git.el.
!
! Copyright (C) 2006-2026 Free Software Foundation, Inc.
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

!> The Git backend of VERSION_CONTROL: whether a file is registered, its state (from
!> `git status --porcelain'), working revision and branch for the mode
!> line, registering, unregistering, reverting, checking out, deleting
!> and renaming files, diffs (diff-index and diff-tree), other revisions
!> (cat-file), the previous revision, revision completion, the ignore
!> file and creating a repository.
!>
!> Committing (`version_control_git_checkin') and its Log Edit mode
!> (`version_control_git_log_edit_mode', with the Sign-Off and No-Verify headers).
!> Tags and branches, pulling and pushing, and the incoming revision.
!>
!> Conflicts: visiting a conflicted file turns on Smerge mode
!> (`version_control_git_find_file_hook'), and saving it without conflict markers
!> marks it resolved (`version_control_git_resolve_conflicts').
!>
!> Annotating: `git blame' (`version_control_git_annotate_command') and its lines, the
!> next revision and the file name changes of a revision.
!>
!> Deferred: the backend functions of the deferred VERSION_CONTROL commands (log,
!> dir-status, merging, stashes,
!> working trees, region history, patches and checking in patches,
!> asynchronous checkins, amending with \[version_control_git_log_edit_toggle_amend]
!> and the published-history checks it needs), the
!> difftool path of `version_control_git_diff' (`version_control_git_diff_switches' VERSION_CONTROL_SWITCHES_UNSPECIFIED), the
!> coding systems of Git's output (UTF-8 is used), remote directories
!> and the other `version_control_git_' options of those features.
module version_control_git_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use data_mod, only: choice_setting_type
    use error_mod, only: status_type, set_error, GENERIC_ERROR_CODE
    use handle_mod, only: handle_type, null_handle
    use value_mod, only: value_type, make_list, xfixnum, vtag, V_INTEGER
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_or_make_buffer, close_buffer, &
                          generate_new_buffer_name, erase_buffer, is_buffer_live
    use editing_functions_mod, only: point, goto_character, point_minimum, point_maximum, buffer_string, is_beginning_of_line, &
                                     is_beginning_of_buffer, position_end_of_line, buffer_substring
    use editing_commands_mod, only: forward_line
    use search_mod, only: string_match, regular_expression_search_forward, looking_at, SEARCH_NOERROR
    use basic_subprograms_mod, only: match_string, version, operator(<=)
    use file_input_output_mod, only: expand_file_name, is_file_directory, is_directory_name, file_exists
    use files_mod, only: file_relative_name, executable_find, revert_buffer_in_progress
    use execute_process_mod, only: execute_process
    use version_control_hooks_mod, only: version_control_backends, version_control_switches_type, &
                                         register_version_control_backend, version_control_find_root, version_control_state, &
                                         version_control_mode_line_state, version_control_working_revision, &
                                         version_control_file_get_git_symbolic_reference, &
                                         version_control_file_set_git_symbolic_reference, version_control_display_status, &
                                         version_control_use_short_revision, VERSION_CONTROL_DISPLAY_STATUS_NO_BACKEND, &
                                         VERSION_CONTROL_SWITCHES_NONE, VERSION_CONTROL_SWITCHES_UNSPECIFIED, &
                                         VERSION_CONTROL_SWITCHES_LIST, VERSION_CONTROL_UP_TO_DATE, VERSION_CONTROL_EDITED, &
                                         VERSION_CONTROL_ADDED, VERSION_CONTROL_REMOVED, VERSION_CONTROL_CONFLICT, &
                                         VERSION_CONTROL_MISSING, VERSION_CONTROL_IGNORED, VERSION_CONTROL_UNREGISTERED, &
                                         VERSION_CONTROL_MODEL_IMPLICIT, VERSION_CONTROL_STATE_UNKNOWN, &
                                         version_control_state_name, version_control_directory_entry_type, &
                                         version_control_directory_fileinfo_type, version_control_directory_update_procedure, &
                                         version_control_file_name_change_type, VERSION_CONTROL_REVISION_GRANULARITY_REPOSITORY
    use version_control_dispatcher_mod, only: version_control_do_command, VERSION_CONTROL_OKSTATUS_UNLESS_SIGNAL, &
                                              VERSION_CONTROL_OKSTATUS_ASYNCHRONOUS, VERSION_CONTROL_DESTINATION_CURRENT, &
                                              VERSION_CONTROL_OKSTATUS_ANY, version_control_resynch_buffer, &
                                              VERSION_CONTROL_NOQUERY_YES
    use time_functions_mod, only: time_type, decoded_time_type, encode_time, real, DECODED_TIME_DAYLIGHT_SAVING_TIME_NO
    use version_control_hooks_mod, only: version_control_resolve_conflicts
    use version_control_mod, only: message_version_control_unresolved_conflicts
    use files_mod, only: after_save_hook
    use simple_merge_mode_mod, only: start_simple_merge_session
    use fortmacs_widget_for_object_collections_mod, only: fortmacs_widget_for_object_collections_part_type
    use version_control_mod, only: version_control_switches
    use version_control_hooks_mod, only: VERSION_CONTROL_REVISION_GRANULARITY_REPOSITORY
    use compile_mod, only: compilation_rule_type
    use hooks_mod, only: hook_type
    use commands_mod, only: commands
    use keymap_mod, only: keymap_set
    use modes_mod, only: major_modes
    use faces_mod, only: named_face_type, display_when, face_attributes
    use font_core_mod, only: font_lock_defaults, font_lock_keyword_type, font_lock_highlight_type, &
                             FONT_LOCK_OVERRIDE_PREPEND
    use font_lock_mod, only: font_lock_keyword, font_lock_highlight
    use log_edit_mod, only: log_edit_toggle_header, log_edit_extract_headers, log_edit_font_lock_keywords, &
                            log_edit_header_type, initialize_log_edit, LOG_EDIT_HEADER_FN
    use string_type_mod, only: string_type, append_string
    implicit none(type, external)
    private

    public :: initialize_version_control_git, version_control_git_registered, version_control_git_state, &
              version_control_git_working_revision, version_control_git_mode_line_string, &
              version_control_git_root, version_control_git_command, version_control_git_previous_revision, &
              version_control_git_revision_table, version_control_git_symbolic_commit, &
              version_control_git_status_to_version_control_state, version_control_git_parse_program_version, &
              version_control_git_program_version, version_control_git_find_file_hook, &
              version_control_git_conflicted_files, get_version_control_git_commands_in_progress
    public :: version_control_git_annotate_command, version_control_git_annotate_time, &
              extract_version_control_git_annotate_revision_at_line, version_control_git_next_revision, &
              version_control_git_file_name_changes
    public :: version_control_git_branches, version_control_git_branch_remotes, version_control_git_create_tag, &
              version_control_git_retrieve_tag, version_control_git_pull, version_control_git_push, &
              version_control_git_incoming_revision, version_control_git_mergebase

    !> String or list of strings specifying switches for Git diff under VERSION_CONTROL.
    !> If VERSION_CONTROL_SWITCHES_UNSPECIFIED, use the value of `version_control_diff_switches'.  If VERSION_CONTROL_SWITCHES_NONE,
    !> use no switches.
    !> Port of vc-git.el vc-git-diff-switches.
    type(version_control_switches_type), target, public :: version_control_git_diff_switches = version_control_switches_type( &
                                                           kind=VERSION_CONTROL_SWITCHES_NONE)

    !> Values of `version_control_git_resolve_conflicts' besides .true. and .false.:
    !> resolve and maybe unstage all files, use `version_control_resolve_conflicts'.
    integer, parameter, public :: VERSION_CONTROL_GIT_RESOLVE_CONFLICTS_UNSTAGE_MAYBE = 2, &
                                  VERSION_CONTROL_GIT_RESOLVE_CONFLICTS_DEFAULT = 3

    !> Whether to mark conflicted file as resolved upon saving.
    !> That is performed after all conflict markers in it have been removed.
    !>
    !> If this is .true. and there are no more conflict markers in the file,
    !> VERSION_CONTROL will mark the conflicts in the saved file as resolved.
    !>
    !> If the value is VERSION_CONTROL_GIT_RESOLVE_CONFLICTS_UNSTAGE_MAYBE, and no merge,
    !> rebase or similar operation is in progress, then after the last
    !> conflict is resolved, also clear the staging area.
    !>
    !> A value of VERSION_CONTROL_GIT_RESOLVE_CONFLICTS_DEFAULT means to use the value of
    !> `version_control_resolve_conflicts'.  Port of vc-git.el vc-git-resolve-conflicts.
    type(choice_setting_type), public :: version_control_git_resolve_conflicts = &
                                         choice_setting_type(name="version_control_git_resolve_conflicts", &
                                                             choice=VERSION_CONTROL_GIT_RESOLVE_CONFLICTS_DEFAULT)

    !> The Git commands in progress in a worktree (what
    !> `get_version_control_git_commands_in_progress' finds).
    type, public :: version_control_git_commands_in_progress_type
        logical :: revert = .false., cherry_pick = .false., rebase = .false., am = .false., merge = .false., &
                   bisect = .false.
    end type version_control_git_commands_in_progress_type

    ! The backend's index in `version_control_backends'.
    integer :: git_backend = 0
    !> String or list of strings specifying switches for Git blame under VERSION_CONTROL.
    !> If VERSION_CONTROL_SWITCHES_UNSPECIFIED, use the value of `version_control_annotate_switches'.  If
    !> VERSION_CONTROL_SWITCHES_NONE, use no switches.
    !>
    !> Tip: Set this to "-w" to make Git blame ignore whitespace when
    !> comparing changes.  See Man page `git-blame' for more.  Port of
    !> vc-git.el vc-git-annotate-switches.
    type(version_control_switches_type), target, public :: version_control_git_annotate_switches

    !> Name of the Git executable (excluding any arguments).  Port of
    !> vc-git.el vc-git-program.
    character(len=:), allocatable, public :: version_control_git_program

    !> Control whether tags are returned by revision completion for Git.
    !>
    !> When true, only branches and remotes will be returned by
    !> `version_control_git_revision_completion_table'.  This is used by various VERSION_CONTROL
    !> commands when completing branch names.  When false, tags are also
    !> included in the completions.  Port of vc-git.el
    !> vc-git-revision-complete-only-branches.
    logical, public :: version_control_git_revision_complete_only_branches = .false.

    !> If true, use the flag `--follow' when producing single file logs.
    !>
    !> A true value will make the printed log automatically follow
    !> the file renames.  The downsides is that the log produced this
    !> way may omit certain (merge) commits, and that `log_view_diff'
    !> fails on commits that used the previous name, in that log buffer.
    !>
    !> When this variable is false, and the log ends with a rename, there is a
    !> button which you can press to show the log for the file name before the
    !> rename.  Port of vc-git.el vc-git-print-log-follow.
    logical, public :: version_control_git_print_log_follow = .false.

    !> String or list of strings giving Git log switches for non-shortlogs.
    !> Port of vc-git.el vc-git-log-switches.
    type(version_control_switches_type), public :: version_control_git_log_switches

    !> String or list of strings giving Git log switches for shortlogs.
    !> Port of vc-git.el vc-git-shortlog-switches.
    type(version_control_switches_type), public :: version_control_git_shortlog_switches

    !> The value of `version_control_git_root_log_format': a format string (which is
    !> passed to "git log" via the argument "--pretty=tformat:FORMAT"), a
    !> regular expression matching the resulting Git log output, and the
    !> highlights of its match for the Log View buffer.
    type, public :: version_control_git_root_log_format_type
        character(len=:), allocatable :: format, regular_expression
        type(font_lock_highlight_type), allocatable :: highlights(:)
    end type version_control_git_root_log_format_type

    !> Git log format for `version_control_print_root_log'.
    !> This should be a FORMAT, a REGULAR_EXPRESSION and KEYWORDS, where FORMAT is a
    !> format string (which is passed to "git log" via the argument
    !> "--pretty=tformat:FORMAT"), REGULAR_EXPRESSION is a regular expression
    !> matching the resulting Git log output, and KEYWORDS is a list of
    !> `font_lock_keywords' for highlighting the Log View buffer.  Port of
    !> vc-git.el vc-git-root-log-format.
    type(version_control_git_root_log_format_type), public :: version_control_git_root_log_format

    !> Hook run after entering Git-Log-View mode.
    type(hook_type), target, public :: version_control_git_log_view_mode_hook = hook_type( &
                                       name="version_control_git_log_view_mode_hook")

    !> The keymap of `version_control_git_log_view_mode'.
    type(handle_type), public :: version_control_git_log_view_mode_map

    ! The stages of `version_control_git_directory_status_files'.
    integer, parameter :: STAGE_DONE = 0, STAGE_UPDATE_INDEX = 1, STAGE_LS_FILES_ADDED = 2, &
                          STAGE_LS_FILES_UP_TO_DATE = 3, STAGE_LS_FILES_CONFLICT = 4, STAGE_LS_FILES_MISSING = 5, &
                          STAGE_LS_FILES_UNKNOWN = 6, STAGE_LS_FILES_IGNORED = 7, STAGE_DIFF_INDEX = 8

    ! The state of a `directory_status_files' computation: its stage, the files
    ! still to be processed, the update function and its VC-Dir buffer,
    ! and the entries computed so far.  Port of vc-git.el
    ! vc-git-dir-status-state.
    type :: directory_status_state_type
        logical :: live = .false.
        integer :: stage = 0
        type(string_type), allocatable :: files(:)
        procedure(version_control_directory_update_procedure), pointer, nopass :: update_function => null()
        type(handle_type) :: buffer
        type(version_control_directory_entry_type), allocatable :: hash(:)
        integer :: n = 0
    end type directory_status_state_type

    ! The computations of `version_control_git_directory_status_files' in progress.
    type(directory_status_state_type), allocatable :: directory_states(:)

    !> True to treat pathspecs in commands literally.
    !> Good example of file name that needs this: "test[56].xx".  Port of
    !> vc-git.el vc-git-use-literal-pathspecs.
    logical, public :: version_control_git_use_literal_pathspecs = .true.

    !> Target length for Git commit summary lines.
    !> If a number, characters in Summary: lines beyond this length are
    !> displayed in the `version_control_git_log_edit_summary_target_warning' face.
    !> A negative value means no highlighting.
    !>
    !> By setting this to an integer around 50, you can improve the
    !> compatibility of your commit messages with Git commands that
    !> print the summary line in width-constrained contexts.  However,
    !> many commit summaries will need to exceed this length.
    !>
    !> See also `version_control_git_log_edit_summary_maximum_length'.  Port of vc-git.el
    !> vc-git-log-edit-summary-target-len (negative for none).
    integer, public :: version_control_git_log_edit_summary_target_length = -1

    !> Maximum length for Git commit summary lines.
    !> If a number, characters in summary lines beyond this length are
    !> displayed in the `version_control_git_log_edit_summary_maximum_warning' face.
    !> A negative value means no highlighting.
    !>
    !> It is good practice to avoid writing summary lines longer than
    !> this because otherwise the summary line will be truncated in many
    !> contexts in which Git commands display summary lines.
    !>
    !> See also `version_control_git_log_edit_summary_target_length'.  Port of vc-git.el
    !> vc-git-log-edit-summary-max-len.
    integer, public :: version_control_git_log_edit_summary_maximum_length = 68

    !> Face for Git commit summary lines beyond the target length.
    !> See `version_control_git_log_edit_summary_target_length'.  Port of vc-git.el
    !> vc-git-log-edit-summary-target-warning.
    type(named_face_type), target, public :: version_control_git_log_edit_summary_target_warning
    !> Face for Git commit summary lines beyond the maximum length.
    !> See `version_control_git_log_edit_summary_maximum_length'.  Port of vc-git.el
    !> vc-git-log-edit-summary-max-warning.
    type(named_face_type), target, public :: version_control_git_log_edit_summary_maximum_warning

    !> Hook run by `version_control_git_log_edit_mode'.
    type(hook_type), target, public :: version_control_git_log_edit_mode_hook = hook_type( &
                                       name="version_control_git_log_edit_mode_hook")

    !> Keymap of `version_control_git_log_edit_mode'.  Port of vc-git.el
    !> vc-git-log-edit-mode-map.
    type(handle_type), public :: version_control_git_log_edit_mode_map

    !> Git object ID of the empty tree object.  Port of vc-git.el
    !> vc-git--empty-tree.
    character(len=*), parameter :: version_control_git_empty_tree = "4b825dc642cb6eb9a060e54bf8d69288fbee4904"

    !> The version of Git (vc-git--program-version), "" until known.
    character(len=:), allocatable :: program_version

    character, parameter :: NUL = achar(0), LF = achar(10), TAB = achar(9)

    !> What `version_control_git_branch_remotes' gives: the remote-tracking branches
    !> UPSTREAM (the configured upstream) and PUSH (of a distinct push
    !> remote), unallocated when there is none.
    type, public :: version_control_git_branch_remotes_type
        character(len=:), allocatable :: upstream, push
    end type version_control_git_branch_remotes_type

    !> Value of `compilation_error_regular_expressions' in *vc-git* buffers.  Port of
    !> vc-git.el vc-git-error-regexp-alist.
    type(compilation_rule_type), allocatable, public :: version_control_git_error_regular_expressions(:)

contains

    ! ------------------------------------------------------------------
    ! Internal commands
    ! ------------------------------------------------------------------

    !> Append an unallocated entry to LIST (a flag passing nothing).  Not a port.
    subroutine append_empty(list)
        type(string_type), allocatable, intent(inout) :: list(:)
        type(string_type) :: empty

        if (.not. allocated(list)) allocate (list(0))
        list = [list, empty]
    end subroutine append_empty

    !> The strings of A then B.  Not a port.
    subroutine append_all(list, more)
        type(string_type), allocatable, intent(inout) :: list(:)
        type(string_type), intent(in) :: more(:)
        type(string_type), allocatable :: grown(:)
        integer :: n

        if (.not. allocated(list)) allocate (list(0))
        n = size(list)
        allocate (grown(n + size(more)))
        grown(1:n) = list
        grown(n + 1:) = more
        call move_alloc(grown, list)
    end subroutine append_all

    !> Return env vars for the `process_environment' of Git processes.
    !> Port of vc-git.el:vc-git--env-vars.
    subroutine git_environment_variables(subcommand, environment)
        character(len=*), intent(in) :: subcommand
        type(string_type), allocatable, intent(out) :: environment(:)

        allocate (environment(0))
        call append_string(environment, "GIT_DIR")
        if (version_control_git_use_literal_pathspecs) call append_string(environment, "GIT_LITERAL_PATHSPECS=1")
        ! Avoid optional repository locking during background operations
        ! (bug#21559, bug#80903).  Skipping these locks is always safe and
        ! can only lead to subsequent commands running more slowly.
        if (revert_buffer_in_progress .or. subcommand == "status") call append_string(environment, "GIT_OPTIONAL_LOCKS=0")
    end subroutine git_environment_variables

    !> The flags of `git' before SUBCOMMAND: --no-pager, and for older Git
    !> --no-optional-locks as `git_environment_variables' sets GIT_OPTIONAL_LOCKS.  Port of
    !> vc-git.el:vc-git--no-optional-locks.
    subroutine leading_flags(subcommand, flags)
        character(len=*), intent(in) :: subcommand
        type(string_type), allocatable, intent(out) :: flags(:)

        allocate (flags(0))
        call append_string(flags, "--no-pager")
        if (revert_buffer_in_progress .or. subcommand == "status") call append_string(flags, "--no-optional-locks")
    end subroutine leading_flags

    !> A wrapper around `version_control_do_command' for use in version_control_git_mod.
    !> The difference to `version_control_do_command' is that this function always invokes
    !> `version_control_git_program'.  BUFFER is the output buffer (*vc* if absent and
    !> CURRENT false; the current buffer with CURRENT).  Port of
    !> vc-git.el:vc-git-command.
    recursive subroutine version_control_git_command(okstatus, files, flags, status, buffer, current, exit_status, process)
        integer, intent(in) :: okstatus
        type(string_type), intent(in) :: files(:), flags(:)
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: buffer
        logical, intent(in), optional :: current
        integer, intent(out), optional :: exit_status
        type(handle_type), intent(out), optional :: process
        type(string_type), allocatable :: environment_variables(:), all(:), fl(:)
        type(handle_type) :: buffer_handle
        character(len=:), allocatable :: saved, root, subcommand
        logical :: rootdir, found, current_buffer

        status = status_type()
        subcommand = ""

        if (size(flags) > 0) then
            if (allocated(flags(1)%text)) subcommand = flags(1)%text
        end if

        call git_environment_variables(subcommand, environment_variables)
        call leading_flags(subcommand, all)
        call append_all(all, flags)
        rootdir = .false.

        if (size(files) == 1) then
            if (is_directory_name(files(1)%text)) then
                call version_control_git_root(files(1)%text, root, found)
                rootdir = found .and. root == files(1)%text
            end if
        end if

        saved = buffer_directory()
        if (rootdir) call set_buffer_directory(files(1)%text)

        ! Three cases:
        ! - operating on root directory and command is one where doing
        !   so requires passing "." to have the usual effect
        !   (e.g. 'git checkout --'   will do nothing;
        !         'git checkout -- .' will revert all files as desired)
        ! - operating on root directory and command is one where we
        !   must pass no list of files to have the usual effect
        !   (e.g. 'git log' for root logs as discussed in bug#16897)
        ! - not operating on root directory,
        !   pass FILE-OR-LIST along as normal.
        if (rootdir .and. subcommand == "checkout") then
            call append_string(fl, ".")
        else if (rootdir) then
            allocate (fl(0))
        else
            fl = files
        end if

        current_buffer = .false.
        if (present(current)) current_buffer = current

        if (current_buffer) then
            call version_control_do_command(VERSION_CONTROL_DESTINATION_CURRENT, okstatus, version_control_git_program, fl, all, &
                                            status, exit_status=exit_status, &
                                            process=process, environment=environment_variables)
        else
            if (present(buffer)) then
                buffer_handle = buffer
            else
                call get_or_make_buffer("*vc*", buffer_handle, status)
                if (.not. status%ok()) return
            end if

            call version_control_do_command(VERSION_CONTROL_DESTINATION_CURRENT, okstatus, version_control_git_program, fl, all, &
                                            status, buffer=buffer_handle, &
                                            exit_status=exit_status, process=process, environment=environment_variables)
        end if

        call set_buffer_directory(saved)
    end subroutine version_control_git_command

    !> The current buffer's `default_directory'.  Not a port.
    function buffer_directory() result(directory)
        character(len=:), allocatable :: directory

        directory = "./"
        if (current == 0) return
        if (allocated(buffers(current)%directory)) directory = buffers(current)%directory
    end function buffer_directory

    ! Not a port.
    subroutine set_buffer_directory(directory)
        character(len=*), intent(in) :: directory

        if (current == 0) return
        buffers(current)%directory = directory
    end subroutine set_buffer_directory

    !> Execute `git COMMAND ARGUMENTS...' with the output (without standard error)
    !> in the current buffer when TO_BUFFER, else discarded; CODE is the
    !> exit status (-1 if cut by a signal).  Port of
    !> vc-git.el:vc-git--call.
    subroutine version_control_git_execute(to_buffer, command, arguments, code, status)
        logical, intent(in) :: to_buffer
        character(len=*), intent(in) :: command
        type(string_type), intent(in) :: arguments(:)
        integer, intent(out) :: code
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: environment_variables(:), all(:)
        type(value_type), allocatable :: argv(:), envv(:)
        type(value_type) :: result
        integer :: k

        code = -1
        call git_environment_variables(command, environment_variables)
        call leading_flags(command, all)
        call append_string(all, command)
        call append_all(all, arguments)
        allocate (argv(size(all)), envv(size(environment_variables)))

        do k = 1, size(all)
            argv(k) = value_type(text=all(k)%text)
        end do

        do k = 1, size(environment_variables)
            envv(k) = value_type(text=environment_variables(k)%text)
        end do

        if (to_buffer) then
            call execute_process(version_control_git_program, result, status, destination=value_type(.true.), &
                                 arguments=make_list(argv), &
                                 error_file="/dev/null", environment=make_list(envv))
        else
            call execute_process(version_control_git_program, result, status, arguments=make_list(argv), &
                                 environment=make_list(envv))
        end if

        if (.not. status%ok()) return
        if (vtag(result) == V_INTEGER) code = int(xfixnum(result))
    end subroutine version_control_git_execute

    !> Run `git COMMAND ARGUMENTS...' and insert standard output in current buffer.
    !> Return whether the process exited with status zero.  Port of
    !> vc-git.el:vc-git--out-ok.
    logical function version_control_git_out_ok(command, arguments) result(ok)
        character(len=*), intent(in) :: command
        type(string_type), intent(in) :: arguments(:)
        type(status_type) :: st
        integer :: code

        call version_control_git_execute(.true., command, arguments, code, st)
        ok = st%ok() .and. code == 0
    end function version_control_git_out_ok

    !> Make a temporary buffer current (with-temp-buffer); OLD is the
    !> buffer to go back to.  Not a port.
    subroutine temporary_begin(old, temporary)
        type(handle_type), intent(out) :: old, temporary
        type(status_type) :: st
        character(len=:), allocatable :: directory

        old = current_buffer()
        directory = buffer_directory()
        call get_or_make_buffer(generate_new_buffer_name(" *temp*"), temporary, st)
        call set_buffer(temporary, st)
        call set_buffer_directory(directory)
    end subroutine temporary_begin

    !> Kill the temporary buffer TEMPORARY and make OLD current again.  Not a port.
    subroutine temporary_end(old, temporary)
        type(handle_type), intent(in) :: old, temporary
        type(status_type) :: st

        call close_buffer(temporary, st)
        call set_buffer(old, st)
    end subroutine temporary_end

    !> Execute a git command on FILE and return its output as string (OK false
    !> on failure).  FILE can be absent.  Port of
    !> vc-git.el:vc-git--run-command-string.
    subroutine version_control_git_execute_command_string(arguments, string, ok, file)
        type(string_type), intent(in) :: arguments(:)
        character(len=:), allocatable, intent(out) :: string
        logical, intent(out) :: ok
        character(len=*), intent(in), optional :: file
        type(string_type), allocatable :: rest(:)
        type(handle_type) :: old, temporary
        integer :: k

        allocate (rest(0))

        do k = 2, size(arguments)
            call append_string(rest, arguments(k)%text)
        end do

        if (present(file)) call append_string(rest, file_relative_name(file))
        call temporary_begin(old, temporary)
        ok = version_control_git_out_ok(arguments(1)%text, rest)
        string = buffer_string()
        call temporary_end(old, temporary)
        if (.not. ok) string = ""
    end subroutine version_control_git_execute_command_string

    !> The strings ARGUMENTS as a list (a helper for the short lists of git
    !> arguments).  Not a port.
    function strings(a1, a2, a3, a4, a5, a6, a7) result(list)
        character(len=*), intent(in) :: a1
        character(len=*), intent(in), optional :: a2, a3, a4, a5, a6, a7
        type(string_type), allocatable :: list(:)

        allocate (list(0))
        call append_string(list, a1)
        if (present(a2)) call append_string(list, a2)
        if (present(a3)) call append_string(list, a3)
        if (present(a4)) call append_string(list, a4)
        if (present(a5)) call append_string(list, a5)
        if (present(a6)) call append_string(list, a6)
        if (present(a7)) call append_string(list, a7)
    end function strings

    !> Check if the git db is empty (no commit done yet).  Port of
    !> vc-git.el:vc-git--empty-db-p.
    logical function is_version_control_git_empty_db() result(empty)
        type(status_type) :: st
        integer :: code

        call version_control_git_execute(.false., "rev-parse", strings("--verify", "HEAD"), code, st)
        empty = code /= 0
    end function is_version_control_git_empty_db

    !> The version of Git, "0" if unknown.  Port of
    !> vc-git.el:vc-git--program-version (the parsing is
    !> `version_control_git_parse_program_version').
    function version_control_git_program_version() result(version)
        character(len=:), allocatable :: version
        character(len=:), allocatable :: s
        logical :: ok

        if (allocated(program_version)) then
            version = program_version
            return
        end if

        call version_control_git_execute_command_string(strings("version"), s, ok)
        if (.not. ok) s = ""
        version = version_control_git_parse_program_version(s)
        program_version = version
    end function version_control_git_program_version

    !> The version in VERSION_STRING, the output of `git version' ("0" if
    !> none).  Some Git versions append additional strings to the
    !> numerical version string. E.g., Git for Windows appends
    !> ".windows.N", while Git for Mac appends " (Apple Git-N)". Capture
    !> numerical version and ignore the rest.  Port of
    !> vc-git.el:vc-git--program-version, in part.
    function version_control_git_parse_program_version(version_string) result(version)
        character(len=*), intent(in) :: version_string
        character(len=:), allocatable :: version
        type(status_type) :: st
        integer :: k

        version = "0"

        if (string_match("git version \([0-9][0-9.]+\)", version_string, st) > 0) then
            version = match_string(1, version_string)
            ! (string-trim-right ... "\\."): one trailing dot.
            k = len(version)
            if (version(k:k) == ".") version = version(1:k - 1)
        end if
    end function version_control_git_parse_program_version

    ! ------------------------------------------------------------------
    ! State-querying functions
    ! ------------------------------------------------------------------

    !> Check whether FILE is registered with git.  Port of
    !> vc-git.el:vc-git-registered.
    logical function version_control_git_registered(file) result(registered)
        character(len=:), allocatable :: directory, path, name, string
        type(handle_type) :: old, temporary
        logical :: found, ok
        character(len=*), intent(in) :: file

        registered = .false.
        call version_control_git_root(file, directory, found)
        if (.not. found) return
        ! If git(1) isn't installed then the `with-demoted-errors'
        ! below will mean we get an error message echoed about that
        ! fact with every `find-file'.  That's noisy, and inconsistent
        ! with other backend's `vc-*-registered' functions which are
        ! quieter in the case that the VERSION_CONTROLS isn't installed.  So check
        ! up here that git(1) is available.  See also bug#18481.
        call executable_find(version_control_git_program, path)
        if (len(path) == 0) return
        ! Do not use the `file-name-directory' here: git-ls-files
        ! sometimes fails to return the correct status for relative
        ! path specs.
        name = file_relative_name(file, directory)
        call temporary_begin(old, temporary)
        call set_buffer_directory(directory)
        ok = version_control_git_out_ok("ls-files", strings("-c", "-z", "--", name))
        ! If result is empty, use ls-tree to check for deleted file.
        if (point_minimum() == point_maximum()) ok = version_control_git_out_ok("ls-tree", strings("--name-only", "-z", "HEAD", &
                                                                                                   "--", name))
        string = buffer_string()
        call temporary_end(old, temporary)
        if (len(string) > len(name)) registered = string(1:len(name) + 1) == name//NUL
    end function version_control_git_registered

    !> Convert CODE_LIST to a VERSION_CONTROL status.
    !>
    !> Each element of CODE_LIST comes from the first two characters of
    !> a line returned by `git status --porcelain' and should be passed
    !> in the order given by `git status'.  Port of
    !> vc-git.el:vc-git--git-status-to-vc-state.
    integer function version_control_git_status_to_version_control_state(codes) result(state)
        type(string_type), intent(in) :: codes(:)
        type(status_type) :: st
        character(len=:), allocatable :: code

        ! It is necessary to allow CODE-LIST to be a list because sometimes git
        ! status returns multiple lines, e.g. for a file that is removed from
        ! the index but is present in the HEAD and working tree.
        if (size(codes) == 0) then
            state = VERSION_CONTROL_UP_TO_DATE
        else if (size(codes) == 1) then
            code = codes(1)%text

            if (code == "!!") then
                state = VERSION_CONTROL_IGNORED
            else if (code == "??") then
                state = VERSION_CONTROL_UNREGISTERED
            else if (code == "D ") then
                state = VERSION_CONTROL_REMOVED
            else if (string_match("^.D$", code, st) > 0) then
                state = VERSION_CONTROL_MISSING
            else if (string_match("^[ M]+$", code, st) > 0) then
                state = VERSION_CONTROL_EDITED
            else if (string_match("^[ A]+$", code, st) > 0) then
                state = VERSION_CONTROL_ADDED
            else if (string_match("^[ U]+$", code, st) > 0) then
                state = VERSION_CONTROL_CONFLICT
            else
                state = VERSION_CONTROL_EDITED
            end if
            ! hazard-ok(short-circuit): size(codes) >= 2 here, so codes(2) is valid.
        else if (size(codes) == 2 .and. codes(1)%text == "D " .and. codes(2)%text == "??") then
            ! I know of two cases when git state returns more than one element,
            ! in both cases returning '("D " "??")':
            ! 1. When a file is removed from the index but present in the
            !    HEAD and working tree.
            ! 2. When a file A is renamed to B in the index and then back to A
            !    in the working tree.
            ! In both of these instances, `unregistered' is a reasonable response.
            state = VERSION_CONTROL_UNREGISTERED
        else
            ! In other cases, let us return `edited'.
            state = VERSION_CONTROL_EDITED
        end if
    end function version_control_git_status_to_version_control_state

    !> Git-specific version of `version_control_state'.  Port of vc-git.el:vc-git-state.
    integer function version_control_git_state(file) result(state)
        character(len=*), intent(in) :: file
        type(string_type), allocatable :: arguments(:), codes(:)
        character(len=:), allocatable :: status
        logical :: ok
        integer :: b, e

        ! It can't set `needs-update' or `needs-merge'. The rough
        ! equivalent would be that upstream branch for current branch is in
        ! fast-forward state i.e. current branch is direct ancestor of
        ! corresponding upstream branch, and the file was modified
        ! upstream.  We'd need to check against the upstream tracking
        ! branch for that (an extra process call or two).
        arguments = strings("status", "--porcelain", "-z", "--untracked-files")
        ! Just to be explicit, it's the default anyway.
        ! Invalid version syntax, an error condition upstream, counts as
        ! a lower version.
        if (version("1.7.6.3") <= version(version_control_git_program_version())) call append_string(arguments, "--ignored")
        call append_string(arguments, "--")
        call version_control_git_execute_command_string(arguments, status, ok, file)

        if (.not. ok) then
            ! If status is nil, there was an error calling git, likely because
            ! the file is not in a git repo.
            state = VERSION_CONTROL_UNREGISTERED
            return
        end if

        ! If this code is adapted to parse 'git status' for a directory,
        ! note that a renamed file takes up two null values and needs to be
        ! treated slightly more carefully.
        allocate (codes(0))
        b = 1

        do while (b <= len(status))
            e = index(status(b:), NUL)

            if (e == 0) then
                e = len(status) + 1
            else
                e = b + e - 1
            end if

            if (e > b) call append_string(codes, status(b:min(b + 1, e - 1)))
            b = e + 1
        end do

        state = version_control_git_status_to_version_control_state(codes)
    end function version_control_git_state

    !> Git-specific version of `version_control_working_revision'.  Port of
    !> vc-git.el:vc-git-working-revision.
    subroutine version_control_git_working_revision(file, revision, found)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: revision
        logical, intent(out) :: found

        if (len(file) < 0) return
        call version_control_git_revision_parse("HEAD", revision, found)
    end subroutine version_control_git_working_revision

    !> The working revision's symbolic name: HEAD.  Port of vc-git.el
    !> vc-git-working-revision-symbol.
    subroutine version_control_git_working_revision_symbol(s)
        character(len=:), allocatable, intent(out) :: s

        s = "HEAD"
    end subroutine version_control_git_working_revision_symbol

    !> The branch HEAD refers to (FOUND false when detached), cached in
    !> FILE's `vc-git-symbolic-ref'.  Port of
    !> vc-git.el:vc-git--symbolic-ref.
    subroutine version_control_git_symbolic_reference(file, reference, found)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: reference
        logical, intent(out) :: found
        character(len=:), allocatable :: string
        type(status_type) :: st

        call version_control_file_get_git_symbolic_reference(file, reference, found)
        if (found) return
        call version_control_git_execute_command_string(strings("symbolic-ref", "HEAD"), string, found)

        if (found) then
            if (string_match("^\(refs/heads/\)?\(.+\)$", string, st) > 0) then
                reference = match_string(2, string)
            else
                reference = string
            end if

            call version_control_file_set_git_symbolic_reference(file, reference)
        else
            call version_control_file_set_git_symbolic_reference(file)
        end if
    end subroutine version_control_git_symbolic_reference

    !> Return a string for `version_control_mode_line' to put in the mode line for FILE.
    !> Port of vc-git.el:vc-git-mode-line-string.
    subroutine version_control_git_mode_line_string(file, s, found)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: s
        logical, intent(out) :: found
        character(len=:), allocatable :: echo, face, indicator, revision, disp_revision, locker
        logical :: has_revision, has_reference
        integer :: state

        state = version_control_state(file, locker=locker)
        call version_control_mode_line_state(state, echo, face, indicator, locker)
        call version_control_working_revision(file, revision, has_revision, git_backend_index())
        call version_control_git_symbolic_reference(file, disp_revision, has_reference)

        if (.not. has_reference) then
            disp_revision = ""
            if (has_revision) disp_revision = revision(1:min(7, len(revision)))
        end if

        s = ""
        if (version_control_display_status%choice /= VERSION_CONTROL_DISPLAY_STATUS_NO_BACKEND) s = "Git"
        s = s//indicator//disp_revision
        found = .true.
    end subroutine version_control_git_mode_line_string

    !> The index of the Git backend.  Not a port.
    integer function git_backend_index() result(b)
        use version_control_hooks_mod, only: version_control_backend_named

        b = version_control_backend_named("Git")
    end function git_backend_index

    !> The root of FILE's working tree (the directory with .git).  Port of
    !> vc-git.el:vc-git-root.
    subroutine version_control_git_root(file, root, found)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: root
        logical, intent(out) :: found

        call version_control_find_root(file, ".git", root, found)
    end subroutine version_control_git_root

    !> Git's checkout model: `implicit'.  Port of
    !> vc-git.el:vc-git-checkout-model.
    integer function version_control_git_checkout_model(files) result(model)
        type(string_type), intent(in) :: files(:)

        model = VERSION_CONTROL_MODEL_IMPLICIT
        if (size(files) < 0) model = 0
    end function version_control_git_checkout_model

    ! ------------------------------------------------------------------
    ! State-changing functions
    ! ------------------------------------------------------------------

    !> Create a new Git repository.  Port of vc-git.el:vc-git-create-repo.
    recursive subroutine version_control_git_create_repository(status)
        type(status_type), intent(out) :: status
        type(string_type) :: none(0)

        call version_control_git_command(0, none, strings("init"), status)
    end subroutine version_control_git_create_repository

    !> Register FILES into the git version-control system.  Port of
    !> vc-git.el:vc-git-register.
    recursive subroutine version_control_git_register(files, status, comment)
        type(string_type), intent(in) :: files(:)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: comment
        type(string_type), allocatable :: flist(:), dlist(:)
        integer :: k

        status = status_type()
        if (present(comment)) continue
        allocate (flist(0), dlist(0))

        do k = size(files), 1, -1
            if (is_file_directory(files(k)%text)) then
                call append_string(dlist, files(k)%text)
            else
                call append_string(flist, files(k)%text)
            end if
        end do

        if (size(flist) > 0) then
            call version_control_git_command(0, flist, strings("update-index", "--add", "--"), status)
            if (.not. status%ok()) return
        end if

        if (size(dlist) > 0) call version_control_git_command(0, dlist, strings("add"), status)
    end subroutine version_control_git_register

    !> Remove FILE from the index.  Port of vc-git.el:vc-git-unregister.
    recursive subroutine version_control_git_unregister(file, status)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status

        call version_control_git_command(0, strings(file), strings("rm", "-f", "--cached", "--"), status)
    end subroutine version_control_git_unregister

    !> Get revision REVISION of FILE into BUFFER.  Port of
    !> vc-git.el:vc-git-find-revision.
    recursive subroutine version_control_git_find_revision(file, revision, buffer, status)
        character(len=*), intent(in) :: file, revision
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: fn, fullname, root, r
        type(string_type) :: none(0)
        logical :: ok, found

        call version_control_git_execute_command_string(strings("ls-files", "-z", "--full-name", "--"), fn, ok, file)

        ! ls-files does not return anything when looking for a revision of
        ! a file that has been renamed or removed.
        if (len(fn) == 0) then
            call version_control_git_root(buffer_directory(), root, found)
            fullname = file_relative_name(file, root)
        else
            fullname = fn(1:len(fn) - 1)
        end if

        r = revision
        if (len(r) == 0) r = "HEAD"
        call version_control_git_command(0, none, strings("cat-file", "--filters", r//":"//fullname), status, buffer=buffer)
    end subroutine version_control_git_find_revision

    !> Return the git ignore file that controls FILE.  Port of
    !> vc-git.el:vc-git-find-ignore-file.
    subroutine version_control_git_find_ignore_file(file, ignore, found)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: ignore
        logical, intent(out) :: found
        character(len=:), allocatable :: root
        type(status_type) :: st

        call version_control_git_root(file, root, found)
        call expand_file_name(".gitignore", ignore, st, root)
        found = .true.
    end subroutine version_control_git_find_ignore_file

    !> Check out REVISION (HEAD if absent) of FILE.  Port of
    !> vc-git.el:vc-git-checkout.
    recursive subroutine version_control_git_checkout(file, status, revision)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: revision

        if (present(revision)) then
            call version_control_git_command(0, strings(file), strings("checkout", revision), status)
        else
            call version_control_git_command(0, strings(file), strings("checkout", "HEAD"), status)
        end if
    end subroutine version_control_git_checkout

    !> Revert FILE to the version stored in the Git repository.  Port of
    !> vc-git.el:vc-git-revert.
    recursive subroutine version_control_git_revert(file, status, contents_done)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: contents_done

        if (present(contents_done)) then
            call version_control_git_command(0, strings(file), strings("update-index", "--"), status)
        else
            call version_control_git_command(0, strings(file), strings("reset", "-q", "--"), status)
            if (.not. status%ok()) return
            call version_control_git_command(VERSION_CONTROL_OKSTATUS_UNLESS_SIGNAL, strings(file), strings("checkout", "-q", &
                                                                                                            "--"), status)
        end if
    end subroutine version_control_git_revert

    !> Revert FILES to the versions stored in the Git repository.  Port of
    !> vc-git.el:vc-git-revert-files.
    recursive subroutine version_control_git_revert_files(files, status)
        type(string_type), intent(in) :: files(:)
        type(status_type), intent(out) :: status

        call version_control_git_command(0, files, strings("reset", "-q", "--"), status)
        if (.not. status%ok()) return
        call version_control_git_command(VERSION_CONTROL_OKSTATUS_UNLESS_SIGNAL, files, strings("checkout", "-q", "--"), status)
    end subroutine version_control_git_revert_files

    !> Delete FILE with `git rm'.  Port of vc-git.el:vc-git-delete-file.
    recursive subroutine version_control_git_delete_file(file, status)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status

        call version_control_git_command(0, strings(file), strings("rm", "-f", "--"), status)
    end subroutine version_control_git_delete_file

    !> Delete FILES with `git rm'.  Port of vc-git.el:vc-git-delete-files.
    recursive subroutine version_control_git_delete_files(files, status)
        type(string_type), intent(in) :: files(:)
        type(status_type), intent(out) :: status

        call version_control_git_command(0, files, strings("rm", "-rf", "--"), status)
    end subroutine version_control_git_delete_files

    !> Rename OLD to NEW with `git mv'.  Port of vc-git.el:vc-git-rename-file.
    recursive subroutine version_control_git_rename_file(old, new, status)
        character(len=*), intent(in) :: old, new
        type(status_type), intent(out) :: status

        call version_control_git_command(0, strings(old, new), strings("mv", "-f", "--"), status)
    end subroutine version_control_git_rename_file

    !> Mark the conflicts in FILES as resolved (`git add').  Port of
    !> vc-git.el:vc-git-mark-resolved.
    recursive subroutine version_control_git_mark_resolved(files, status)
        type(string_type), intent(in) :: files(:)
        type(status_type), intent(out) :: status

        call version_control_git_command(0, files, strings("add"), status)
    end subroutine version_control_git_mark_resolved

    ! ------------------------------------------------------------------
    ! Diff and revisions
    ! ------------------------------------------------------------------

    !> Get a difference report using Git between two revisions of FILES.
    !> Port of vc-git.el:vc-git-diff (`version_control_git_diff_switches' not VERSION_CONTROL_SWITCHES_UNSPECIFIED; the
    !> difftool path is deferred).
    recursive subroutine version_control_git_diff(files, buffer, asynchronous, status, rev1, rev2)
        type(string_type), intent(in) :: files(:)
        type(handle_type), intent(in) :: buffer
        logical, intent(in) :: asynchronous
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: rev1, rev2
        type(string_type), allocatable :: flags(:), sw(:)
        character(len=:), allocatable :: command, r1
        integer :: okstatus

        status = status_type()
        command = "diff-tree"

        if (present(rev2)) then
            r1 = version_control_git_empty_tree
            if (present(rev1)) r1 = rev1
        else
            command = "diff-index"

            if (present(rev1)) then
                r1 = rev1
            else if (is_version_control_git_empty_db()) then
                ! If there aren't any commits yet then there is no HEAD.
                ! So diff against the empty tree object.
                r1 = version_control_git_empty_tree
            else
                r1 = "HEAD"
            end if
        end if

        if (version_control_git_diff_switches%kind == VERSION_CONTROL_SWITCHES_UNSPECIFIED) then
            call set_error(status, GENERIC_ERROR_CODE, "version_control_git_diff with git difftool is not ported yet")
            return
        end if

        flags = strings(command, "--exit-code")
        call version_control_switches(git_backend_index(), "diff", sw)
        call append_all(flags, sw)
        call append_string(flags, "-p")
        call append_string(flags, r1)

        if (present(rev2)) then
            call append_string(flags, rev2)
        else
            call append_empty(flags)
        end if

        call append_string(flags, "--")
        okstatus = 1
        if (asynchronous) okstatus = VERSION_CONTROL_OKSTATUS_ASYNCHRONOUS
        call version_control_git_command(okstatus, files, flags, status, buffer=buffer)
    end subroutine version_control_git_diff

    !> The revisions Git can complete: HEAD and the branches, tags (unless
    !> `version_control_git_revision_complete_only_branches') and remotes.  Port of
    !> vc-git.el:vc-git-revision-table (as
    !> vc-git-revision-completion-table).
    subroutine version_control_git_revision_table(files, table)
        type(string_type), intent(in) :: files(:)
        type(string_type), allocatable, intent(out) :: table(:)
        type(string_type) :: none(0)
        type(handle_type) :: old, temporary
        type(status_type) :: st
        character(len=:), allocatable :: regular_expression
        integer(int64) :: found

        ! What about `files'?!?  --Stef
        if (size(files) < 0) return
        allocate (table(0))
        call append_string(table, "HEAD")
        call temporary_begin(old, temporary)
        call version_control_git_command(VERSION_CONTROL_OKSTATUS_UNLESS_SIGNAL, none, strings("for-each-ref", &
                                                                                         "--format=%(refname)"), st, current=.true.)
        call goto_character(point_minimum())

        if (version_control_git_revision_complete_only_branches) then
            regular_expression = "^refs/\(heads\|remotes\)/\(.*\)$"
        else
            regular_expression = "^refs/\(heads\|tags\|remotes\)/\(.*\)$"
        end if

        do
            call regular_expression_search_forward(regular_expression, found, st, noerror=SEARCH_NOERROR)
            if (found == 0) exit
            call append_string(table, match_in_buffer(2))
        end do

        call temporary_end(old, temporary)
    end subroutine version_control_git_revision_table

    !> The text of group N of the last buffer match.  Not a port.
    function match_in_buffer(n) result(s)
        integer, intent(in) :: n
        character(len=:), allocatable :: s

        s = match_string(n)
    end function match_in_buffer

    !> `--abbrev-commit' with `version_control_use_short_revision', else
    !> `--no-abbrev-commit'.  Port of vc-git.el:vc-git--maybe-abbrev.
    function maybe_abbreviation() result(s)
        character(len=:), allocatable :: s

        if (version_control_use_short_revision) then
            s = "--abbrev-commit"
        else
            s = "--no-abbrev-commit"
        end if
    end function maybe_abbreviation

    !> Git-specific version of `version_control_previous_revision'.  Port of
    !> vc-git.el:vc-git-previous-revision.
    subroutine version_control_git_previous_revision(file, revision, previous, found)
        character(len=*), intent(in) :: file, revision
        character(len=:), allocatable, intent(out) :: previous
        logical, intent(out) :: found
        character(len=:), allocatable :: file_name, symbol, carets
        type(handle_type) :: old, temporary
        type(status_type) :: st
        integer(int64) :: shortage, p
        logical :: ok
        integer :: n
        character(len=16) :: text

        previous = ""
        found = .false.

        if (string_match("\`HEAD\(\^*\)\'", revision, st) > 0) then
            carets = match_string(1, revision)
            write (text, '("HEAD~", i0)') 1 + len(carets)
            previous = trim(text)
            found = .true.
        else if (string_match("\`HEAD~\([0-9]+\)\'", revision, st) > 0) then
            carets = match_string(1, revision)
            read (carets, *) n
            write (text, '("HEAD~", i0)') 1 + n
            previous = trim(text)
            found = .true.
        else if (len(file) > 0) then
            file_name = file_relative_name(file)
            call temporary_begin(old, temporary)
            ok = version_control_git_out_ok("rev-list", strings(maybe_abbreviation(), "-2", revision, "--", file_name))

            if (ok) then
                call goto_character(point_maximum())
                ok = is_beginning_of_line()
            end if

            if (ok) then
                shortage = forward_line(-1_int64)
                ok = shortage == 0 .and. .not. is_beginning_of_buffer()
            end if

            if (ok) then
                p = point()
                call buffer_substring(p, point_maximum() - 1, previous, st)
                found = st%ok()
            end if

            call temporary_end(old, temporary)

            if (found) then
                call version_control_git_symbolic_commit(previous, symbol, ok)
                if (ok) previous = symbol
            end if
        else
            ! We used to use "^" here, but that fails on MS-Windows if git
            ! is invoked via a batch file, in which case cmd.exe strips
            ! the "^" because it is a special character for cmd which
            ! process-file does not (and cannot) quote.
            call version_control_git_revision_parse(revision//"~1", previous, found)
        end if
    end subroutine version_control_git_previous_revision

    !> The revision REVISION names (FOUND false if none).  Port of
    !> vc-git.el:vc-git--rev-parse.
    subroutine version_control_git_revision_parse(revision, out, found)
        character(len=*), intent(in) :: revision
        character(len=:), allocatable, intent(out) :: out
        logical, intent(out) :: found
        type(handle_type) :: old, temporary
        type(status_type) :: st
        type(string_type), allocatable :: arguments(:)

        out = ""
        allocate (arguments(0))
        if (version_control_use_short_revision) call append_string(arguments, "--short")
        call append_string(arguments, revision)
        call temporary_begin(old, temporary)
        found = version_control_git_out_ok("rev-parse", arguments)

        if (found) then
            call goto_character(point_minimum())
            call buffer_substring(point(), position_end_of_line(), out, st)
        end if

        call temporary_end(old, temporary)
    end subroutine version_control_git_revision_parse

    !> Translate revision string of COMMIT to a symbolic form.
    !> If FORCE is true, the result is allowed to include revision
    !> specifications like "master~8" (the 8th parent of the commit
    !> currently pointed to by the master branch), otherwise such revision
    !> specifications are rejected, and FOUND is false.  Port of
    !> vc-git.el:vc-git-symbolic-commit.
    subroutine version_control_git_symbolic_commit(commit, symbol, found, force)
        character(len=*), intent(in) :: commit
        character(len=:), allocatable, intent(out) :: symbol
        logical, intent(out) :: found
        logical, intent(in), optional :: force
        type(handle_type) :: old, temporary
        type(status_type) :: st
        integer(int64) :: shortage
        logical :: f

        symbol = ""
        f = .false.
        if (present(force)) f = force
        call temporary_begin(old, temporary)
        found = version_control_git_out_ok("name-rev", strings("--no-undefined", "--name-only", commit))

        if (found) then
            call goto_character(point_minimum())
            if (.not. f) found = .not. looking_at("^.*[~^].*$", st)
        end if

        if (found) then
            shortage = forward_line(2_int64)
            found = shortage == 1 .and. is_beginning_of_line()
        end if

        if (found) call buffer_substring(point_minimum(), point_maximum() - 1, symbol, st)
        call temporary_end(old, temporary)
    end subroutine version_control_git_symbolic_commit

    ! ------------------------------------------------------------------
    ! Checking in
    ! ------------------------------------------------------------------

    !> Toggle whether to add the "Signed-off-by" line at the end of commit message.
    !> Port of vc-git.el:vc-git-log-edit-toggle-signoff.
    recursive subroutine version_control_git_log_edit_toggle_signoff(status)
        type(status_type), intent(out) :: status
        logical :: on

        on = log_edit_toggle_header("Sign-Off", "yes", status)
    end subroutine version_control_git_log_edit_toggle_signoff

    !> Toggle whether to bypass the pre-commit and commit-msg hooks.  Port of
    !> vc-git.el:vc-git-log-edit-toggle-no-verify.
    recursive subroutine version_control_git_log_edit_toggle_no_verify(status)
        type(status_type), intent(out) :: status
        logical :: on

        on = log_edit_toggle_header("No-Verify", "yes", status)
    end subroutine version_control_git_log_edit_toggle_no_verify

    ! Port of vc-git.el:vc-git--log-edit-summary-check.
    subroutine log_edit_summary_check(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regular_expression
        character(len=64) :: text
        integer(int64) :: position

        found = .false.
        call regular_expression_search_forward("^Summary: ", position, status, bound=limit, noerror=SEARCH_NOERROR)
        if (position == 0 .or. .not. status%ok()) return

        if (version_control_git_log_edit_summary_maximum_length >= 0 &
            .and. version_control_git_log_edit_summary_target_length >= 0) then
            write (text, '(".\{,", i0, "\}\(.\{,", i0, "\}\)\(.*\)")') version_control_git_log_edit_summary_target_length, &
                version_control_git_log_edit_summary_maximum_length - version_control_git_log_edit_summary_target_length
            regular_expression = trim(text)
        else if (version_control_git_log_edit_summary_maximum_length >= 0) then
            write (text, '(".\{,", i0, "\}\(?2:.*\)")') version_control_git_log_edit_summary_maximum_length
            regular_expression = trim(text)
        else if (version_control_git_log_edit_summary_target_length >= 0) then
            write (text, '(".\{,", i0, "\}\(.*\)")') version_control_git_log_edit_summary_target_length
            regular_expression = trim(text)
        else
            return
        end if

        call regular_expression_search_forward(regular_expression, position, status, bound=limit, noerror=SEARCH_NOERROR)
        found = position /= 0
    end subroutine log_edit_summary_check

    !> Major mode for editing Git log messages.
    !> It is based on `log_edit_mode', and has Git-specific extensions.
    !> Port of vc-git.el:vc-git-log-edit-mode (\[version_control_git_log_edit_toggle_amend]
    !> is deferred).
    recursive subroutine version_control_git_log_edit_mode(status)
        type(status_type), intent(out) :: status
        type(font_lock_keyword_type), allocatable :: keywords(:), more(:)
        type(font_lock_highlight_type) :: h(2)
        logical :: saved

        if (.not. associated(major_modes%version_control_git_log_edit_mode%execute)) call define_log_edit_mode()
        call major_modes%version_control_git_log_edit_mode%begin(saved, status)
        if (.not. status%ok()) return
        call log_edit_font_lock_keywords(keywords)
        h(1) = font_lock_highlight(1, face="version_control_git_log_edit_summary_target_warning", &
                                   override=FONT_LOCK_OVERRIDE_PREPEND, &
                                   laxmatch=.true.)
        h(2) = font_lock_highlight(2, face="version_control_git_log_edit_summary_maximum_warning", &
                                   override=FONT_LOCK_OVERRIDE_PREPEND, &
                                   laxmatch=.true.)
        allocate (more(1))
        more(1) = font_lock_keyword(highlights=h, matcher=log_edit_summary_check)
        keywords = [keywords, more]
        call font_lock_defaults%set_local(keywords=keywords, keywords_only=.true.)
        call major_modes%version_control_git_log_edit_mode%end(saved, status)
    end subroutine version_control_git_log_edit_mode

    ! Define Git's Log Edit mode, its keymap and faces.  Not a port.
    subroutine define_log_edit_mode()
        type(named_face_type), pointer :: f
        type(status_type) :: st

        f => version_control_git_log_edit_summary_target_warning
        call f%define("version_control_git_log_edit_summary_target_warning", &
                      "Face for Git commit summary lines beyond the target length."//LF// &
                      "See `version_control_git_log_edit_summary_target_length'.")
        call f%specification_entry(display_when(), face_attributes(inherit="warning"))
        f => version_control_git_log_edit_summary_maximum_warning
        call f%define("version_control_git_log_edit_summary_maximum_warning", &
                      "Face for Git commit summary lines beyond the maximum length."//LF// &
                      "See `version_control_git_log_edit_summary_maximum_length'.")
        call f%specification_entry(display_when(), face_attributes(inherit="error"))
        if (.not. associated(major_modes%log_edit_mode%execute)) call initialize_log_edit()
        call major_modes%version_control_git_log_edit_mode%define("version_control_git_log_edit_mode", "Log-Edit/git", &
                                                                  version_control_git_log_edit_mode, &
                                                           version_control_git_log_edit_mode_hook, parent=major_modes%log_edit_mode)
        version_control_git_log_edit_mode_map = major_modes%version_control_git_log_edit_mode%map
        commands%version_control_git_log_edit_mode%execute => version_control_git_log_edit_mode
        commands%version_control_git_log_edit_toggle_signoff%execute => version_control_git_log_edit_toggle_signoff
        commands%version_control_git_log_edit_toggle_no_verify%execute => version_control_git_log_edit_toggle_no_verify
        call keymap_set(version_control_git_log_edit_mode_map, "C-c C-s", "version_control_git_log_edit_toggle_signoff", st)
        call keymap_set(version_control_git_log_edit_mode_map, "C-c C-n", "version_control_git_log_edit_toggle_no_verify", st)
    end subroutine define_log_edit_mode

    ! (boolean-arg-fn "--amend") and friends: the argument if the header's
    ! value is "yes".  Not a port.
    subroutine amend_argument(value, arguments)
        character(len=*), intent(in) :: value
        type(string_type), allocatable, intent(out) :: arguments(:)

        call boolean_argument(value, "--amend", arguments)
    end subroutine amend_argument

    ! Not a port.
    subroutine no_verify_argument(value, arguments)
        character(len=*), intent(in) :: value
        type(string_type), allocatable, intent(out) :: arguments(:)

        call boolean_argument(value, "--no-verify", arguments)
    end subroutine no_verify_argument

    ! Not a port.
    subroutine signoff_argument(value, arguments)
        character(len=*), intent(in) :: value
        type(string_type), allocatable, intent(out) :: arguments(:)

        call boolean_argument(value, "--signoff", arguments)
    end subroutine signoff_argument

    ! Not a port.
    subroutine boolean_argument(value, argument, arguments)
        character(len=*), intent(in) :: value, argument
        type(string_type), allocatable, intent(out) :: arguments(:)

        if (value == "yes") then
            call append_string(arguments, argument)
        else
            allocate (arguments(0))
        end if
    end subroutine boolean_argument

    ! Port of vc-git.el:vc-git--log-edit-extract-headers: MESSAGE and the
    ! command line arguments ARGUMENTS.
    recursive subroutine log_edit_extract_headers_git(comment, message, arguments, status)
        character(len=*), intent(in) :: comment
        character(len=:), allocatable, intent(out) :: message
        type(string_type), allocatable, intent(out) :: arguments(:)
        type(status_type), intent(out) :: status
        type(log_edit_header_type) :: headers(5)

        headers(1)%name = "Author"
        headers(1)%cmdarg = "--author"
        headers(2)%name = "Date"
        headers(2)%cmdarg = "--date"
        headers(3)%name = "Amend"
        headers(3)%kind = LOG_EDIT_HEADER_FN
        headers(3)%fn => amend_argument
        headers(4)%name = "No-Verify"
        headers(4)%kind = LOG_EDIT_HEADER_FN
        headers(4)%fn => no_verify_argument
        headers(5)%name = "Sign-Off"
        headers(5)%kind = LOG_EDIT_HEADER_FN
        headers(5)%fn => signoff_argument
        call log_edit_extract_headers(headers, comment, message, arguments, status)
    end subroutine log_edit_extract_headers_git

    !> Git-specific version of `version_control_backend_checkin'.
    !> REVISION is ignored.  Port of vc-git.el:vc-git-checkin.
    recursive subroutine version_control_git_checkin(files, comment, status, revision)
        type(string_type), intent(in) :: files(:)
        character(len=*), intent(in) :: comment
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: revision

        if (present(revision)) continue
        call version_control_git_checkin_1(comment, files, status)
    end subroutine version_control_git_checkin

    !> Workhorse routine for `version_control_git_checkin'.
    !> COMMENT is the commit message.  FILES is the list of files to check
    !> in.  Port of vc-git.el:vc-git--checkin (checking in a patch, and the
    !> asynchronous commit of `version_control_asynchronous_checkin', are deferred; the message
    !> goes on the command line, as on GNU/Linux).
    recursive subroutine version_control_git_checkin_1(comment, files, status)
        use files_mod, only: abbreviate_file_name
        character(len=*), intent(in) :: comment
        type(string_type), intent(in) :: files(:)
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: extracted(:)
        type(string_type), allocatable :: flags(:), fl(:)
        character(len=:), allocatable :: file1, root, saved, message
        logical :: found, only
        integer :: k

        status = status_type()

        if (size(files) > 0) then
            file1 = files(1)%text
        else
            file1 = buffer_directory()
        end if

        call version_control_git_root(file1, root, found)

        if (.not. found) then
            call set_error(status, GENERIC_ERROR_CODE, "Invalid call to `version_control_git_checkin'")
            return
        end if

        only = size(files) > 1
        if (.not. only) only = root /= abbreviate_file_name(file1)
        saved = buffer_directory()
        file1 = root
        call expand_file_name(file1, root, status)
        if (.not. status%ok()) return
        call set_buffer_directory(root)
        call log_edit_extract_headers_git(comment, message, extracted, status)

        if (status%ok()) then
            allocate (flags(0))
            call append_string(flags, "commit")
            call append_string(flags, "-m")
            call append_string(flags, message)

            do k = 1, size(extracted)
                call append_string(flags, extracted(k)%text)
            end do

            ! When operating on the whole tree, better pass
            ! "-a" than ".", since "."  fails when we're
            ! committing a merge.
            if (only) then
                call append_string(flags, "--only")
                call append_string(flags, "--")
                fl = files
            else
                call append_string(flags, "-a")
                allocate (fl(0))
            end if

            call version_control_git_command(0, fl, flags, status)
        end if

        call set_buffer_directory(saved)
    end subroutine version_control_git_checkin_1

    ! ------------------------------------------------------------------
    ! History functions
    ! ------------------------------------------------------------------

    ! (ensure-list SWITCHES) appended to FLAGS.  Not a port.
    subroutine append_switches(flags, switches)
        type(string_type), allocatable, intent(inout) :: flags(:)
        type(version_control_switches_type), intent(in) :: switches

        if (switches%kind == VERSION_CONTROL_SWITCHES_LIST) then
            if (allocated(switches%list)) call append_all(flags, switches%list)
        end if
    end subroutine append_switches

    !> Print commit log associated with FILES into specified BUFFER.
    !> If SHORTLOG is true, use a short format based on `version_control_git_root_log_format'.
    !> (This requires at least Git version 1.5.6, for the --graph option.)
    !> If START_REVISION is not "", it is the newest revision to show.
    !> If LIMIT is a number, show no more than this many entries.
    !> If LIMIT is a non-empty string, use it as a base revision.  Port of
    !> vc-git.el:vc-git-print-log.
    recursive subroutine version_control_git_print_log(files, buffer, shortlog, start_revision, limit, status, limit_unsupported)
        use version_control_dispatcher_mod, only: version_control_setup_buffer
        use version_control_mod, only: version_control_log_view_type, VERSION_CONTROL_LOG_VIEW_WITH_DIFF
        use version_control_hooks_mod, only: version_control_log_limit_type, VERSION_CONTROL_LIMIT_NUMBER, &
                                             VERSION_CONTROL_LIMIT_REVISION
        use file_input_output_mod, only: is_file_directory
        use value_mod, only: value_type
        use data_mod, only: symbol_value, set
        type(string_type), intent(in) :: files(:)
        type(handle_type), intent(in) :: buffer
        logical, intent(in) :: shortlog
        character(len=*), intent(in) :: start_revision
        type(version_control_log_limit_type), intent(in) :: limit
        type(status_type), intent(out) :: status
        logical, intent(out) :: limit_unsupported
        type(string_type), allocatable :: flags(:)
        type(handle_type) :: old
        type(value_type) :: saved
        type(status_type) :: st
        character(len=:), allocatable :: start, range
        character(len=24) :: limit_text

        limit_unsupported = .false.
        ! `vc-do-command' creates the buffer, but we need it before running
        ! the command.
        call version_control_setup_buffer(buffer, status)
        if (.not. status%ok()) return
        ! If the buffer exists from a previous invocation it might be
        ! read-only.
        call symbol_value(value_type(name="inhibit_read_only"), saved, st)
        call set(value_type(name="inhibit_read_only"), value_type(.true.), st)
        ! In some parts of Git's revision and revision range
        ! notation, an empty string is equivalent to "HEAD", but not
        ! everywhere.  For simplicity we'll always be explicit.
        start = start_revision
        if (len(start) == 0) start = "HEAD"
        old = current_buffer()
        call set_buffer(buffer, st)
        flags = strings("log", "--no-color")

        if (version_control_git_print_log_follow .and. size(files) == 1) then
            ! "--follow" on directories or multiple files is broken
            ! https://debbugs.gnu.org/cgi/bugreport.cgi?bug=8756
            ! https://debbugs.gnu.org/cgi/bugreport.cgi?bug=16422
            if (.not. is_file_directory(files(1)%text)) call append_string(flags, "--follow")
        end if

        if (shortlog) then
            call append_all(flags, strings("--graph", "--decorate", "--date=short", &
                                           "--pretty=tformat:"//version_control_git_root_log_format%format, "--abbrev-commit"))
            call append_switches(flags, version_control_git_shortlog_switches)
        else
            call append_switches(flags, version_control_git_log_switches)
        end if

        if (limit%kind == VERSION_CONTROL_LIMIT_NUMBER) then
            write (limit_text, "(i0)") limit%n
            call append_all(flags, strings("-n", trim(limit_text)))
        end if

        if (version_control_log_view_type() == VERSION_CONTROL_LOG_VIEW_WITH_DIFF) call append_string(flags, "-p")
        ! An empty string LIMIT doesn't make sense given the
        ! specification of this VERSION_CONTROL backend function, and is tricky to
        ! deal with in combination with Git's double-dot notation for
        ! specifying revision ranges.  So discard it right away.
        range = start

        if (limit%kind == VERSION_CONTROL_LIMIT_REVISION) then
            if (len(limit%revision) > 0) range = limit%revision//".."//start
        end if

        call append_string(flags, range)
        call append_string(flags, "--")
        call version_control_git_command(VERSION_CONTROL_OKSTATUS_ASYNCHRONOUS, files, flags, status, buffer=buffer)
        call set_buffer(old, st)
        call set(value_type(name="inhibit_read_only"), saved, st)
    end subroutine version_control_git_print_log

    !> Major mode for Git logs.  Port of vc-git.el:vc-git-log-view-mode.
    recursive subroutine version_control_git_log_view_mode(status)
        use version_control_mod, only: version_control_log_view_type, version_control_shortlog, &
                                       VERSION_CONTROL_LOG_VIEW_LOG_UNINTEGRATED, VERSION_CONTROL_LOG_VIEW_LONG, &
                                       VERSION_CONTROL_LOG_VIEW_LOG_SEARCH, VERSION_CONTROL_LOG_VIEW_WITH_DIFF, &
                                       VERSION_CONTROL_LOG_VIEW_SHORT, VERSION_CONTROL_LOG_VIEW_LOG_OUTGOING, &
                                       VERSION_CONTROL_LOG_VIEW_LOG_INCOMING, VERSION_CONTROL_LOG_VIEW_MERGEBASE
        use log_view_mod, only: set_log_view_file_regular_expression, set_log_view_message_regular_expression, &
                                set_log_view_per_file_logs, set_log_view_expanded_log_entry_function, &
                                set_log_view_font_lock_keywords
        type(status_type), intent(out) :: status
        type(font_lock_keyword_type), allocatable :: keywords(:)
        character(len=*), parameter :: TAB = achar(9)
        character(len=*), parameter :: EMAIL = "[A-Za-z0-9_.+-]+@[A-Za-z0-9_.-]+"
        character(len=:), allocatable :: message_regular_expression
        logical :: saved, short, expand
        integer :: view

        if (.not. associated(major_modes%version_control_git_log_view_mode%execute)) call define_log_view_mode()
        call major_modes%version_control_git_log_view_mode%begin(saved, status)
        if (.not. status%ok()) return
        view = version_control_log_view_type()
        ! Don't have file markers, so use impossible regexp.
        call set_log_view_file_regular_expression("\`a\`")
        call set_log_view_per_file_logs(.false.)

        if (view == VERSION_CONTROL_LOG_VIEW_LOG_UNINTEGRATED) then
            short = version_control_shortlog()
        else
            short = .not. (view == VERSION_CONTROL_LOG_VIEW_LONG .or. view == VERSION_CONTROL_LOG_VIEW_LOG_SEARCH .or. view &
                           == VERSION_CONTROL_LOG_VIEW_WITH_DIFF)
        end if

        if (short) then
            message_regular_expression = version_control_git_root_log_format%regular_expression
        else
            message_regular_expression = "^commit +\([0-9a-z]+\)"
        end if

        call set_log_view_message_regular_expression(message_regular_expression)

        ! Allow expanding short log entries.
        if (view == VERSION_CONTROL_LOG_VIEW_LOG_UNINTEGRATED) then
            expand = version_control_shortlog()
        else
            expand = view == VERSION_CONTROL_LOG_VIEW_SHORT .or. view == VERSION_CONTROL_LOG_VIEW_LOG_OUTGOING .or. &
                     view == VERSION_CONTROL_LOG_VIEW_LOG_INCOMING .or. view == VERSION_CONTROL_LOG_VIEW_MERGEBASE
        end if

        ! (setq truncate-lines t): `truncate_lines' is not ported.
        if (expand) call set_log_view_expanded_log_entry_function(version_control_git_expanded_log_entry)

        if (short) then
            allocate (keywords(1))
            keywords(1) = font_lock_keyword(version_control_git_root_log_format%regular_expression, &
                                            highlights=version_control_git_root_log_format%highlights)
        else
            allocate (keywords(8))
            keywords(1) = font_lock_keyword(message_regular_expression, face="change_log_acknowledgment", subexp=1)
            ! Handle the case:
            ! user: foo@bar
            keywords(2) = font_lock_keyword("^\(?:Author\|Commit\):[ "//TAB//"]+\("//EMAIL//"\)", &
                                            face="change_log_email", subexp=1)
            ! Handle the case:
            ! user: FirstName LastName <foo@bar>
            keywords(3) = font_lock_keyword("^\(?:Author\|Commit\):[ "//TAB//"]+\([^<(]+?\)[ "//TAB//"]*[(<]\(" &
                                            //EMAIL//"\)[>)]", highlights=two("change_log_name", "change_log_email"))
            keywords(4) = font_lock_keyword("^ +\(?:\(?:[Aa]cked\|[Ss]igned-[Oo]ff\)-[Bb]y:\)[ "//TAB//"]+\(" &
                                            //EMAIL//"\)", face="change_log_name", subexp=1)
            keywords(5) = font_lock_keyword("^ +\(?:\(?:[Aa]cked\|[Ss]igned-[Oo]ff\)-[Bb]y:\)[ "//TAB//"]+\([^<(]+?\)[ " &
                                            //TAB//"]*[(<]\("//EMAIL//"\)[>)]", &
                                            highlights=two("change_log_name", "change_log_email"))
            keywords(6) = font_lock_keyword("^Merge: \([0-9a-z]+\) \([0-9a-z]+\)", &
                                            highlights=two("change_log_acknowledgment", "change_log_acknowledgment"))
            keywords(7) = font_lock_keyword("^\(?:Date:   \|AuthorDate: \|CommitDate: \)\(.+\)", &
                                            face="change_log_date", subexp=1)
            keywords(8) = font_lock_keyword("^summary:[ "//TAB//"]+\(.+\)", face="log_view_message", subexp=1)
        end if

        call set_log_view_font_lock_keywords(keywords)
        call major_modes%version_control_git_log_view_mode%end(saved, status)

    contains

        ! The highlights (1 'FACE1) (2 'FACE2).
        function two(face1, face2) result(h)
            character(len=*), intent(in) :: face1, face2
            type(font_lock_highlight_type) :: h(2)

            h(1) = font_lock_highlight(1, face=face1)
            h(2) = font_lock_highlight(2, face=face2)
        end function two
    end subroutine version_control_git_log_view_mode

    !> Move to the log entry for REVISION.
    !> REVISION may have the form BRANCH, BRANCH~N,
    !> or BRANCH^ (where "^" can be repeated).  FOUND, when present, tells
    !> whether the entry was found.  Port of
    !> vc-git.el:vc-git-show-log-entry.
    recursive subroutine show_version_control_git_log_entry(revision, found)
        use search_mod, only: search_forward, string_match, match_end_in_string => match_end, &
                              match_beginning_in_string => match_beginning
        use editing_commands_mod, only: beginning_of_line
        character(len=*), intent(in) :: revision
        logical, intent(out), optional :: found
        integer(int64) :: position, count
        type(status_type) :: st

        position = 0
        call goto_character(point_minimum())

        if (len(revision) > 0) then
            count = 0

            if (string_match("~\([0-9]\)\'", revision, st) > 0) then
                count = 1 + (iachar(revision(len(revision):len(revision))) - iachar("0"))
            else if (string_match("\^+\'", revision, st) > 0) then
                count = 1 + (match_end_in_string(0, st) - match_beginning_in_string(0, st))
            end if

            if (count > 0) then
                call search_forward(LF//"commit "//revision, position, st, noerror=SEARCH_NOERROR, count=count)
            else
                call search_forward(LF//"commit "//revision, position, st, noerror=SEARCH_NOERROR)
            end if
        end if

        if (present(found)) found = position > 0
        call beginning_of_line()
    end subroutine show_version_control_git_log_entry

    !> The expanded log entry of REVISION: its full log message.  Port of
    !> vc-git.el:vc-git-expanded-log-entry.
    recursive subroutine version_control_git_expanded_log_entry(revision, text, found)
        use search_mod, only: replace_match
        use editing_commands_mod, only: forward_line
        use editing_functions_mod, only: is_end_of_buffer, buffer_string
        character(len=*), intent(in) :: revision
        character(len=:), allocatable, intent(out) :: text
        logical, intent(out) :: found
        type(string_type), allocatable :: flags(:)
        type(string_type) :: none(0)
        type(handle_type) :: old, temporary
        type(status_type) :: st
        integer(int64) :: position, shortage

        found = .false.
        text = ""
        call temporary_begin(old, temporary)
        flags = strings("log", revision, "-1", "--no-color")
        call append_switches(flags, version_control_git_log_switches)
        call append_string(flags, "--")
        call version_control_git_command(VERSION_CONTROL_OKSTATUS_UNLESS_SIGNAL, none, flags, st, current=.true.)
        call goto_character(point_minimum())

        if (.not. is_end_of_buffer()) then
            ! Indent the expanded log entry.
            do
                call regular_expression_search_forward("^  ", position, st, noerror=SEARCH_NOERROR)
                if (position == 0) exit
                call replace_match("", st)
                shortage = forward_line(1_int64)
            end do

            text = buffer_string()
            found = .true.
        end if

        call temporary_end(old, temporary)
    end subroutine version_control_git_expanded_log_entry

    ! Define Git's Log View mode.  Not a port.
    subroutine define_log_view_mode()
        use log_view_mod, only: initialize_log_view
        use add_log_mod, only: initialize_add_log

        ! (require 'add-log): we need the faces of add-log.
        if (.not. associated(major_modes%change_log_mode%execute)) call initialize_add_log()
        if (.not. associated(major_modes%log_view_mode%execute)) call initialize_log_view()
        call major_modes%version_control_git_log_view_mode%define("version_control_git_log_view_mode", "Git-Log-View", &
                                                                  version_control_git_log_view_mode, &
                                                           version_control_git_log_view_mode_hook, parent=major_modes%log_view_mode)
        version_control_git_log_view_mode_map = major_modes%version_control_git_log_view_mode%map
        commands%version_control_git_log_view_mode%execute => version_control_git_log_view_mode
    end subroutine define_log_view_mode

    ! The default `version_control_git_root_log_format'.  Not a port.
    subroutine initialize_root_log_format()
        if (allocated(version_control_git_root_log_format%format)) return
        version_control_git_root_log_format%format = "%d%h..: %an %ad %s"
        ! The first shy group matches the characters drawn by --graph.
        ! We use numbered groups because `log-view-message-re' wants the
        ! revision number to be group 1.
        version_control_git_root_log_format%regular_expression = "^\(?:[*/\| ]+ \)?\(?2: ([^)]+)\)?\(?1:[0-9a-z]+\)\.\.: " &
                                                             //"\(?3:.*?\)[ "//achar(9)//"]+\(?4:[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}\)"
        allocate (version_control_git_root_log_format%highlights(4))
        version_control_git_root_log_format%highlights(1) = font_lock_highlight(1, face="log_view_message")
        version_control_git_root_log_format%highlights(2) = font_lock_highlight(2, face="change_log_list", laxmatch=.true.)
        version_control_git_root_log_format%highlights(3) = font_lock_highlight(3, face="change_log_name")
        version_control_git_root_log_format%highlights(4) = font_lock_highlight(4, face="change_log_date")
    end subroutine initialize_root_log_format

    ! ------------------------------------------------------------------
    ! VC-Dir
    ! ------------------------------------------------------------------

    !> Escape filename NAME if necessary.  Port of
    !> vc-git.el:vc-git-escape-file-name.
    function version_control_git_escape_file_name(name) result(escaped)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: escaped
        integer :: k

        if (scan(name, LF//achar(9)//'"\') == 0) then
            escaped = name
            return
        end if

        escaped = '"'

        do k = 1, len(name)
            select case (name(k:k))
            case (LF)
                escaped = escaped//"\n"
            case (achar(9))
                escaped = escaped//"\t"
            case ("\")
                escaped = escaped//"\\"
            case ('"')
                escaped = escaped//'\"'
            case default
                escaped = escaped//name(k:k)
            end select
        end do

        escaped = escaped//'"'
    end function version_control_git_escape_file_name

    !> Return a string describing the file type based on its permissions,
    !> and its face.  Port of vc-git.el:vc-git-file-type-as-string.
    subroutine version_control_git_file_type_as_string(old_permission, new_permission, string, face)
        integer, intent(in) :: old_permission, new_permission
        character(len=:), allocatable, intent(out) :: string, face
        character(len=12) :: buffer
        integer :: old_type, new_type

        old_type = ishft(old_permission, -9)
        new_type = ishft(new_permission, -9)
        string = ""

        select case (new_type)
        case (64)  ! File.
            select case (old_type)
            case (80)
                string = "   (type change symlink -> file)"
            case (112)
                string = "   (type change subproject -> file)"
            case default
                continue
            end select
        case (80)  ! Symlink.
            select case (old_type)
            case (64)
                string = "   (type change file -> symlink)"
            case (112)
                string = "   (type change subproject -> symlink)"
            case default
                string = "   (symlink)"
            end select
        case (112)  ! Subproject.
            select case (old_type)
            case (64)
                string = "   (type change file -> subproject)"
            case (80)
                string = "   (type change symlink -> subproject)"
            case default
                string = "   (subproject)"
            end select
        case (72)  ! Directory (internal, not a real git state).
            continue
        case (0)  ! Deleted or unknown.
            select case (old_type)
            case (80)
                string = "   (symlink)"
            case (112)
                string = "   (subproject)"
            case default
                continue
            end select
        case default
            write (buffer, "(o0)") new_type
            string = "   (unknown type "//trim(buffer)//")"
        end select

        if (len(string) > 0) then
            face = "font_lock_comment_face"
        else if (new_type == 72) then
            string = "/"
            face = ""
        else
            face = ""
        end if
    end subroutine version_control_git_file_type_as_string

    !> Return a string describing the copy or rename associated with an
    !> entry of state STATE, or an empty string if none.  Port of
    !> vc-git.el:vc-git-rename-as-string.
    function version_control_git_rename_as_string(state, extra) result(string)
        use version_control_hooks_mod, only: VERSION_CONTROL_RENAME_NONE, VERSION_CONTROL_RENAME_COPY
        integer, intent(in) :: state
        type(version_control_directory_entry_type), intent(in) :: extra
        character(len=:), allocatable :: string

        string = ""
        if (.not. extra%has_extra .or. extra%rename_state == VERSION_CONTROL_RENAME_NONE) return

        if (extra%rename_state == VERSION_CONTROL_RENAME_COPY) then
            string = "   (copied from "
        else if (state == VERSION_CONTROL_ADDED) then
            string = "   (renamed from "
        else
            string = "   (renamed to "
        end if

        string = string//version_control_git_escape_file_name(extra%original_name)//")"
    end function version_control_git_rename_as_string

    !> Format a permission change as string.  Port of
    !> vc-git.el:vc-git-permissions-as-string.
    function version_control_git_permissions_as_string(old_permission, new_permission, has) result(string)
        integer, intent(in) :: old_permission, new_permission
        logical, intent(in) :: has
        character(len=:), allocatable :: string

        if (.not. has .or. iand(73, ieor(old_permission, new_permission)) == 0) then
            string = "  "
        else if (iand(73, old_permission) == 0) then
            string = "+x"
        else
            string = "-x"
        end if
    end function version_control_git_permissions_as_string

    !> Pretty-printer for the vc-dir-fileinfo structure.  Port of
    !> vc-git.el:vc-git-dir-printer (without the mouse maps and help
    !> echoes).
    recursive subroutine version_control_git_directory_printer(info)
        use version_control_directory_mod, only: insert_with_face
        type(version_control_directory_fileinfo_type), intent(in) :: info
        character(len=:), allocatable :: display_state, face, type_string, type_face
        character(len=12) :: padded
        logical :: isdir
        integer :: state, old_permission, new_permission

        isdir = allocated(info%directory)
        state = info%entry%state

        if (isdir) then
            display_state = ""
        else if (allocated(info%display_state)) then
            display_state = info%display_state
        else
            display_state = version_control_state_name(state)
        end if

        old_permission = 0
        new_permission = 0

        if (info%entry%has_extra) then
            old_permission = info%entry%old_permission
            new_permission = info%entry%new_permission
        end if

        call insert_with_face("  ", "")

        if (info%marked) then
            call insert_with_face("*", "version_control_directory_mark_indicator")
        else
            call insert_with_face(" ", "version_control_directory_mark_indicator")
        end if

        call insert_with_face("  ", "")

        if (display_state == "up-to-date") then
            face = "version_control_directory_status_up_to_date"
        else if (display_state == "missing" .or. display_state == "conflict" .or. display_state == "committing") then
            face = "version_control_directory_status_warning"
        else if (display_state == "ignored") then
            face = "version_control_directory_status_ignored"
        else
            face = "version_control_directory_status_edited"
        end if

        if (len(display_state) >= 12) then
            call insert_with_face(display_state, face)
        else
            padded = display_state
            call insert_with_face(padded, face)
        end if

        call insert_with_face("  ", "")
        call insert_with_face(version_control_git_permissions_as_string(old_permission, new_permission, info%entry%has_extra), &
                              "version_control_directory_header")
        call insert_with_face("    ", "")

        if (isdir) then
            call insert_with_face(version_control_git_escape_file_name(info%entry%name), "version_control_directory_directory")
        else
            call insert_with_face(version_control_git_escape_file_name(info%entry%name), "version_control_directory_file")
        end if

        call version_control_git_file_type_as_string(old_permission, new_permission, type_string, type_face)
        call insert_with_face(type_string, type_face)

        if (isdir) then
            call insert_with_face(version_control_git_rename_as_string(VERSION_CONTROL_STATE_UNKNOWN, info%entry), &
                                  "font_lock_comment_face")
        else
            call insert_with_face(version_control_git_rename_as_string(state, info%entry), "font_lock_comment_face")
        end if
    end subroutine version_control_git_directory_printer

    ! Update FILENAME of the computation K to FILE_STATE, with FILE_INFO.
    ! Port of vc-git.el:vc-git-dir-status-update-file.
    subroutine directory_status_update_file(k, filename, file_state, file_info)
        integer, intent(in) :: k
        character(len=*), intent(in) :: filename
        integer, intent(in) :: file_state
        type(version_control_directory_entry_type), intent(in) :: file_info
        type(version_control_directory_entry_type), allocatable :: grown(:)
        integer :: j, i

        associate (s => directory_states(k))
            do j = 1, s%n
                if (s%hash(j)%name == filename) exit
            end do

            if (j > s%n) then
                if (.not. allocated(s%hash)) allocate (s%hash(16))

                if (s%n == size(s%hash)) then
                    allocate (grown(2*s%n))

                    do i = 1, s%n
                        grown(i) = s%hash(i)
                    end do

                    call move_alloc(grown, s%hash)
                end if

                s%n = s%n + 1
                j = s%n
            end if

            s%hash(j) = file_info
            s%hash(j)%name = filename
            s%hash(j)%state = file_state

            ! (delete filename files)
            if (allocated(s%files)) then
                s%files = pack(s%files, [(s%files(i)%text /= filename, i=1, size(s%files))])
            end if
        end associate
    end subroutine directory_status_update_file

    ! (vc-git-create-extra-fileinfo OLD-PERM NEW-PERM RENAME-STATE ORIG-NAME).
    ! Not a port.
    function extra_fileinfo(old_permission, new_permission, rename_state, original_name) result(info)
        use version_control_hooks_mod, only: VERSION_CONTROL_RENAME_NONE
        integer, intent(in) :: old_permission, new_permission
        integer, intent(in), optional :: rename_state
        character(len=*), intent(in), optional :: original_name
        type(version_control_directory_entry_type) :: info

        info%has_extra = .true.
        info%old_permission = old_permission
        info%new_permission = new_permission
        info%rename_state = VERSION_CONTROL_RENAME_NONE
        if (present(rename_state)) info%rename_state = rename_state
        if (present(original_name)) info%original_name = original_name
    end function extra_fileinfo

    ! (string-to-number S 8).  Not a port.
    integer function octal(s)
        character(len=*), intent(in) :: s
        integer :: ios

        read (s, "(o12)", iostat=ios) octal
        if (ios /= 0) octal = 0
    end function octal

    !> Process sentinel for the various dir-status stages.  Port of
    !> vc-git.el:vc-git-after-dir-status-stage.
    recursive subroutine version_control_git_after_directory_status_stage(action, status)
        use version_control_dispatcher_mod, only: version_control_action_type
        use version_control_hooks_mod, only: VERSION_CONTROL_RENAME_COPY, VERSION_CONTROL_RENAME_RENAME
        type(version_control_action_type), intent(in) :: action
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: files(:)
        type(version_control_directory_entry_type), allocatable :: result(:)
        type(handle_type) :: old
        character(len=:), allocatable :: name, state, new_name
        integer(int64) :: found
        type(status_type) :: st
        integer :: k, next_stage, permission, old_permission, new_permission, j, s
        logical :: had_files

        status = status_type()
        k = action%n

        if (.not. is_buffer_live(action%buffer)) then
            directory_states(k)%live = .false.
            return
        end if

        old = current_buffer()
        call set_buffer(action%buffer, st)
        files = directory_states(k)%files
        had_files = size(files) > 0
        next_stage = STAGE_DONE
        ! First stage is always update-index.
        !   After that, if no commits yet, ls-files-added.
        !   Otherwise (there are commits), diff-index then ls-files-missing.
        !     After ls-files-missing, if FILES is not empty, ls-files-up-to-date.
        !     After ls-files-missing, if FILES     nil, ls-files-conflict.
        ! Then always ls-files-unknown.
        ! Finally, if FILES is not empty, ls-files-ignored.
        call goto_character(point_minimum())

        select case (directory_states(k)%stage)
        case (STAGE_UPDATE_INDEX)
            if (is_version_control_git_empty_db()) then
                next_stage = STAGE_LS_FILES_ADDED
            else
                next_stage = STAGE_DIFF_INDEX
            end if
        case (STAGE_LS_FILES_ADDED)
            next_stage = STAGE_LS_FILES_UNKNOWN

            do
                call regular_expression_search_forward("\([0-7]\{6\}\) [0-9a-f]\{40\} 0"//achar(9)//"\([^"//NUL//"]+\)"//NUL, &
                                                       found, &
                                                       st, noerror=SEARCH_NOERROR)
                if (found == 0) exit
                new_permission = octal(match_in_buffer(1))
                name = match_in_buffer(2)
                call directory_status_update_file(k, name, VERSION_CONTROL_ADDED, extra_fileinfo(0, new_permission))
            end do
        case (STAGE_LS_FILES_UP_TO_DATE)
            next_stage = STAGE_LS_FILES_UNKNOWN

            do
                call regular_expression_search_forward("\([0-7]\{6\}\) [0-9a-f]\{40\} \([0-3]\)"//achar(9)//"\([^"//NUL//"]+\)" &
                                                       //NUL, &
                                                       found, st, noerror=SEARCH_NOERROR)
                if (found == 0) exit
                permission = octal(match_in_buffer(1))
                state = match_in_buffer(2)
                name = match_in_buffer(3)

                if (state == "0") then
                    if (.not. in_hash(k, name)) then
                        ! `diff-index' stage has not produced a more precise info.
                        call directory_status_update_file(k, name, VERSION_CONTROL_UP_TO_DATE, extra_fileinfo(permission, &
                                                                                                              permission))
                    end if
                else
                    ! `diff-index' assigns `edited' status to conflicted
                    ! files, so we can't do the above in both cases.
                    call directory_status_update_file(k, name, VERSION_CONTROL_CONFLICT, extra_fileinfo(permission, permission))
                end if
            end do
        case (STAGE_LS_FILES_CONFLICT)
            next_stage = STAGE_LS_FILES_UNKNOWN

            ! It's enough to look for "3" to notice a conflict.
            do
                call regular_expression_search_forward("\([0-7]\{6\}\) [0-9a-f]\{40\} 3"//achar(9)//"\([^"//NUL//"]+\)"//NUL, &
                                                       found, &
                                                       st, noerror=SEARCH_NOERROR)
                if (found == 0) exit
                permission = octal(match_in_buffer(1))
                name = match_in_buffer(2)
                call directory_status_update_file(k, name, VERSION_CONTROL_CONFLICT, extra_fileinfo(permission, permission))
            end do
        case (STAGE_LS_FILES_MISSING)
            if (had_files) then
                next_stage = STAGE_LS_FILES_UP_TO_DATE
            else
                next_stage = STAGE_LS_FILES_CONFLICT
            end if

            call nul_separated(VERSION_CONTROL_MISSING)
        case (STAGE_LS_FILES_UNKNOWN)
            if (had_files) next_stage = STAGE_LS_FILES_IGNORED
            call nul_separated(VERSION_CONTROL_UNREGISTERED)
        case (STAGE_LS_FILES_IGNORED)
            call nul_separated(VERSION_CONTROL_IGNORED)
        case default
            continue
        case (STAGE_DIFF_INDEX)
            ! This is output from 'git diff-index' without --cached.
            ! Therefore this stage compares HEAD and the working tree and
            ! ignores the index (cf. git-diff-index(1) "RAW OUTPUT FORMAT").
            ! In particular that means it cannot distinguish between
            ! `removed' (deletion staged) and `missing' (deleted only in
            ! working tree).  Set them all to `removed' and then do
            ! `ls-files-missing' as the next stage to possibly change some
            ! of those just set to `removed', to `missing'.
            next_stage = STAGE_LS_FILES_MISSING

            do
                call regular_expression_search_forward(":\([0-7]\{6\}\) \([0-7]\{6\}\) [0-9a-f]\{40\} [0-9a-f]\{40\} " &
                                                       //"\(\([ADMUT]\)"//NUL//"\([^"//NUL//"]+\)\|\([CR]\)[0-9]*"//NUL//"\([^" &
                                                       //NUL//"]+\)"//NUL//"\([^"//NUL//"]+\)\)"//NUL, found, st, &
                                                       noerror=SEARCH_NOERROR)
                if (found == 0) exit
                old_permission = octal(match_in_buffer(1))
                new_permission = octal(match_in_buffer(2))
                state = match_in_buffer(4)
                if (len(state) == 0) state = match_in_buffer(6)
                name = match_in_buffer(5)
                if (len(name) == 0) name = match_in_buffer(7)
                new_name = match_in_buffer(8)

                if (len(new_name) > 0) then
                    ! Copy or rename.
                    if (state(1:1) == "C") then
                        call directory_status_update_file(k, new_name, VERSION_CONTROL_ADDED, &
                                                          extra_fileinfo(old_permission, new_permission, &
                                                                         VERSION_CONTROL_RENAME_COPY, name))
                    else
                        call directory_status_update_file(k, name, VERSION_CONTROL_REMOVED, extra_fileinfo(0, 0, &
                                                                                           VERSION_CONTROL_RENAME_RENAME, new_name))
                        call directory_status_update_file(k, new_name, VERSION_CONTROL_ADDED, &
                                                          extra_fileinfo(old_permission, new_permission, &
                                                                         VERSION_CONTROL_RENAME_RENAME, name))
                    end if
                else
                    select case (state(1:1))
                    case ("M")
                        s = VERSION_CONTROL_EDITED
                    case ("A", "C")
                        s = VERSION_CONTROL_ADDED
                    case ("D")
                        s = VERSION_CONTROL_REMOVED
                    case default  ! U and T: FIXME
                        s = VERSION_CONTROL_EDITED
                    end select

                    call directory_status_update_file(k, name, s, extra_fileinfo(old_permission, new_permission))
                end if
            end do
        end select

        ! If we had files but now we don't, it's time to stop.
        if (had_files) then
            if (size(directory_states(k)%files) == 0) next_stage = STAGE_DONE
        end if

        directory_states(k)%stage = next_stage
        directory_states(k)%files = files

        if (next_stage /= STAGE_DONE) then
            call version_control_git_directory_status_goto_stage(k, status)
            call set_buffer(old, st)
        else
            ! (maphash (lambda (key value) (push (cons key value) result)))
            allocate (result(directory_states(k)%n))

            do j = 1, directory_states(k)%n
                result(directory_states(k)%n + 1 - j) = directory_states(k)%hash(j)
            end do

            directory_states(k)%live = .false.
            call set_buffer(old, st)
            call directory_states(k)%update_function(result, directory_states(k)%buffer, .false.)
        end if

    contains

        ! Every "NAME\0" of the output, with STATE.
        subroutine nul_separated(state)
            integer, intent(in) :: state

            do
                call regular_expression_search_forward("\([^"//NUL//"]*?\)"//NUL, found, st, noerror=SEARCH_NOERROR, count=1_int64)
                if (found == 0) exit
                call directory_status_update_file(k, match_in_buffer(1), state, extra_fileinfo(0, 0))
            end do
        end subroutine nul_separated

        logical function in_hash(k, name)
            integer, intent(in) :: k
            character(len=*), intent(in) :: name
            integer :: j

            in_hash = .false.

            do j = 1, directory_states(k)%n
                if (directory_states(k)%hash(j)%name == name) then
                    in_hash = .true.
                    return
                end if
            end do
        end function in_hash
    end subroutine version_control_git_after_directory_status_stage

    ! Run the command of the current stage of the computation K in the
    ! current buffer, then `version_control_git_after_directory_status_stage'.  Port of
    ! vc-git.el:vc-git-dir-status-goto-stage.
    recursive subroutine version_control_git_directory_status_goto_stage(k, status)
        use version_control_dispatcher_mod, only: version_control_action_type, version_control_exec_after
        integer, intent(in) :: k
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: files(:), flags(:)
        type(string_type) :: none(0)
        type(version_control_action_type) :: action
        type(status_type) :: st
        integer :: allowed_exit

        ! TODO: Look into reimplementing this using `git status --porcelain=v2'.
        files = directory_states(k)%files
        allowed_exit = 1
        call erase_buffer(st)

        select case (directory_states(k)%stage)
        case (STAGE_UPDATE_INDEX)
            if (size(files) > 0) then
                call version_control_git_command(VERSION_CONTROL_OKSTATUS_ASYNCHRONOUS, files, strings("add", "--refresh", "--"), &
                                                 status, &
                                                 current=.true.)
                ! git-add exits 128 if some of FILES are untracked;
                ! we can ignore that (bug#79999).
                allowed_exit = 128
            else
                call version_control_git_command(VERSION_CONTROL_OKSTATUS_ASYNCHRONOUS, none, strings("update-index", &
                                                                                                      "--refresh"), status, &
                                                 current=.true.)
            end if
        case (STAGE_LS_FILES_ADDED, STAGE_LS_FILES_UP_TO_DATE)
            call version_control_git_command(VERSION_CONTROL_OKSTATUS_ASYNCHRONOUS, files, strings("ls-files", "-z", "-c", "-s", &
                                                                                                   "--"), status, &
                                             current=.true.)
        case (STAGE_LS_FILES_CONFLICT)
            call version_control_git_command(VERSION_CONTROL_OKSTATUS_ASYNCHRONOUS, files, strings("ls-files", "-z", "-u", "--"), &
                                             status, current=.true.)
        case (STAGE_LS_FILES_MISSING)
            call version_control_git_command(VERSION_CONTROL_OKSTATUS_ASYNCHRONOUS, files, strings("ls-files", "-z", "-d", "--"), &
                                             status, current=.true.)
        case (STAGE_LS_FILES_UNKNOWN)
            call version_control_git_command(VERSION_CONTROL_OKSTATUS_ASYNCHRONOUS, files, strings("ls-files", "-z", "-o", &
                                                                                                   "--exclude-standard", "--"), &
                                             status, current=.true.)
        case (STAGE_LS_FILES_IGNORED)
            flags = strings("ls-files", "-z", "-o", "-i", "--directory", "--no-empty-directory", "--exclude-standard")
            call append_string(flags, "--")
            call version_control_git_command(VERSION_CONTROL_OKSTATUS_ASYNCHRONOUS, files, flags, status, current=.true.)
        case (STAGE_DIFF_INDEX)
            ! --relative added in Git 1.5.5.
            call version_control_git_command(VERSION_CONTROL_OKSTATUS_ASYNCHRONOUS, files, strings("diff-index", "--relative", &
                                                                                                   "-z", "-M", "HEAD", "--"), &
                                             status, current=.true.)
        case default
            continue
        end select

        if (.not. status%ok()) return
        ! (vc-run-delayed-success ALLOWED-EXIT ...)
        action%execute => version_control_git_after_directory_status_stage
        action%n = k
        action%buffer = current_buffer()
        action%okstatus = allowed_exit
        call version_control_exec_after(action, status)
    end subroutine version_control_git_directory_status_goto_stage

    !> Return a list of (FILE STATE EXTRA) entries for DIRECTORY.  Port of
    !> vc-git.el:vc-git-dir-status-files.
    recursive subroutine version_control_git_directory_status_files(directory, files, update, buffer, status)
        character(len=*), intent(in) :: directory
        type(string_type), intent(in) :: files(:)
        procedure(version_control_directory_update_procedure) :: update
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        type(directory_status_state_type) :: state
        integer :: k

        ! Further things that would have to be fixed later:
        ! - how to handle unregistered directories
        ! - how to support vc-dir on a subdir of the project tree
        if (len(directory) < 0) return
        if (.not. allocated(directory_states)) allocate (directory_states(0))

        do k = 1, size(directory_states)
            if (.not. directory_states(k)%live) exit
        end do

        if (k > size(directory_states)) directory_states = [directory_states, state]
        directory_states(k)%live = .true.
        directory_states(k)%stage = STAGE_UPDATE_INDEX
        directory_states(k)%files = files
        directory_states(k)%update_function => update
        directory_states(k)%buffer = buffer
        directory_states(k)%n = 0
        if (allocated(directory_states(k)%hash)) deallocate (directory_states(k)%hash)
        call version_control_git_directory_status_goto_stage(k, status)
    end subroutine version_control_git_directory_status_files

    !> Return the name of the current branch (FOUND false if HEAD is
    !> detached).  Port of vc-git.el:vc-git-working-branch.
    subroutine version_control_git_working_branch(branch, found)
        character(len=:), allocatable, intent(out) :: branch
        logical, intent(out) :: found

        call version_control_git_out_match(strings("symbolic-ref", "HEAD"), "^\(refs/heads/\)?\(.+\)$", 2, branch, found)
    end subroutine version_control_git_working_branch

    !> Run `git ARGUMENTS...' and return match for group number GROUP of REGULAR_EXPRESSION.
    !> FOUND is false if the output does not match.  The exit status is
    !> ignored.  Port of vc-git.el:vc-git--out-match.
    subroutine version_control_git_out_match(arguments, regular_expression, group, match, found)
        type(string_type), intent(in) :: arguments(:)
        character(len=*), intent(in) :: regular_expression
        integer, intent(in) :: group
        character(len=:), allocatable, intent(out) :: match
        logical, intent(out) :: found
        type(string_type), allocatable :: rest(:)
        type(handle_type) :: old, temporary
        character(len=:), allocatable :: out
        type(status_type) :: st
        logical :: ok
        integer :: k

        allocate (rest(0))

        do k = 2, size(arguments)
            call append_string(rest, arguments(k)%text)
        end do

        call temporary_begin(old, temporary)
        ok = version_control_git_out_ok(arguments(1)%text, rest)
        out = buffer_string()
        call temporary_end(old, temporary)
        match = ""
        found = string_match(regular_expression, out, st) > 0
        if (found) match = match_string(group, out)
    end subroutine version_control_git_out_match

    !> The URL of the remote REMOTE_NAME ("origin" by default) of the
    !> repository of FILE_OR_DIRECTORY.  Port of vc-git.el:vc-git-repository-url.
    subroutine version_control_git_repository_url(file_or_directory, url, remote_name)
        character(len=*), intent(in) :: file_or_directory
        character(len=:), allocatable, intent(out) :: url
        character(len=*), intent(in), optional :: remote_name
        type(string_type) :: none(0)
        type(handle_type) :: old, temporary
        character(len=:), allocatable :: root, option_name, saved
        type(status_type) :: st
        logical :: found

        call version_control_git_root(file_or_directory, root, found)
        call temporary_begin(old, temporary)
        saved = buffer_directory()
        if (found) call set_buffer_directory(root)
        ! The "get-url" subcommand of "git remote" was new in git 2.7.0;
        ! "git config" also works in older versions.  -- rgr, 15-Aug-23.
        option_name = "remote.origin.url"
        if (present(remote_name)) option_name = "remote."//remote_name//".url"
        call version_control_git_command(0, none, strings("config", "--get", option_name), st, current=.true.)
        url = buffer_string()
        if (len(url) > 0) url = url(1:len(url) - 1)
        call temporary_end(old, temporary)
    end subroutine version_control_git_repository_url

    ! Return headers for branch-related information.  Port of
    ! vc-git.el:vc-git-dir--branch-headers.
    subroutine version_control_git_directory_branch_headers(parts)
        type(fortmacs_widget_for_object_collections_part_type), allocatable, intent(inout) :: parts(:)
        character(len=:), allocatable :: branch, branch_merge, branch_remote, tracking, remote_url
        logical :: found, has_merge, has_remote

        call version_control_git_working_branch(branch, found)

        if (found) then
            call version_control_git_out_match(strings("config", "branch."//branch//".merge"), "^\(refs/heads/\)?\(.+\)$", 2, &
                                               branch_merge, has_merge)
            has_remote = .false.
            if (has_merge) call version_control_git_out_match(strings("config", "branch."//branch//".remote"), &
                                                              "\([^"//LF//"]+\)", 1, branch_remote, has_remote)

            if (has_merge .and. has_remote) then
                if (branch_remote == ".") then
                    tracking = branch_merge
                    remote_url = "none (tracking local branch)"
                else
                    tracking = branch_remote//"/"//branch_merge
                    call version_control_git_repository_url(buffer_directory(), remote_url, branch_remote)
                end if
            end if
        else
            branch = "none (detached HEAD)"
        end if

        call fmt("Branch", branch)
        if (allocated(tracking)) call fmt("Tracking", tracking)
        if (allocated(remote_url)) call fmt("Remote", remote_url)

    contains

        subroutine fmt(key, value)
            character(len=*), intent(in) :: key, value
            character(len=11) :: padded

            padded = key
            call append_part(parts, padded//": ", "version_control_directory_header")
            call append_part(parts, value, "version_control_directory_header_value")
            call append_part(parts, LF, "")
        end subroutine fmt
    end subroutine version_control_git_directory_branch_headers

    ! Append TEXT with FACE to PARTS.  Not a port.
    subroutine append_part(parts, text, face)
        type(fortmacs_widget_for_object_collections_part_type), allocatable, intent(inout) :: parts(:)
        character(len=*), intent(in) :: text, face
        type(fortmacs_widget_for_object_collections_part_type) :: part

        part%text = text
        part%face = face
        parts = [parts, part]
    end subroutine append_part

    !> Resolve .git/PATH for the current working tree (.git for none).
    !> In particular, handle the case where this is a linked working
    !> tree, such that .git is a plain file.
    !>
    !> See the --git-dir and --git-path options to git-rev-parse(1).  Port of
    !> vc-git.el:vc-git--git-path.
    function version_control_git_git_path(path) result(full)
        character(len=*), intent(in), optional :: path
        character(len=:), allocatable :: full
        character(len=:), allocatable :: out
        type(status_type) :: st
        logical :: ok

        if (present(path)) then
            if (len(path) > 0) then
                ! Canonicalize in this branch because --git-dir always returns
                ! an absolute file name.
                call version_control_git_execute_command_string(strings("rev-parse", "--git-path", path), out, ok)
                call expand_file_name(trim_right(out), full, st, buffer_directory())
                return
            end if
        end if

        call version_control_git_execute_command_string(strings("rev-parse", "--git-dir"), out, ok)
        full = trim_right(out)//"/"
    end function version_control_git_git_path

    !> The Git commands in progress in this worktree.  Port of
    !> vc-git.el:vc-git--cmds-in-progress.
    function get_version_control_git_commands_in_progress() result(commands)
        type(version_control_git_commands_in_progress_type) :: commands
        character(len=:), allocatable :: gitdir

        gitdir = version_control_git_git_path()
        ! See contrib/completion/git-prompt.sh in git.git.
        commands%revert = file_exists(in_gitdir("REVERT_HEAD"))
        commands%cherry_pick = file_exists(in_gitdir("CHERRY_PICK_HEAD"))
        commands%rebase = is_file_directory(in_gitdir("rebase-merge"))
        if (.not. commands%rebase) commands%rebase = file_exists(in_gitdir("rebase-apply/rebasing"))
        commands%am = file_exists(in_gitdir("rebase-apply/applying"))
        commands%merge = file_exists(in_gitdir("MERGE_HEAD"))
        commands%bisect = file_exists(in_gitdir("BISECT_START"))

    contains

        ! (expand-file-name NAME gitdir)
        function in_gitdir(name) result(f)
            character(len=*), intent(in) :: name
            character(len=:), allocatable :: f
            type(status_type) :: st

            call expand_file_name(name, f, st, gitdir)
        end function in_gitdir
    end function get_version_control_git_commands_in_progress

    ! Return headers for Git commands in progress in this worktree.  Port of
    ! vc-git.el:vc-git-dir--in-progress-headers.
    subroutine version_control_git_directory_in_progress_headers(parts)
        type(fortmacs_widget_for_object_collections_part_type), allocatable, intent(inout) :: parts(:)
        type(version_control_git_commands_in_progress_type) :: commands

        commands = get_version_control_git_commands_in_progress()

        ! For now just a heading, key bindings can be added
        ! later for various bisect actions.
        if (commands%bisect) then
            call append_part(parts, "Bisect     : in progress", "version_control_directory_status_warning")
            call append_part(parts, LF, "")
        end if

        if (commands%rebase) then
            call append_part(parts, "Rebase     : in progress", "version_control_directory_status_warning")
            call append_part(parts, LF, "")
        end if
    end subroutine version_control_git_directory_in_progress_headers

    !> The Git headers of a VC-Dir buffer: the branch, its tracking branch
    !> and remote, and the commands in progress.  Port of
    !> vc-git.el:vc-git-dir-extra-headers (the stash headers are deferred:
    !> they need button.el).
    recursive subroutine version_control_git_directory_extra_headers(directory, parts)
        character(len=*), intent(in) :: directory
        type(fortmacs_widget_for_object_collections_part_type), allocatable, intent(out) :: parts(:)
        character(len=:), allocatable :: saved

        allocate (parts(0))
        saved = buffer_directory()
        call set_buffer_directory(directory)
        ! Each helper returns a list of headers.  Each header must be a
        ! propertized string with no final newline.
        call version_control_git_directory_branch_headers(parts)
        call version_control_git_directory_in_progress_headers(parts)
        call set_buffer_directory(saved)
        ! (string-join ... "\n"): no newline after the last header.
        if (size(parts) > 0) parts = parts(1:size(parts) - 1)
    end subroutine version_control_git_directory_extra_headers

    ! ------------------------------------------------------------------
    ! Conflicts
    ! ------------------------------------------------------------------

    !> Return the list of files with conflicts in DIRECTORY.  Port of
    !> vc-git.el:vc-git-conflicted-files.
    subroutine version_control_git_conflicted_files(directory, files)
        character(len=*), intent(in) :: directory
        type(string_type), allocatable, intent(out) :: files(:)
        type(string_type), allocatable :: pushed(:)
        character(len=:), allocatable :: output, line, state, file, full
        type(status_type) :: st
        integer :: i, j, k
        logical :: ok

        allocate (pushed(0))
        call version_control_git_execute_command_string(strings("status", "--porcelain", "--"), output, ok, directory)
        i = 1

        do while (i <= len(output))
            j = index(output(i:), LF)

            if (j == 0) then
                line = output(i:)
                i = len(output) + 1
            else
                line = output(i:i + j - 2)
                i = i + j
            end if

            if (len(line) == 0) cycle

            if (string_match("\([ MADRCU?!][ MADRCU?!]\) \(.+\)\(?: -> \(.+\)\)?", line, st) > 0) then
                state = match_string(1, line)
                file = match_string(2, line)

                ! See git-status(1).
                select case (state)
                case ("AU", "UD", "UA", "DU", "AA", "UU")
                    call expand_file_name(file, full, st, directory)
                    call append_string(pushed, full)
                case default
                    continue
                end select
            end if
        end do

        ! Upstream adds each file at the front: the last line's file first.
        allocate (files(0))

        do k = size(pushed), 1, -1
            call append_string(files, pushed(k)%text)
        end do
    end subroutine version_control_git_conflicted_files

    !> Call "git add" if the conflict markers have been removed.  Port of
    !> vc-git.el:vc-git-resolve-when-done.
    recursive subroutine resolve_version_control_git_when_done(status)
        type(status_type), intent(out) :: status
        type(version_control_git_commands_in_progress_type) :: commands
        type(string_type), allocatable :: none(:), conflicted(:)
        character(len=:), allocatable :: file, root
        integer(int64) :: here, found
        logical :: has_root

        status = status_type()
        if (current == 0) return
        if (.not. allocated(buffers(current)%filename)) return
        file = buffers(current)%filename
        here = point()
        call goto_character(point_minimum())
        call regular_expression_search_forward("^<<<<<<< ", found, status, noerror=SEARCH_NOERROR)

        resolve: block
            if (.not. status%ok()) exit resolve
            if (found /= 0) exit resolve
            call version_control_git_command(0, strings(file), strings("add"), status)
            if (.not. status%ok()) exit resolve

            ! Doing a merge or rebase-like operation, so bug#20292
            ! doesn't apply.
            !
            ! If we were to 'git reset' in the middle of a
            ! cherry-pick, for example, it would effectively abort
            ! the cherry-pick, losing the user's progress.
            if (version_control_git_resolve_conflicts%choice == VERSION_CONTROL_GIT_RESOLVE_CONFLICTS_UNSTAGE_MAYBE) then
                commands = get_version_control_git_commands_in_progress()

                if (.not. (commands%merge .or. commands%rebase .or. commands%am .or. commands%revert .or. commands%cherry_pick)) &
                    then
                    call version_control_git_root(file, root, has_root)
                    if (.not. has_root) root = ""
                    call version_control_git_conflicted_files(root, conflicted)

                    if (size(conflicted) == 0) then
                        allocate (none(0))
                        call version_control_git_command(0, none, strings("reset"), status)
                        if (.not. status%ok()) exit resolve
                    end if
                end if
            end if

            call version_control_resynch_buffer(file, buffer_directory(), status, keep=.true., noquery=VERSION_CONTROL_NOQUERY_YES)
            if (.not. status%ok()) exit resolve
            ! Remove the hook so that it is not called multiple times.
            call after_save_hook%remove(resolve_version_control_git_when_done, local=.true.)
        end block resolve

        call goto_character(here)
    end subroutine resolve_version_control_git_when_done

    !> Activate `simple_merge_mode' if there is a conflict.  Port of
    !> vc-git.el:vc-git-find-file-hook.
    subroutine version_control_git_find_file_hook()
        character(len=:), allocatable :: file
        integer(int64) :: here, found
        type(status_type) :: st

        if (current == 0) return
        if (.not. allocated(buffers(current)%filename)) return
        file = buffers(current)%filename
        if (version_control_state(file, git_backend) /= VERSION_CONTROL_CONFLICT) return
        here = point()
        call goto_character(point_minimum())
        call regular_expression_search_forward("^<<<<<<< ", found, st, noerror=SEARCH_NOERROR)
        call goto_character(here)
        if (found == 0) return
        call start_simple_merge_session(st)

        if (.not. (version_control_git_resolve_conflicts%is_off() .or. &
                   (version_control_git_resolve_conflicts%choice == VERSION_CONTROL_GIT_RESOLVE_CONFLICTS_DEFAULT &
                    .and. .not. version_control_resolve_conflicts))) then
            call after_save_hook%prepend(resolve_version_control_git_when_done, local=.true.)
        end if

        call message_version_control_unresolved_conflicts(file)
    end subroutine version_control_git_find_file_hook

    ! Annotate (vc-annotate.el's backend functions)
    ! ------------------------------------------------------------------
    !> Start `git blame' of revision REVISION of FILE, its output in BUFFER.
    !> Port of vc-git.el:vc-git-annotate-command (the output is decoded as
    !> UTF-8: vc-git--asciify-coding-system is not needed).
    recursive subroutine version_control_git_annotate_command(file, buffer, revision, status)
        character(len=*), intent(in) :: file
        type(handle_type), intent(in) :: buffer
        character(len=*), intent(in) :: revision
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: flags(:), switches(:)
        type(string_type) :: none(0)
        character(len=:), allocatable :: name

        name = file_relative_name(file)
        flags = strings("blame", "--date=short")
        call version_control_switches(git_backend_index(), "annotate", switches)
        call append_all(flags, switches)
        call append_all(flags, strings(revision, "--", name))
        call version_control_git_command(VERSION_CONTROL_OKSTATUS_ASYNCHRONOUS, none, flags, status, buffer=buffer)
    end subroutine version_control_git_annotate_command

    !> The time of the `git blame' line at point, as fractional days
    !> (FOUND false if there is none), moving point past the annotation.
    !> Port of vc-git.el:vc-git-annotate-time.
    recursive subroutine version_control_git_annotate_time(days, found)
        real(real64), intent(out) :: days
        logical, intent(out) :: found
        type(decoded_time_type) :: decoded
        type(time_type) :: encoded
        type(status_type) :: st
        character(len=:), allocatable :: date, time, zone
        integer(int64) :: position
        logical :: has_time

        days = 0
        found = .false.
        call regular_expression_search_forward("^[0-9a-f^]+[^()]+(.*?\([0-9]+-[0-9]+-[0-9]+\)" &
                                               //"\(?: \([0-9]+:[0-9]+:[0-9]+\) \([-+0-9]+\)\)? +[0-9]+) ", position, st, &
                                               noerror=SEARCH_NOERROR)
        if (.not. st%ok() .or. position == 0) return
        date = match_string(1)
        time = match_string(2, matched=has_time)
        zone = match_string(3)
        ! (ignore-errors (iso8601-parse dt)), with decoded-time-set-defaults.
        call parse_annotate_date(date, decoded, found)
        if (.not. found) return

        if (has_time) then
            call parse_annotate_time(time, zone, decoded, found)
            if (.not. found) return
        end if

        encoded = encode_time(decoded, status=st)
        found = st%ok()
        ! (vc-annotate-convert-time ...): fractional days.
        if (found) days = real(encoded)/86400.0_real64
    end subroutine version_control_git_annotate_time

    !> The year, month and day of DATE ("YYYY-MM-DD") in DECODED, local
    !> time at midnight (FOUND false if DATE is not a date).  Port of
    !> iso8601.el:iso8601-parse-date for the dates `git blame' prints, with
    !> time-date.el:decoded-time-set-defaults.
    subroutine parse_annotate_date(date, decoded, found)
        character(len=*), intent(in) :: date
        type(decoded_time_type), intent(out) :: decoded
        logical, intent(out) :: found
        integer :: first, second, ios

        found = .false.
        first = index(date, "-")
        second = index(date, "-", back=.true.)
        if (first <= 1 .or. second <= first + 1 .or. second == len(date)) return
        read (date(1:first - 1), *, iostat=ios) decoded%year
        if (ios /= 0) return
        read (date(first + 1:second - 1), *, iostat=ios) decoded%month
        if (ios /= 0) return
        read (date(second + 1:), *, iostat=ios) decoded%day
        if (ios /= 0) return
        found = decoded%month >= 1 .and. decoded%month <= 12 .and. decoded%day >= 1 .and. decoded%day <= 31
    end subroutine parse_annotate_date

    !> Set the hour, minute and second of TIME ("HH:MM:SS") and the time
    !> zone ZONE ("+HHMM", "+HH:MM" or "+HH") in DECODED (FOUND false if
    !> they are not valid).  Port of iso8601.el:iso8601-parse-time and
    !> iso8601.el:iso8601-parse-zone for the times `git blame' prints.
    subroutine parse_annotate_time(time, zone, decoded, found)
        character(len=*), intent(in) :: time, zone
        type(decoded_time_type), intent(inout) :: decoded
        logical, intent(out) :: found
        character(len=:), allocatable :: digits
        integer :: hours, minutes, ios
        integer(int64) :: seconds

        found = .false.
        if (len(time) /= 8 .or. len(zone) < 3) return
        if (time(3:3) /= ":" .or. time(6:6) /= ":") return
        read (time(1:2), *, iostat=ios) decoded%hour
        if (ios /= 0) return
        read (time(4:5), *, iostat=ios) decoded%minute
        if (ios /= 0) return
        read (time(7:8), *, iostat=ios) seconds
        if (ios /= 0) return
        decoded%second = time_type(seconds=seconds)
        if (zone(1:1) /= "+" .and. zone(1:1) /= "-") return
        digits = zone(2:)
        if (len(digits) == 5) digits = digits(1:2)//digits(4:5)
        if (len(digits) /= 2 .and. len(digits) /= 4) return
        if (verify(digits, "0123456789") /= 0) return
        read (digits(1:2), *) hours
        minutes = 0
        if (len(digits) == 4) read (digits(3:4), *) minutes
        decoded%zone = 3600*hours + 60*minutes
        if (zone(1:1) == "-") decoded%zone = -decoded%zone
        decoded%daylight_saving_time = DECODED_TIME_DAYLIGHT_SAVING_TIME_NO
        found = decoded%hour <= 24 .and. decoded%minute <= 59 .and. seconds <= 60
    end subroutine parse_annotate_time

    !> The revision of the `git blame' line at point, and the file it had
    !> there when `git blame' names one ("" otherwise); FOUND false if the
    !> line has none.  Port of
    !> vc-git.el:vc-git-annotate-extract-revision-at-line.
    recursive subroutine extract_version_control_git_annotate_revision_at_line(revision, file, found)
        use editing_commands_mod, only: beginning_of_line
        character(len=:), allocatable, intent(out) :: revision, file
        logical, intent(out) :: found
        character(len=:), allocatable :: file_name, root
        type(status_type) :: st
        integer(int64) :: original_point
        logical :: has_file, has_root

        revision = ""
        file = ""
        original_point = point()
        call beginning_of_line()
        found = looking_at("\^?\([0-9a-f]+\) \(\([^(]+\) \)?", st)

        if (found) then
            revision = match_string(1)
            file_name = match_string(2, matched=has_file)

            if (has_file) then
                ! Remove trailing whitespace from the file name.
                file_name = trim(match_string(3))
                call version_control_git_root(buffer_directory(), root, has_root)
                call expand_file_name(file_name, file, st, root)
                if (.not. st%ok()) file = file_name
            end if
        end if

        call goto_character(original_point)
    end subroutine extract_version_control_git_annotate_revision_at_line

    !> Git-specific version of `version_control_next_revision'.  Port of
    !> vc-git.el:vc-git-next-revision.
    subroutine version_control_git_next_revision(file, revision, next, found)
        use search_mod, only: search_forward
        character(len=*), intent(in) :: file, revision
        character(len=:), allocatable, intent(out) :: next
        logical, intent(out) :: found
        character(len=:), allocatable :: saved_directory, root, file_name, current_revision, symbol
        type(handle_type) :: old, temporary
        type(status_type) :: st
        integer(int64) :: shortage, p, position
        logical :: ok

        next = ""
        found = .false.
        saved_directory = buffer_directory()
        call version_control_git_root(file, root, ok)
        if (ok) call set_buffer_directory(root)
        file_name = file_relative_name(file)
        call temporary_begin(old, temporary)
        ok = version_control_git_out_ok("rev-list", strings(maybe_abbreviation(), "-1", revision, "--", file_name))

        if (ok) then
            call goto_character(point_maximum())
            ok = is_beginning_of_line()
        end if

        if (ok) then
            shortage = forward_line(-1_int64)
            ok = shortage == 0 .and. is_beginning_of_buffer()
        end if

        if (ok) call buffer_substring(point(), point_maximum() - 1, current_revision, st)
        call temporary_end(old, temporary)

        if (ok) then
            call temporary_begin(old, temporary)
            ok = version_control_git_out_ok("rev-list", strings(maybe_abbreviation(), "HEAD", "--", file_name))

            if (ok) then
                call goto_character(point_minimum())
                call search_forward(current_revision, position, st, noerror=SEARCH_NOERROR)
                ok = position > 0
            end if

            if (ok) then
                shortage = forward_line(-1_int64)
                ok = shortage == 0
            end if

            if (ok) then
                p = point()
                shortage = forward_line(1_int64)
                call buffer_substring(p, point() - 1, next, st)
                found = st%ok()
            end if

            call temporary_end(old, temporary)
        end if

        call set_buffer_directory(saved_directory)

        if (found) then
            call version_control_git_symbolic_commit(next, symbol, ok)
            if (ok) next = symbol
        end if
    end subroutine version_control_git_next_revision

    !> The files that revision REVISION added, deleted, copied or renamed, with
    !> their names before and after it (none when
    !> `version_control_git_print_log_follow' is true).  Port of
    !> vc-git.el:vc-git-file-name-changes.
    recursive subroutine version_control_git_file_name_changes(revision, changes)
        character(len=*), intent(in) :: revision
        type(version_control_file_name_change_type), allocatable, intent(out) :: changes(:)
        type(string_type) :: none(0)
        type(version_control_file_name_change_type) :: change
        type(string_type), allocatable :: flags(:), switches(:)
        type(handle_type) :: old, temporary
        type(status_type) :: st
        character(len=:), allocatable :: root, kind, name
        integer(int64) :: position
        logical :: has_root

        allocate (changes(0))
        call version_control_git_root(buffer_directory(), root, has_root)
        call temporary_begin(old, temporary)

        if (.not. version_control_git_print_log_follow) then
            flags = strings("diff", "--name-status", "--diff-filter=ADCR", revision//"^", revision)
            call version_control_switches(git_backend_index(), "file_name_changes", switches)
            call append_all(flags, switches)
            call version_control_git_command(VERSION_CONTROL_OKSTATUS_ANY, none, flags, st, current=.true.)
        end if

        call goto_character(point_minimum())

        do
            call regular_expression_search_forward("^\([ADCR]\)[0-9]*"//TAB//"\([^"//LF//TAB//"]+\)\(?:"//TAB//"\([^"//LF//TAB &
                                                   //"]+\)\)?", position, st, noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. position == 0) exit
            kind = match_string(1)
            change = version_control_file_name_change_type()

            select case (kind)
            case ("A")
                change%new = expand_in(match_string(2))
            case ("D")
                change%old = expand_in(match_string(2))
            case default
                ! "C" and "R".
                change%old = expand_in(match_string(2))
                name = match_string(3)
                change%new = expand_in(name)
            end select

            changes = [changes, change]
        end do

        call temporary_end(old, temporary)

    contains

        ! (expand-file-name NAME root).
        function expand_in(name) result(full)
            character(len=*), intent(in) :: name
            character(len=:), allocatable :: full
            type(status_type) :: est

            call expand_file_name(name, full, est, root)
            if (.not. est%ok()) full = name
        end function expand_in
    end subroutine version_control_git_file_name_changes

    ! ------------------------------------------------------------------
    ! Tags, branches, pulling and pushing
    ! ------------------------------------------------------------------

    !> STRING without its trailing whitespace.  Port of
    !> subr-x.el:string-trim-right.
    function trim_right(string) result(trimmed)
        character(len=*), intent(in) :: string
        character(len=:), allocatable :: trimmed
        integer :: n

        n = len(string)

        do while (n > 0)
            if (scan(string(n:n), " "//achar(9)//LF//achar(13)) == 0) exit
            n = n - 1
        end do

        trimmed = string(1:n)
    end function trim_right

    !> Run `git ARGUMENTS...' and return standard output as a string.
    !> The exit status is ignored.  Port of vc-git.el:vc-git--out-str.
    function out_string(arguments) result(string)
        type(string_type), intent(in) :: arguments(:)
        character(len=:), allocatable :: string
        type(string_type), allocatable :: rest(:)
        type(handle_type) :: old, temporary
        logical :: ok

        rest = arguments(2:)
        call temporary_begin(old, temporary)
        ok = version_control_git_out_ok(arguments(1)%text, rest)
        string = buffer_string()
        call temporary_end(old, temporary)
    end function out_string

    !> Return the existing branches, as a list of strings.
    !> The first element of the list is the current branch (unallocated
    !> when there is none).  Port of vc-git.el:vc-git-branches.
    subroutine version_control_git_branches(branches)
        type(string_type), allocatable, intent(out) :: branches(:)
        type(handle_type) :: old, temporary
        type(status_type) :: st
        integer(int64) :: shortage
        integer :: code

        allocate (branches(1))
        call temporary_begin(old, temporary)
        ! 'git branch' is a porcelain command whose output could change in
        ! the future.
        call version_control_git_execute(.true., "for-each-ref", strings("--format=%(HEAD) %(refname:short)", "refs/heads/"), &
                                         code, st)
        call goto_character(point_minimum())

        do while (point() < point_maximum())
            if (looking_at("^\([ *]\) \(.+\)$", st)) then
                if (match_in_buffer(1) == "*") then
                    branches(1)%text = match_in_buffer(2)
                else
                    call append_string(branches, match_in_buffer(2))
                end if
            end if

            shortage = forward_line(1_int64)
        end do

        call temporary_end(old, temporary)
    end subroutine version_control_git_branches

    !> Return the configured remote branches for BRANCH.
    !> BRANCH defaults to the current branch.
    !> If there is a configured upstream, return the remote-tracking branch
    !> as UPSTREAM.  If there is a distinct configured push remote,
    !> return the remote-tracking branch there as PUSH.
    !> A configured push remote that's just the same as the upstream remote is
    !> ignored because that means we're not actually in a triangular workflow.
    !> Port of vc-git.el:vc-git--branch-remotes.
    subroutine version_control_git_branch_remotes(remotes, branch)
        type(version_control_git_branch_remotes_type), intent(out) :: remotes
        character(len=*), intent(in), optional :: branch
        character(len=:), allocatable :: name, pull, merge, push
        logical :: found

        ! Possibly we could simplify this using @{push}, but that may involve
        ! an unwanted dependency on the setting of push.default.
        if (present(branch)) then
            name = branch
        else
            call version_control_git_working_branch(name, found)
            if (.not. found) name = "nil"
        end if

        pull = trim_right(out_string(strings("config", "branch."//name//".remote")))
        merge = trim_right(out_string(strings("config", "branch."//name//".merge")))
        if (index(merge, "refs/heads/") == 1) merge = merge(len("refs/heads/") + 1:)
        push = trim_right(out_string(strings("config", "branch."//name//".pushRemote")))
        if (len(push) == 0) push = trim_right(out_string(strings("config", "remote.pushDefault")))
        if (len(pull) > 0 .and. len(merge) > 0) remotes%upstream = pull//"/"//merge
        if (len(push) == 0) return
        if (push == pull .and. len(push) == len(pull)) return
        remotes%push = push//"/"//name
    end subroutine version_control_git_branch_remotes

    !> Read the start point of a new branch in DIRECTORY (the root of the
    !> current repository if absent), defaulting to the current branch.
    !> Port of vc-git.el:vc-git--read-start-point.
    recursive subroutine version_control_git_read_start_point(start, status, directory)
        use version_control_mod, only: version_control_read_revision
        character(len=:), allocatable, intent(out) :: start
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: directory
        type(string_type), allocatable :: branches(:), files(:)
        character(len=:), allocatable :: root
        logical :: found

        call version_control_git_branches(branches)

        if (present(directory)) then
            call append_string(files, directory)
        else
            call version_control_git_root(buffer_directory(), root, found)
            call append_string(files, root)
        end if

        if (allocated(branches(1)%text)) then
            call version_control_read_revision("Start point (default "//branches(1)%text//"): ", start, status, files, &
                                               git_backend_index(), &
                                               default=branches(1)%text)
        else
            call version_control_read_revision("Start point: ", start, status, files, git_backend_index())
        end if
    end subroutine version_control_git_read_start_point

    !> In DIRECTORY, make a tag called NAME, or with IS_BRANCH a branch called NAME
    !> (asking for its start point) that is then checked out.  Port of
    !> vc-git.el:vc-git-create-tag.
    recursive subroutine version_control_git_create_tag(directory, name, is_branch, status)
        use basic_subprograms_mod, only: read_y_or_n
        use error_mod, only: USER_ERROR_CODE
        character(len=*), intent(in) :: directory, name
        logical, intent(in) :: is_branch
        type(status_type), intent(out) :: status
        type(string_type) :: none(0)
        type(string_type), allocatable :: flags(:)
        character(len=:), allocatable :: saved, start_point
        logical :: proceed
        integer :: code

        saved = buffer_directory()
        call set_buffer_directory(directory)
        start_point = ""

        if (is_branch) then
            call version_control_git_read_start_point(start_point, status, directory)

            if (.not. status%ok()) then
                call set_buffer_directory(saved)
                return
            end if
        end if

        call version_control_git_command(VERSION_CONTROL_OKSTATUS_UNLESS_SIGNAL, none, strings("update-index", "--refresh"), &
                                         status, exit_status=code)

        if (status%ok() .and. code /= 0) then
            call read_y_or_n("Modified files exist.  Proceed? ", proceed, status)

            if (status%ok() .and. .not. proceed) then
                if (is_branch) then
                    call set_error(status, USER_ERROR_CODE, "Can't create branch with modified files")
                else
                    call set_error(status, USER_ERROR_CODE, "Can't create tag with modified files")
                end if
            end if
        end if

        if (status%ok()) then
            if (is_branch) then
                flags = strings("checkout", "-b", name)
                if (len(start_point) > 0) call append_string(flags, start_point)
                call version_control_git_command(0, none, flags, status)
            else
                call version_control_git_command(0, none, strings("tag", name), status)
            end if
        end if

        call set_buffer_directory(saved)
    end subroutine version_control_git_create_tag

    !> In DIRECTORY, check out the tag or branch NAME.  Port of
    !> vc-git.el:vc-git-retrieve-tag.
    recursive subroutine version_control_git_retrieve_tag(directory, name, update, status)
        character(len=*), intent(in) :: directory, name
        logical, intent(in) :: update
        type(status_type), intent(out) :: status
        type(string_type) :: none(0)
        character(len=:), allocatable :: saved

        if (update .and. .false.) return
        saved = buffer_directory()
        call set_buffer_directory(directory)
        call version_control_git_command(0, none, strings("checkout", name), status)
        call set_buffer_directory(saved)
    end subroutine version_control_git_retrieve_tag

    !> Run COMMAND (a string; either push or pull) on the current Git branch.
    !> If PROMPT is true, prompt for the Git command to run.  EXTRA_ARGUMENTS
    !> follow COMMAND.  PROCESS is the asynchronous process.  Port of
    !> vc-git.el:vc-git--pushpull.
    recursive subroutine version_control_git_pushpull(command, prompt, extra_arguments, status, process)
        use file_input_output_mod, only: expand_file_name
        use process_mod, only: set_process_query_on_exit_flag, set_process_filter
        use buffer_mod, only: get_buffer
        use version_control_dispatcher_mod, only: version_control_do_asynchronous_command, version_control_exec_after, &
                                                  version_control_action_type, version_control_set_asynchronous_update
        character(len=*), intent(in) :: command
        logical, intent(in) :: prompt
        type(string_type), intent(in) :: extra_arguments(:)
        type(status_type), intent(out) :: status
        type(handle_type), intent(out) :: process
        type(string_type), allocatable :: arguments(:)
        type(version_control_action_type) :: action
        type(handle_type) :: buffer, old
        type(status_type) :: st
        character(len=:), allocatable :: root, full_root, buffer_name, git_program, compile_command
        logical :: found
        integer :: k

        process = null_handle()
        call version_control_git_root(buffer_directory(), root, found)

        if (.not. found) then
            call set_error(status, GENERIC_ERROR_CODE, "Not in a Git repository: "//buffer_directory())
            return
        end if

        call expand_file_name(root, full_root, st)
        if (.not. st%ok()) full_root = root
        buffer_name = "*vc-git : "//full_root//"*"
        git_program = version_control_git_program
        ! TODO if pushing, prompt if no default push location - cf bzr.
        arguments = strings(command)
        call append_all(arguments, extra_arguments)
        call version_control_do_asynchronous_command(buffer_name, root, git_program, arguments, status, process, prompt, &
                                                     "version_control_git_history")
        if (.not. status%ok()) return
        call set_process_query_on_exit_flag(process, .true., st)
        ! "git pull" includes progress output that uses ^M to move point
        ! to the beginning of the line.  Just translate these to newlines
        ! (but don't do anything with the CRLF sequence).
        call set_process_filter(process, st, pushpull_filter)
        ! The command, edited when prompting: COMMAND and EXTRA-ARGS.
        compile_command = git_program//" "

        do k = 1, size(arguments)
            if (k > 1) compile_command = compile_command//" "
            if (allocated(arguments(k)%text)) compile_command = compile_command//arguments(k)%text
        end do

        if (size(arguments) == 1) compile_command = compile_command//" "
        buffer = get_buffer(buffer_name)
        old = current_buffer()
        call set_buffer(buffer, st)
        action%execute => pushpull_setup
        action%buffer = buffer
        action%s1 = compile_command
        action%s2 = root
        call version_control_exec_after(action, status)
        call set_buffer(old, st)
        if (.not. status%ok()) return
        call version_control_set_asynchronous_update(buffer, status)
    end subroutine version_control_git_pushpull

    !> The process filter of `version_control_git_pushpull': translate the carriage
    !> returns of progress output that are not followed by a newline to
    !> newlines, then insert S as `version_control_process_filter' does.  Port of the
    !> wrapped process filter of vc-git.el:vc-git--pushpull.
    recursive subroutine pushpull_filter(process, s, status)
        use version_control_dispatcher_mod, only: version_control_process_filter
        type(handle_type), intent(in) :: process
        character(len=*), intent(in) :: s
        type(status_type), intent(out) :: status
        character(len=len(s)) :: translated
        integer :: i

        translated = s

        do i = 1, len(s)
            if (s(i:i) /= achar(13)) cycle

            if (i == len(s)) then
                translated(i:i) = LF
            else if (s(i + 1:i + 1) /= LF) then
                translated(i:i) = LF
            end if
        end do

        call version_control_process_filter(process, translated, status)
    end subroutine pushpull_filter

    !> Once the pull or push is done: put its buffer in VC-Compilation mode
    !> with the command (ACTION%S1) and directory (ACTION%S2) to run it
    !> again.  Port of the vc-run-delayed part of
    !> vc-git.el:vc-git--pushpull.
    recursive subroutine pushpull_setup(action, status)
        use version_control_dispatcher_mod, only: version_control_action_type, version_control_compilation_mode
        use compile_mod, only: compile_command, compilation_directory
        type(version_control_action_type), intent(in) :: action
        type(status_type), intent(out) :: status

        status = status_type()
        if (.not. is_buffer_live(action%buffer)) return
        call version_control_compilation_mode(status, version_control_git_error_regular_expressions)
        if (.not. status%ok()) return
        call compile_command%set_local(action%s1)
        call compilation_directory%set_local(action%s2)
    end subroutine pushpull_setup

    !> Pull changes into the current Git branch.
    !> Normally, this runs "git pull", unless there is a configured push
    !> remote, in which case pull from that.
    !> If PROMPT is true, prompt for the Git command to run.  Port of
    !> vc-git.el:vc-git-pull.
    recursive subroutine version_control_git_pull(prompt, status, process)
        logical, intent(in) :: prompt
        type(status_type), intent(out) :: status
        type(handle_type), intent(out) :: process
        type(version_control_git_branch_remotes_type) :: remotes
        integer :: slash

        call version_control_git_branch_remotes(remotes)
        slash = 0
        if (allocated(remotes%push)) slash = index(remotes%push, "/")

        if (slash > 0) then
            call version_control_git_pushpull("pull", prompt, strings("--stat", remotes%push(1:slash - 1), &
                                                                      remotes%push(slash + 1:)), &
                                              status, process)
        else
            ! When the upstream is of the form "REMOTE/BRANCH", 'git pull'
            ! is equivalent to 'git pull REMOTE BRANCH' per git-pull(1).
            call version_control_git_pushpull("pull", prompt, strings("--stat"), status, process)
        end if
    end subroutine version_control_git_pull

    !> Push changes from the current Git branch.
    !> Normally, this runs "git push".  If PROMPT is true, prompt
    !> for the Git command to run.  Port of vc-git.el:vc-git-push.
    recursive subroutine version_control_git_push(prompt, status, process)
        logical, intent(in) :: prompt
        type(status_type), intent(out) :: status
        type(handle_type), intent(out) :: process
        type(string_type) :: none(0)

        call version_control_git_pushpull("push", prompt, none, status, process)
    end subroutine version_control_git_push

    !> The REVISION at the head of the branch at UPSTREAM_LOCATION ("" for
    !> the push remote's or else the upstream's branch), fetched first with
    !> REFRESH or when it is not known yet; FOUND is false when there is
    !> no such branch.  Port of vc-git.el:vc-git-incoming-revision.
    recursive subroutine version_control_git_incoming_revision(upstream_location, refresh, revision, found, status)
        character(len=*), intent(in) :: upstream_location
        logical, intent(in) :: refresh
        character(len=:), allocatable, intent(out) :: revision
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        type(version_control_git_branch_remotes_type) :: remotes
        type(string_type) :: none(0)
        type(handle_type) :: old, temporary
        type(status_type) :: st
        character(len=:), allocatable :: revision_name, parsed
        logical :: known

        status = status_type()
        revision = ""
        found = .false.

        if (len(upstream_location) > 0) then
            revision_name = upstream_location
        else
            call version_control_git_branch_remotes(remotes)

            if (allocated(remotes%push)) then
                revision_name = remotes%push
            else if (allocated(remotes%upstream)) then
                revision_name = remotes%upstream
            else
                return
            end if
        end if

        call version_control_git_revision_parse(revision_name, parsed, known)

        if (refresh .or. .not. known) then
            ! Extract remote from "remote/branch".
            if (index(revision_name, "/") > 0) then
                call version_control_git_command(0, none, strings("fetch", revision_name(1:index(revision_name, "/") - 1)), status)
            else
                call version_control_git_command(0, none, strings("fetch", revision_name), status)
            end if

            if (.not. status%ok()) return
        end if

        ! In order to give none if no such branch.
        call temporary_begin(old, temporary)
        call version_control_git_command(0, none, strings("log", "--max-count=1", "--pretty=format:%H", revision_name), st, &
                                         current=.true.)
        found = st%ok()
        if (found) revision = buffer_string()
        call temporary_end(old, temporary)
    end subroutine version_control_git_incoming_revision

    !> The common ancestor BASE of REV1 and REV2 ("" for HEAD).  Port of
    !> vc-git.el:vc-git-mergebase.
    recursive subroutine version_control_git_mergebase(rev1, rev2, base, status)
        character(len=*), intent(in) :: rev1, rev2
        character(len=:), allocatable, intent(out) :: base
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: out
        logical :: ok

        status = status_type()

        if (len(rev2) > 0) then
            call version_control_git_execute_command_string(strings("merge-base", rev1, rev2), out, ok)
        else
            call version_control_git_execute_command_string(strings("merge-base", rev1, "HEAD"), out, ok)
        end if

        base = ""

        if (ok) then
            base = trim_right(out)
        else
            call set_error(status, GENERIC_ERROR_CODE, "No common ancestor for merge base")
        end if
    end subroutine version_control_git_mergebase

    !> Make the Git backend known to VERSION_CONTROL.  Not a port.
    subroutine initialize_version_control_git()
        integer :: k

        if (.not. allocated(version_control_git_program)) version_control_git_program = "git"
        call define_log_edit_mode()
        call initialize_root_log_format()
        call define_log_view_mode()
        call register_version_control_backend("Git", k)
        git_backend = k

        associate (b => version_control_backends(k))
            b%checkin => version_control_git_checkin
            b%log_edit_mode => version_control_git_log_edit_mode
            b%registered => version_control_git_registered
            b%state => version_control_git_state
            b%working_revision => version_control_git_working_revision
            b%working_revision_symbol => version_control_git_working_revision_symbol
            b%mode_line_string => version_control_git_mode_line_string
            b%root => version_control_git_root
            ! (defalias 'vc-git-responsible-p #'vc-git-root)
            b%responsible_p => version_control_git_root
            b%find_ignore_file => version_control_git_find_ignore_file
            b%checkout_model => version_control_git_checkout_model
            b%register => version_control_git_register
            b%unregister => version_control_git_unregister
            b%revert => version_control_git_revert
            b%revert_files => version_control_git_revert_files
            b%diff => version_control_git_diff
            b%find_revision => version_control_git_find_revision
            b%checkout => version_control_git_checkout
            b%delete_file => version_control_git_delete_file
            b%delete_files => version_control_git_delete_files
            b%rename_file => version_control_git_rename_file
            b%previous_revision => version_control_git_previous_revision
            b%revision_completion_table => version_control_git_revision_table
            b%mark_resolved => version_control_git_mark_resolved
            b%create_repository => version_control_git_create_repository
            b%diff_switches => version_control_git_diff_switches
            b%print_log => version_control_git_print_log
            b%log_view_mode => version_control_git_log_view_mode
            b%show_log_entry => show_version_control_git_log_entry
            b%directory_status_files => version_control_git_directory_status_files
            b%directory_extra_headers => version_control_git_directory_extra_headers
            b%directory_printer => version_control_git_directory_printer
            b%find_file_hook => version_control_git_find_file_hook
            b%annotate_command => version_control_git_annotate_command
            b%annotate_time => version_control_git_annotate_time
            b%annotate_extract_revision_at_line => extract_version_control_git_annotate_revision_at_line
            b%next_revision => version_control_git_next_revision
            b%file_name_changes => version_control_git_file_name_changes
            b%annotate_switches => version_control_git_annotate_switches
            b%revision_granularity = VERSION_CONTROL_REVISION_GRANULARITY_REPOSITORY
            b%update_on_retrieve_tag = .false.
            b%create_tag => version_control_git_create_tag
            b%retrieve_tag => version_control_git_retrieve_tag
            b%pull => version_control_git_pull
            b%push => version_control_git_push
            b%incoming_revision => version_control_git_incoming_revision
            b%mergebase => version_control_git_mergebase
        end associate

        if (.not. allocated(version_control_git_error_regular_expressions)) then
            allocate (version_control_git_error_regular_expressions(1))
            version_control_git_error_regular_expressions(1)%name = "version_control_git"
            version_control_git_error_regular_expressions(1)%regular_expression = "^ \(.+\)\> *|"
            version_control_git_error_regular_expressions(1)%file = 1
            version_control_git_error_regular_expressions(1)%type = 0
        end if
    end subroutine initialize_version_control_git
end module version_control_git_mod
