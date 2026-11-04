! project_mod.f90 -- operations on the current project.
! Ported from GNU Emacs 31.1 lisp/progmodes/project.el, with
! lisp/vc/vc.el vc-default-ignore-completion-table and
! lisp/progmodes/xref.el xref--find-ignores-arguments.
!
! Copyright (C) 2015-2026 Free Software Foundation, Inc.
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

!> Projects: the current project (`get_current_project') is found by the
!> procedures of `project_find_functions' from the current buffer's
!> `default_directory'.  A project is an object extending `project_type',
!> whose type-bound procedures give its root, external roots, ignores,
!> files, buffers and name; a backend extends it and overrides `root'
!> (required) and any of the others.
!>
!> The VC-aware backend (`try_vc_project', `vc_project_type') finds a
!> project root by the markers of the VC backends (`.git', ...) and
!> `project_vc_extra_root_markers', and lists the files with `git
!> ls-files' (other backends with `find').  A transient project
!> (`transient_project_type') is a directory chosen by the user.
!>
!> The known projects are saved in `project_list_file' in namelist syntax
!> (namelist_mod).  The commands are on C-x p (`project_prefix_map').
!>
!> The status of the port (what is Deferred) is in PORTING.md's row for
!> project_mod.
module project_mod
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use commands_mod, only: commands
    use error_mod, only: status_type, set_error, format_error, condition_matches, FM_USER_ERROR, FM_GENERIC_ERROR, &
                         FM_PERMISSION_DENIED
    use handle_mod, only: handle_type, null_handle
    use value_mod, only: value_type, make_list, make_keymap_ref, command_type, vtag, V_STRING
    use data_mod, only: string_setting_type, forward_setting, history_setting_type, find_history
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_or_make_buffer, generate_new_buffer_name, &
                          close_buffer, is_buffer_live, get_buffer, buffer_list, buffer_serial_live
    use insdel_mod, only: insert
    use editfns_mod, only: buffer_string
    use fns_mod, only: count_characters, read_yes_or_no
    use search_mod, only: string_match, re_search_forward, SEARCH_NOERROR, match_beginning, match_end
    use subr_mod, only: shell_quote_argument, match_string, is_version_less_or_equal
    use string_type_mod, only: string_type, append_string
    use fileio_mod, only: expand_file_name, file_name_as_directory, directory_file_name, file_name_directory, &
                          file_name_nondirectory, is_file_directory, file_exists, is_file_name_absolute, &
                          is_directory_name, insert_file_contents, write_region, is_file_regular
    use files_mod, only: abbreviate_file_name, file_relative_name, is_file_in_directory, is_file_equal, &
                         wildcard_to_regexp, find_file, create_directory, locate_dominating_file, &
                         locate_dominating_stop_dir_regexp, save_some_buffers
    use config_mod, only: state_directory
    use sysdep_mod, only: fortmacs_directory_entries
    use keymap_mod, only: make_sparse_keymap, keymap_set, define_key, set_keymap_parent, lookup_key, &
                          key_description, current_global_map, where_is_internal, overriding_terminal_local_map
    use keyboard_mod, only: read_key_sequence
    use callint_mod, only: execute_interactively, current_prefix_arg, prefix_numeric_value, command_of, PREFIX_NONE, &
                           PREFIX_NUMBER
    use minibuf_table_mod, only: completion_table_type, completion_list_table
    use minibuf_mod, only: try_completion, minibuffer_default, completion_ignore_case, &
                           minibuffer_setup_function
    use minibuffer_mod, only: completing_read, read_directory_name, read_file_name, read_file_name_completion_ignore_case, &
                              REQUIRE_MATCH_YES, REQUIRE_MATCH_CONFIRM, read_buffer_completion_ignore_case
    use replace_mod, only: format_prompt
    use xdisp_mod, only: message
    use window_mod, only: switch_to_buffer, display_buffer, pop_to_buffer_same_window, other_buffer
    use derived_mod, only: provided_mode_derived_p
    use namelist_mod, only: namelist_group_type, namelist_item_type, read_namelist, namelist_text, NAMELIST_STRING
    use vc_hooks_mod, only: vc_handled_backends, vc_directory_exclusions, vc_ignore_dir_regexp, &
                            vc_backends, vc_backend_named
    use vc_mod, only: vc_responsible_backend
    use vc_git_mod, only: vc_git_command, vc_git_program_version, vc_git_root, vc_git_use_literal_pathspecs
    use vc_dir_mod, only: vc_dir
    use dired_mode_mod, only: dired
    use shell_mod, only: shell
    use comint_mod, only: comint_check_process
    use compile_mod, only: recompile, compilation_directory
    use grep_mod, only: find_program, grep_find_ignored_files
    use simple_mod, only: execute_process_shell_command
    use buff_menu_mod, only: list_buffers_noselect
    implicit none(type, external)
    private

    public :: init_project, get_current_project, find_project_in_directory, try_vc_project, &
              combine_project_directories, subtract_project_directories, remember_project, forget_project, &
              known_project_roots, read_project_dir, read_project_name, read_project, project_prefixed_buffer_name, &
              find_file_in_project, find_file_in_project_root, find_project_file, find_project_or_external_file, &
              find_project_dir, dired_in_project, vc_dir_in_project, shell_in_project, compile_in_project, &
              recompile_in_project, shell_command_in_project, async_shell_command_in_project, &
              execute_extended_command_in_project, execute_any_command_in_project, &
              execute_prefix_or_any_command_in_project, switch_to_project_buffer, display_project_buffer, &
              list_project_buffers, list_project_buffers_in_buffer_menu, close_project_buffers, &
              save_some_project_buffers, switch_project, remember_projects_under, forget_zombie_projects, &
              forget_projects_under, is_local_project, read_project_file_relative, read_project_file_absolute, &
              project_clear_cache, project_files_in_directory, project_vc_ignores_of, vc_git_project_list_files, &
              is_project_buffer_matching, project_buffers_to_close
    public :: project_and, project_or, project_not, project_major_mode, project_derived_mode, &
              project_buffer_name_regexp, project_buffer_test

    !> A project: what the procedures of `project_find_functions' give.  A
    !> backend extends it and overrides `root' (required) and any of the
    !> other procedures; `transient_project_type' and `vc_project_type' are
    !> the two built in.  Port of project.el's project instance (the
    !> generic functions are the type-bound procedures).
    type, abstract, public :: project_type
    contains
        !> The root directory of the project.  It usually contains the main
        !> build file, dependencies configuration file, etc.  Though neither
        !> is mandatory.  The directory name must be absolute.
        procedure(project_root_procedure), deferred :: root
        !> The external roots: the directories outside of the project that
        !> are still related to it.  If the project deals with source code
        !> then, depending on the languages used, this list should include
        !> the headers search path, load path, class path, and so on.
        procedure :: external_roots => project_external_roots
        !> A human-readable name for the project.  Nominally unique, but not
        !> enforced.
        procedure :: name => project_name
        !> The glob patterns to ignore inside DIR.  Patterns can match both
        !> regular files and directories.  To root an entry, start it with
        !> "./".  To match directories only, end it with "/".  DIR must be
        !> either the root or one of the external roots.
        procedure :: ignores => project_ignores
        !> The files in the directories DIRS (absolute directories: some of
        !> the project roots or external roots or subdirectories of those;
        !> the root when absent).  The default uses `find_program', with the
        !> ignores of each directory.
        procedure :: list_files => project_list_files
        !> All live buffers that belong to the project.  The default
        !> matches each buffer to the root using the buffer's
        !> `default_directory'.
        procedure :: buffers => project_buffers
    end type project_type

    abstract interface
        !> A project's `root'.  Not a port.
        function project_root_procedure(self) result(root)
            import :: project_type
            implicit none(type, external)
            class(project_type), intent(in) :: self
            character(len=:), allocatable :: root
        end function project_root_procedure

        !> A procedure of `project_find_functions': the project containing
        !> DIR, or PROJECT unallocated when the procedure does not apply.
        !> Not a port.
        recursive subroutine project_find_procedure(dir, project, status)
            import :: project_type, status_type
            implicit none(type, external)
            character(len=*), intent(in) :: dir
            class(project_type), allocatable, intent(out) :: project
            type(status_type), intent(out) :: status
        end subroutine project_find_procedure

        !> The test a prompter applies to a project root DIR: whether the
        !> user may select it.  Not a port.
        logical function project_root_test_procedure(dir)
            implicit none(type, external)
            character(len=*), intent(in) :: dir
        end function project_root_test_procedure

        !> `project_prompter': read a project root directory DIR, with
        !> PROMPT, offering only the roots TEST accepts, and only known
        !> projects with REQUIRE_KNOWN.  Not a port.
        recursive subroutine project_prompter_procedure(dir, status, prompt, test, require_known)
            import :: status_type, project_root_test_procedure
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: dir
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: prompt
            procedure(project_root_test_procedure), optional :: test
            logical, intent(in), optional :: require_known
        end subroutine project_prompter_procedure

        !> `project_read_file_name_function': read a file name FILE with
        !> PROMPT among ALL_FILES, with the history named HISTORY and
        !> MB_DEFAULT as the "future history" (M-n).  Not a port.
        recursive subroutine project_read_file_name_procedure(prompt, all_files, file, status, history, mb_default)
            import :: string_type, status_type
            implicit none(type, external)
            character(len=*), intent(in) :: prompt
            type(string_type), intent(in) :: all_files(:)
            character(len=:), allocatable, intent(out) :: file
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: history
            type(string_type), intent(in), optional :: mb_default(:)
        end subroutine project_read_file_name_procedure

        !> `project_buffers_viewer': list the buffers of PROJECT (only the
        !> file-visiting ones with FILES_ONLY).  Not a port.
        recursive subroutine project_buffers_viewer_procedure(project, files_only, status)
            import :: project_type, status_type
            implicit none(type, external)
            class(project_type), intent(in) :: project
            logical, intent(in) :: files_only
            type(status_type), intent(out) :: status
        end subroutine project_buffers_viewer_procedure

        !> A test of a project: whether it is excluded (`project_list_exclude')
        !> or pruned (`project_prune_zombie_projects', given the root).
        !> Not a port.
        logical function project_test_procedure(project)
            import :: project_type
            implicit none(type, external)
            class(project_type), intent(in) :: project
        end function project_test_procedure

        !> A buffer test of a buffer condition (`project_buffer_test'):
        !> whether BUFFER matches.  Not a port.
        logical function project_buffer_test_procedure(buffer)
            import :: handle_type
            implicit none(type, external)
            type(handle_type), intent(in) :: buffer
        end function project_buffer_test_procedure

        !> `project_vc_external_roots_function': the external roots of
        !> the current buffer.  Not a port.
        subroutine project_external_roots_procedure(roots)
            import :: string_type
            implicit none(type, external)
            type(string_type), allocatable, intent(out) :: roots(:)
        end subroutine project_external_roots_procedure
    end interface

    public :: project_root_procedure, project_find_procedure, project_root_test_procedure, &
              project_prompter_procedure, project_read_file_name_procedure, project_buffers_viewer_procedure, &
              project_test_procedure, project_buffer_test_procedure, project_external_roots_procedure

    !> A transient project: a project rooted in a directory the user chose,
    !> including all the files under the directory except for those that
    !> match entries in `vc_directory_exclusions' or
    !> `grep_find_ignored_files_variable'.  Port of project.el's (transient
    !> . DIR) instance.
    type, extends(project_type), public :: transient_project_type
        !> The root directory.
        character(len=:), allocatable :: directory
    contains
        procedure :: root => transient_project_root
    end type transient_project_type

    !> A project of the VC-aware backend: its VC BACKEND (as in
    !> `vc_handled_backends'; "" when the root was found by an extra root
    !> marker outside of any repository) and root DIRECTORY.  Port of
    !> project.el's (vc BACKEND ROOT) instance.
    type, extends(project_type), public :: vc_project_type
        character(len=:), allocatable :: backend
        character(len=:), allocatable :: directory
    contains
        procedure :: root => vc_project_root
        procedure :: external_roots => vc_project_external_roots
        procedure :: name => vc_project_name
        procedure :: ignores => vc_project_ignores
        procedure :: list_files => vc_project_list_files
        procedure :: buffers => vc_project_buffers
    end type vc_project_type

    ! A procedure of `project_find_functions'.
    type :: project_find_entry_type
        procedure(project_find_procedure), pointer, nopass :: fn => null()
    end type project_find_entry_type

    !> The type of `project_find_functions': procedures called in turn
    !> until one gives a project.  Not a port.
    type, public :: project_find_hook_type
        type(project_find_entry_type), allocatable :: entries(:)
    contains
        !> Add FN first (add-hook's default place).
        procedure :: prepend => project_find_hook_prepend
        !> Add FN last (add-hook with APPEND).
        procedure :: append => project_find_hook_append
        !> Remove FN (remove-hook).
        procedure :: remove => project_find_hook_remove
        !> Remove all the procedures.
        procedure :: clear => project_find_hook_clear
        !> Call the procedures with DIR until one gives a project
        !> (run-hook-with-args-until-success).
        procedure :: execute => project_find_hook_execute
    end type project_find_hook_type

    !> An element of `project_vc_backend_markers': the root MARKER of the
    !> VC BACKEND (a base file name or a glob pattern for such).
    type, public :: project_vc_backend_marker_type
        character(len=8) :: backend = ""
        character(len=16) :: marker = ""
    end type project_vc_backend_marker_type

    !> An element of `project_list_exclude': a project root REGEXP (""
    !> for none), or a TEST of the project.
    type, public :: project_list_exclude_type
        character(len=256) :: regexp = ""
        procedure(project_test_procedure), pointer, nopass :: test => null()
    end type project_list_exclude_type

    !> The kinds of node of a buffer condition.
    integer, parameter :: CONDITION_NAME = 1, CONDITION_TEST = 2, CONDITION_MAJOR_MODE = 3, &
                          CONDITION_DERIVED_MODE = 4, CONDITION_NOT = 5, CONDITION_AND = 6, CONDITION_OR = 7

    ! One node of a buffer condition: its KIND, its TEXT (a regexp or a
    ! mode name), its buffer TEST, and for `and', `or' and `not' the
    ! number of conditions it combines.
    type :: project_condition_node_type
        integer :: kind = CONDITION_NAME
        character(len=128) :: text = ""
        procedure(project_buffer_test_procedure), pointer, nopass :: test => null()
        integer :: count = 0
    end type project_condition_node_type

    !> A condition on a buffer, made with `project_buffer_name_regexp',
    !> `project_buffer_test', `project_major_mode', `project_derived_mode',
    !> `project_not', `project_and' and `project_or': its nodes in prefix
    !> order (the tree flattened).
    type, public :: project_buffer_condition_type
        type(project_condition_node_type), allocatable :: nodes(:)
    end type project_buffer_condition_type

    !> A list of buffer conditions (the type of
    !> `project_close_buffer_conditions' and
    !> `project_ignore_buffer_conditions'): a buffer matches it if it
    !> matches any of them.
    type, public :: project_buffer_conditions_type
        type(project_buffer_condition_type), allocatable :: conditions(:)
    contains
        !> Add a condition last.
        procedure :: append => conditions_append
        !> Remove all the conditions.
        procedure :: clear => conditions_clear
    end type project_buffer_conditions_type

    !> An element of `project_switch_commands': the COMMAND to run when
    !> KEY is pressed (" " to look the key up in `project_prefix_map'),
    !> with LABEL in the dispatch menu.
    type, public :: project_switch_command_type
        type(command_type), pointer :: command => null()
        character(len=32) :: label = ""
        character :: key = " "
    end type project_switch_command_type

    !> When `project_prune_zombie_projects' forgets the projects that no
    !> longer exist, each a test of the project root (null for never):
    !> LIST_FIRST_READ on the first reading of the list, LIST_WRITE after
    !> saving it to `project_list_file', PROMPT before every prompting,
    !> INTERACTIVELY only when `forget_zombie_projects' is called
    !> interactively.
    type, public :: project_prune_zombie_projects_type
        procedure(project_root_test_procedure), pointer, nopass :: list_first_read => null()
        procedure(project_root_test_procedure), pointer, nopass :: list_write => null()
        procedure(project_root_test_procedure), pointer, nopass :: prompt => null()
        procedure(project_root_test_procedure), pointer, nopass :: interactively => null()
    end type project_prune_zombie_projects_type

    !> The values of the cache timeouts that mean no time-based expiration.
    real, parameter, public :: PROJECT_VC_CACHE_TIMEOUT_UNLIMITED = -1.0

    ! ------------------------------------------------------------------
    ! Settings
    ! ------------------------------------------------------------------

    !> Special hook to find the project containing a given directory.
    !> Each procedure on this hook is called in turn with one argument,
    !> the directory in which to look, and should give either no project
    !> to mean that it is not applicable, or a project.  A project is an
    !> object of a type extending `project_type'.  Port of project.el
    !> project-find-functions.
    type(project_find_hook_type), public :: project_find_functions

    !> Value to use instead of `default_directory' when detecting the
    !> project.  When it is set, `get_current_project' will always skip
    !> prompting too.  Port of project.el
    !> project-current-directory-override.
    type(string_setting_type), target, public :: project_current_directory_override = &
                                                 string_setting_type(name="project_current_directory_override")

    !> Function to call to prompt for a project.
    !> The function is called with up to three optional arguments: PROMPT,
    !> TEST and REQUIRE_KNOWN.
    !>
    !> PROMPT is the prompt string to use.
    !>
    !> TEST, if present, is a logical function of a project root directory,
    !> that should give .false. for a project the user should not be
    !> allowed to select.  The `project_prompter' should filter project
    !> completion candidates presented to the user using this test.
    !>
    !> If REQUIRE_KNOWN is .true., the value of `project_prompter' should
    !> only allow the user to select from known projects.  Otherwise, the
    !> function may allow the user to input arbitrary directories.  If TEST
    !> and REQUIRE_KNOWN are both given, the value of `project_prompter'
    !> should not return any project root directory TEST rejects.
    !>
    !> The function must always return a valid project.
    !>
    !> If there is a current project, it satisfies TEST (or TEST is absent),
    !> and the method of prompting involves a default selection, then this
    !> default selection should be the current project root.  For example
    !> if the function uses `completing_read' then the current project, if
    !> any, should be passed as the DEFAULT argument to `completing_read',
    !> and returned in the case that the user replies with empty input.
    !> Port of project.el project-prompter.
    procedure(project_prompter_procedure), pointer, public :: project_prompter => null()

    !> List of patterns to add to the ignores of a VC-aware project.
    !> Port of project.el project-vc-ignores (only the global value:
    !> directory-local values are Deferred).
    character(len=:), allocatable, public :: project_vc_ignores(:)

    !> True to consider submodules part of the parent project.  Port of
    !> project.el project-vc-merge-submodules.
    logical, public :: project_vc_merge_submodules = .true.

    !> When true, the VC-aware project backend includes untracked files.
    !> Port of project.el project-vc-include-untracked.
    logical, public :: project_vc_include_untracked = .true.

    !> When not "", the name of the current VC-aware project.  Port of
    !> project.el project-vc-name.
    character(len=:), allocatable, public :: project_vc_name

    !> List of additional markers to signal project roots.
    !>
    !> A marker is either a base file name or a glob pattern for such.
    !>
    !> A directory containing such a marker file or a file matching a
    !> marker pattern will be recognized as the root of a VC-aware
    !> project.
    !>
    !> Example values: ".dir-locals.el", "package.json", "pom.xml",
    !> "requirements.txt", "Gemfile", "*.gemspec", "autogen.sh".
    !>
    !> These will be used in addition to regular directory markers such as
    !> ".git", ".hg", and so on, depending on the value of
    !> `vc_handled_backends'.  It is most useful when a project has
    !> subdirectories inside it that need to be considered as separate
    !> projects.  It can also be used for projects outside of VC
    !> repositories.
    !>
    !> In either case, their behavior will still obey the relevant
    !> variables, such as `project_vc_ignores' or `project_vc_name'.
    !> Port of project.el project-vc-extra-root-markers.
    character(len=:), allocatable, public :: project_vc_extra_root_markers(:)

    !> Function that returns a list of external roots.
    !>
    !> It should return a list of directory roots that contain source files
    !> related to the current buffer.
    !>
    !> The directory names should be absolute.  Used in the VC-aware
    !> project backend implementation of the external roots (null: none).
    !> Port of project.el project-vc-external-roots-function.
    procedure(project_external_roots_procedure), pointer, public :: project_vc_external_roots_function => null()

    !> Records assigning root markers to VC backends.
    !> See `project_vc_extra_root_markers' for the marker value format.
    !> Port of project.el project-vc-backend-markers-alist.
    type(project_vc_backend_marker_type), allocatable, public :: project_vc_backend_markers(:)

    !> Number of seconds to cache a value in VC-aware project methods
    !> (PROJECT_VC_CACHE_TIMEOUT_UNLIMITED disables time-based expiration).
    !> Port of project.el project-vc-cache-timeout (the timeouts by a test
    !> of the directory are Deferred).
    real, public :: project_vc_cache_timeout = 2.0

    !> Number of seconds to cache non-essential information.
    !> The format of the value is same as `project_vc_cache_timeout', but
    !> while the former is intended for interactive commands, this variable
    !> uses higher numbers, intended for "background" things.  It is used
    !> when `get_current_project' does not prompt.  Port of project.el
    !> project-vc-non-essential-cache-timeout.
    real, public :: project_vc_non_essential_cache_timeout = 300.0

    !> If true, `list_files' is allowed to return relative file names.
    !> The file names should be relative to the project root.  And this
    !> can only happen when all returned files are in the same directory.
    !> In other words, the DIRS argument of `list_files' has to be absent
    !> or a list of only one element.  Port of project.el
    !> project-files-relative-names.
    logical, public :: project_files_relative_names = .false.

    !> Function to call to read a file name from a list:
    !> `read_project_file_relative' or `read_project_file_absolute'.
    !> Port of project.el project-read-file-name-function.
    procedure(project_read_file_name_procedure), pointer, public :: project_read_file_name_function => null()

    !> List of conditions to filter the buffers to be switched to.
    !> If any of these conditions are satisfied for a buffer in the current
    !> project, `switch_to_project_buffer' and `display_project_buffer'
    !> ignore it.  See `project_close_buffer_conditions' for the general
    !> form of conditions.  Port of project.el
    !> project-ignore-buffer-conditions.
    type(project_buffer_conditions_type), public :: project_ignore_buffer_conditions

    !> When to remove from the project list the projects that were
    !> removed: each component is a test of a project root that should
    !> give .true. if the project must be removed, LIST_FIRST_READ applied
    !> on the first reading of the list, LIST_WRITE after saving the list
    !> to `project_list_file', PROMPT before every prompting, and
    !> INTERACTIVELY only when `forget_zombie_projects' is called
    !> interactively.  Port of project.el project-prune-zombie-projects.
    type(project_prune_zombie_projects_type), public :: project_prune_zombie_projects

    !> Function to use in `list_project_buffers' to render the list.
    !> It should accept two arguments: PROJECT and FILES_ONLY.  The latter
    !> means that only file-visiting buffers should be displayed.  Port of
    !> project.el project-buffers-viewer.
    procedure(project_buffers_viewer_procedure), pointer, public :: project_buffers_viewer => null()

    !> List of conditions to close buffers related to a project.
    !> This list is used by `close_project_buffers'.
    !> Each condition is either:
    !> - a regular expression, to match a buffer name
    !>   (`project_buffer_name_regexp'),
    !> - a logical function that takes a buffer as argument and returns
    !>   .true. if the buffer should be closed (`project_buffer_test'),
    !> - `project_major_mode': the buffer is closed if the buffer's major
    !>   mode is the mode named,
    !> - `project_derived_mode': the buffer is closed if the buffer's
    !>   major mode is derived from the mode named,
    !> - `project_not': the negation of a condition,
    !> - `project_and': conditions that all have to be met,
    !> - `project_or': conditions of which at least one has to be met.
    !>
    !> If any of these conditions are satisfied for a buffer in the current
    !> project, it will be closed.  Port of project.el
    !> project-kill-buffer-conditions.
    type(project_buffer_conditions_type), public :: project_close_buffer_conditions

    !> File in which to save the list of known projects.  Port of
    !> project.el project-list-file.
    character(len=:), allocatable, public :: project_list_file

    !> Exclude projects from being remembered by `remember_project'.
    !> It should be a list of regexps and tests for project roots and
    !> objects to always exclude from being remembered.  The test should
    !> take one argument, the project, and should return .true. if the
    !> project should not be remembered.  Port of project.el
    !> project-list-exclude.
    type(project_list_exclude_type), allocatable, public :: project_list_exclude(:)

    !> The commands available upon "switching" to another project, as a
    !> dispatch menu for `switch_project'.  Each element gives the COMMAND
    !> to run when KEY is pressed and the LABEL that distinguishes the menu
    !> entries.  If KEY is " ", COMMAND must be bound in
    !> `project_prefix_map', and the key is looked up in that map.  Port of
    !> project.el project-switch-commands.
    type(project_switch_command_type), allocatable, public :: project_switch_commands(:)

    !> Whether `switch_project' will use the entire `project_prefix_map'.
    !> If false, `switch_project' will only recognize commands listed in
    !> `project_switch_commands', and an error condition occurs when other
    !> commands are invoked.  If this is true, all the keys in
    !> `project_prefix_map' are valid even if they aren't listed in the
    !> dispatch menu produced from `project_switch_commands'.  Port of
    !> project.el project-switch-use-entire-map.
    logical, public :: project_switch_use_entire_map = .false.

    !> Keymap for project commands.  Port of project.el project-prefix-map.
    type(handle_type), public :: project_prefix_map

    !> The history of `read_project_dir' (upstream's project--dir-history).
    type(history_setting_type), target, public :: project_dir_history = history_setting_type(name="project_dir_history")

    !> The history of `read_project_name' (upstream's
    !> project--name-history).
    type(history_setting_type), target, public :: project_name_history = &
                                                  history_setting_type(name="project_name_history")

    ! ------------------------------------------------------------------
    ! Internal state
    ! ------------------------------------------------------------------

    ! project--list: the known project roots, read from
    ! `project_list_file' when first needed (LIST_READ).
    type(string_type), allocatable :: project_list(:)
    logical :: list_read = .false.

    ! non-essential: true while `get_current_project' does not prompt.
    logical :: non_essential = .false.

    ! A VC-aware search cached for a directory (project.el's `project-vc'
    ! file property): whether a project was FOUND, the PROJECT, and the
    ! clock COUNT when it was cached.
    type :: vc_cache_entry_type
        character(len=:), allocatable :: dir
        logical :: found = .false.
        type(vc_project_type) :: project
        integer(int64) :: count = 0
    end type vc_cache_entry_type

    type(vc_cache_entry_type), allocatable :: vc_cache(:)

    ! The markers' regexps and the matches of the last directory looked
    ! at by `try_vc_search'.
    type(string_type), allocatable :: marker_regexps(:)
    type(string_type), allocatable :: last_matches(:)

    ! The project of each Buffer Menu listing project buffers (the
    ! closure of upstream's buffer-list function): by buffer serial.
    type :: buffer_menu_project_type
        integer(int64) :: serial = 0
        class(project_type), allocatable :: project
    end type buffer_menu_project_type

    type(buffer_menu_project_type), allocatable :: buffer_menu_projects(:)

    ! The root `save_some_project_buffers' saves the buffers of.
    character(len=:), allocatable :: save_root

    ! The defaults `completing_read_strict' gives M-n.
    type(string_type), allocatable :: pending_defaults(:)

    ! The "Select a directory" choice of the prompters.
    character(len=*), parameter :: DIR_CHOICE = "... (choose a dir)"

contains

    ! ------------------------------------------------------------------
    ! Strings
    ! ------------------------------------------------------------------

    !> Append the elements of MORE to LIST.  Not a port.
    subroutine append_strings(list, more)
        type(string_type), allocatable, intent(inout) :: list(:)
        type(string_type), intent(in) :: more(:)
        integer :: k

        if (.not. allocated(list)) allocate (list(0))

        do k = 1, size(more)
            call append_string(list, more(k)%text)
        end do
    end subroutine append_strings

    !> The strings of LIST as a list value (for completion tables).
    !> Not a port.
    function string_list(list) result(v)
        type(string_type), intent(in) :: list(:)
        type(value_type) :: v
        type(value_type), allocatable :: elems(:)
        integer :: k

        allocate (elems(size(list)))

        do k = 1, size(list)
            elems(k) = value_type(text=list(k)%text)
        end do

        v = make_list(elems)
    end function string_list

    !> Whether LIST holds TEXT.  Not a port.
    logical function has_string(list, text)
        type(string_type), intent(in) :: list(:)
        character(len=*), intent(in) :: text
        integer :: k

        has_string = .false.

        do k = 1, size(list)
            if (list(k)%text == text .and. len(list(k)%text) == len(text)) then
                has_string = .true.
                return
            end if
        end do
    end function has_string

    !> Whether A sorts before B, comparing bytes with a prefix first, as
    !> fns.c:Fstring_lessp does.  Not a port.
    pure logical function is_string_less(a, b)
        character(len=*), intent(in) :: a, b
        integer :: k

        do k = 1, min(len(a), len(b))
            if (a(k:k) /= b(k:k)) then
                is_string_less = ichar(a(k:k)) < ichar(b(k:k))
                return
            end if
        end do

        is_string_less = len(a) < len(b)
    end function is_string_less

    !> Sort LIST with `is_string_less' (a stable merge sort).  Not a port.
    subroutine sort_strings(list)
        type(string_type), intent(inout) :: list(:)
        integer, allocatable :: order(:), scratch(:)
        type(string_type), allocatable :: sorted(:)
        integer :: n, width, lo, mid, hi, i, j, k

        n = size(list)
        if (n < 2) return
        order = [(k, k=1, n)]
        allocate (scratch(n))
        width = 1

        do while (width < n)
            lo = 1

            do while (lo <= n)
                mid = min(lo + width - 1, n)
                hi = min(lo + 2*width - 1, n)
                i = lo
                j = mid + 1
                k = lo

                do while (i <= mid .and. j <= hi)
                    if (is_string_less(list(order(j))%text, list(order(i))%text)) then
                        scratch(k) = order(j)
                        j = j + 1
                    else
                        scratch(k) = order(i)
                        i = i + 1
                    end if

                    k = k + 1
                end do

                do while (i <= mid)
                    scratch(k) = order(i)
                    i = i + 1
                    k = k + 1
                end do

                do while (j <= hi)
                    scratch(k) = order(j)
                    j = j + 1
                    k = k + 1
                end do

                lo = hi + 1
            end do

            order = scratch
            width = 2*width
        end do

        sorted = list(order)
        list = sorted
    end subroutine sort_strings

    !> The strings of the character array ARRAY, without trailing blanks
    !> (none when unallocated).  Not a port.
    function trimmed_strings(array) result(list)
        character(len=:), allocatable, intent(in) :: array(:)
        type(string_type), allocatable :: list(:)
        integer :: k

        allocate (list(0))
        if (.not. allocated(array)) return

        do k = 1, size(array)
            call append_string(list, trim(array(k)))
        end do
    end function trimmed_strings

    !> The current buffer's `default_directory' ("./" for none).
    !> Not a port.
    function default_directory() result(dir)
        character(len=:), allocatable :: dir

        dir = "./"
        if (current == 0) return
        if (allocated(buffers(current)%directory)) dir = buffers(current)%directory
    end function default_directory

    !> Make DIR the current buffer's `default_directory'.  Not a port.
    subroutine set_default_directory(dir)
        character(len=*), intent(in) :: dir

        if (current == 0) return
        buffers(current)%directory = dir
    end subroutine set_default_directory

    !> FILE expanded (FILE itself when it can't be).  Not a port.
    function expanded(file, dir) result(name)
        character(len=*), intent(in) :: file
        character(len=*), intent(in), optional :: dir
        character(len=:), allocatable :: name
        type(status_type) :: st

        call expand_file_name(file, name, st, dir)
        if (.not. st%ok()) name = file
    end function expanded

    !> Whether STRING starts with PREFIX.  Not a port.
    pure logical function starts_with(string, prefix)
        character(len=*), intent(in) :: string, prefix

        starts_with = .false.
        if (len(prefix) <= len(string)) starts_with = string(1:len(prefix)) == prefix
    end function starts_with

    !> Whether STRING ends with SUFFIX.  Not a port.
    pure logical function ends_with(string, suffix)
        character(len=*), intent(in) :: string, suffix

        ends_with = .false.
        if (len(suffix) <= len(string)) ends_with = string(len(string) - len(suffix) + 1:) == suffix
    end function ends_with

    !> Make a fresh temporary buffer current, with the `default_directory'
    !> DIR (with-temp-buffer); OLD is the buffer to go back to.  Not a port.
    subroutine temp_begin(old, tmp, dir)
        type(handle_type), intent(out) :: old, tmp
        character(len=*), intent(in) :: dir
        type(status_type) :: st

        old = current_buffer()
        call get_or_make_buffer(generate_new_buffer_name(" *temp*"), tmp, st)
        call set_buffer(tmp, st)
        call set_default_directory(dir)
    end subroutine temp_begin

    !> Close the temporary buffer TMP and make OLD current again.
    !> Not a port.
    subroutine temp_end(old, tmp)
        type(handle_type), intent(in) :: old, tmp
        type(status_type) :: st

        call close_buffer(tmp, st)
        call set_buffer(old, st)
    end subroutine temp_end

    !> The text of FILE (FOUND false if it can't be read).  Not a port.
    subroutine file_text(file, text, found)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: text
        logical, intent(out) :: found
        type(handle_type) :: old, tmp
        type(status_type) :: st
        integer(int64) :: inserted

        text = ""
        call temp_begin(old, tmp, default_directory())
        call insert_file_contents(file, .false., inserted, st)
        found = st%ok()
        if (found) text = buffer_string()
        call temp_end(old, tmp)
    end subroutine file_text

    ! ------------------------------------------------------------------
    ! The hook of project finders
    ! ------------------------------------------------------------------

    ! Not a port.
    subroutine project_find_hook_prepend(self, fn)
        class(project_find_hook_type), intent(inout) :: self
        procedure(project_find_procedure) :: fn
        type(project_find_entry_type) :: entry

        call self%remove(fn)
        entry%fn => fn
        self%entries = [entry, self%entries]
    end subroutine project_find_hook_prepend

    ! Not a port.
    subroutine project_find_hook_append(self, fn)
        class(project_find_hook_type), intent(inout) :: self
        procedure(project_find_procedure) :: fn
        type(project_find_entry_type) :: entry

        call self%remove(fn)
        entry%fn => fn
        self%entries = [self%entries, entry]
    end subroutine project_find_hook_append

    ! Not a port.
    subroutine project_find_hook_remove(self, fn)
        class(project_find_hook_type), intent(inout) :: self
        procedure(project_find_procedure) :: fn
        type(project_find_entry_type), allocatable :: kept(:)
        integer :: k

        if (.not. allocated(self%entries)) allocate (self%entries(0))
        allocate (kept(0))

        do k = 1, size(self%entries)
            if (.not. associated(self%entries(k)%fn, fn)) kept = [kept, self%entries(k)]
        end do

        call move_alloc(kept, self%entries)
    end subroutine project_find_hook_remove

    ! Not a port.
    subroutine project_find_hook_clear(self)
        class(project_find_hook_type), intent(inout) :: self

        if (allocated(self%entries)) deallocate (self%entries)
        allocate (self%entries(0))
    end subroutine project_find_hook_clear

    ! Not a port.
    recursive subroutine project_find_hook_execute(self, dir, project, status)
        class(project_find_hook_type), intent(in) :: self
        character(len=*), intent(in) :: dir
        class(project_type), allocatable, intent(out) :: project
        type(status_type), intent(out) :: status
        type(project_find_entry_type), allocatable :: entries(:)
        integer :: k

        status = status_type()
        if (.not. allocated(self%entries)) return
        ! A copy: a procedure may change the hook.
        entries = self%entries

        do k = 1, size(entries)
            call entries(k)%fn(dir, project, status)
            if (.not. status%ok()) return
            if (allocated(project)) return
        end do
    end subroutine project_find_hook_execute

    ! ------------------------------------------------------------------
    ! The current project
    ! ------------------------------------------------------------------

    !> Return the project in DIRECTORY, defaulting to `default_directory'.
    !>
    !> When no project is found in that directory, the result depends on
    !> MAYBE_PROMPT: if it is .false. or absent, PROJECT is unallocated,
    !> else prompt the user for the project to use.  To prompt for a
    !> project, call the function specified by `project_prompter', which
    !> returns the directory in which to look for the project.  If no
    !> project is found in that directory, return a "transient" project.
    !> PROMPT, when present, is passed to the prompter function.
    !>
    !> The "transient" project is a special kind of value which denotes a
    !> project rooted in that directory and includes all the files under
    !> the directory except for those that match entries in
    !> `vc_directory_exclusions' or `grep_find_ignored_files_variable'.
    !>
    !> See `project_find_functions' for the general form of the project.
    !> Port of project.el:project-current.
    recursive subroutine get_current_project(project, status, maybe_prompt, prompt, directory)
        class(project_type), allocatable, intent(out) :: project
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: maybe_prompt
        character(len=*), intent(in), optional :: prompt, directory
        character(len=:), allocatable :: dir, override
        logical :: prompting, saved_non_essential

        prompting = .false.
        if (present(maybe_prompt)) prompting = maybe_prompt
        override = ""
        if (allocated(project_current_directory_override%value)) override = project_current_directory_override%value

        if (present(directory)) then
            dir = directory
        else if (len(override) > 0) then
            dir = override
        else
            dir = default_directory()
        end if

        saved_non_essential = non_essential
        non_essential = .not. prompting
        call find_project_in_directory(dir, project, status)
        non_essential = saved_non_essential
        if (.not. status%ok()) return

        if (.not. allocated(project) .and. len(override) == 0 .and. prompting) then
            call execute_prompter(dir, status, prompt)
            if (.not. status%ok()) return
            saved_non_essential = non_essential
            non_essential = .false.
            call find_project_in_directory(dir, project, status)
            non_essential = saved_non_essential
            if (.not. status%ok()) return
        end if

        if (prompting) then
            if (allocated(project)) then
                call remember_project(project, status)
            else
                call remove_from_project_list(abbreviate_file_name(dir), "Project `", "' not found; removed from list", &
                                              status)
                allocate (project, source=transient_project_type(directory=dir))
            end if
        end if
    end subroutine get_current_project

    !> Call `project_prompter' (`read_project_dir' when null) for DIR, with
    !> PROMPT if present.  Not a port.
    recursive subroutine execute_prompter(dir, status, prompt, test, require_known)
        character(len=:), allocatable, intent(out) :: dir
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: prompt
        procedure(project_root_test_procedure), optional :: test
        logical, intent(in), optional :: require_known

        if (associated(project_prompter)) then
            call project_prompter(dir, status, prompt, test, require_known)
        else
            call read_project_dir(dir, status, prompt, test, require_known)
        end if
    end subroutine execute_prompter

    !> The project in DIR from `project_find_functions'; none when a
    !> `permission-denied' error condition occurs.  Port of
    !> project.el:project--find-in-directory.
    recursive subroutine find_project_in_directory(dir, project, status)
        character(len=*), intent(in) :: dir
        class(project_type), allocatable, intent(out) :: project
        type(status_type), intent(out) :: status

        call project_find_functions%execute(dir, project, status)

        ! Maybe we'd like to continue to the next backend instead?  Let's
        ! see if somebody ever ends up in that situation.
        if (.not. status%ok()) then
            if (status%code == FM_PERMISSION_DENIED) then
                status = status_type()
                if (allocated(project)) deallocate (project)
            end if
        end if
    end subroutine find_project_in_directory

    ! ------------------------------------------------------------------
    ! The generic project procedures
    ! ------------------------------------------------------------------

    !> The external roots of PROJECT: none by default.  Port of
    !> project.el:project-external-roots.
    function project_external_roots(self) result(roots)
        class(project_type), intent(in) :: self
        type(string_type), allocatable :: roots(:)

        allocate (roots(0))
    end function project_external_roots

    !> The base name of the root directory.  Port of
    !> project.el:project-name.
    function project_name(self) result(name)
        class(project_type), intent(in) :: self
        character(len=:), allocatable :: name

        name = file_name_nondirectory(directory_file_name(self%root()))
    end function project_name

    !> The directories of `vc_directory_exclusions' and the files of
    !> `grep_find_ignored_files' for DIR.  Port of
    !> project.el:project-ignores.
    function project_ignores(self, dir) result(ignores)
        class(project_type), intent(in) :: self
        character(len=*), intent(in) :: dir
        type(string_type), allocatable :: ignores(:)

        call default_ignores(dir, ignores)
    end function project_ignores

    !> The ignores of a project with no ignores of its own, as upstream
    !> gives them for no project.  Port of project.el:project-ignores.
    subroutine default_ignores(dir, ignores)
        character(len=*), intent(in) :: dir
        type(string_type), allocatable, intent(out) :: ignores(:)
        character(len=64), allocatable :: files(:)
        integer :: k

        allocate (ignores(0))

        if (allocated(vc_directory_exclusions)) then
            do k = 1, size(vc_directory_exclusions)
                call append_string(ignores, trim(vc_directory_exclusions(k))//"/")
            end do
        end if

        files = grep_find_ignored_files(dir)

        do k = 1, size(files)
            call append_string(ignores, trim(files(k)))
        end do
    end subroutine default_ignores

    !> The files of PROJECT in the directories DIRS (its root when absent),
    !> listed by `find' with the ignores of each directory.  Port of
    !> project.el:project-files.
    recursive subroutine project_list_files(self, files, status, dirs)
        class(project_type), intent(in) :: self
        type(string_type), allocatable, intent(out) :: files(:)
        type(status_type), intent(out) :: status
        type(string_type), intent(in), optional :: dirs(:)
        type(string_type), allocatable :: roots(:), more(:), ignores(:)
        integer :: k

        allocate (files(0))
        status = status_type()

        if (present(dirs)) then
            roots = dirs
        else
            allocate (roots(1))
            roots(1)%text = self%root()
        end if

        do k = 1, size(roots)
            call project_dir_ignores(self, roots(k)%text, ignores)
            call project_files_in_directory(roots(k)%text, ignores, more, status)
            if (.not. status%ok()) return
            call append_strings(files, more)
        end do
    end subroutine project_list_files

    !> The ignores of PROJECT that apply to DIR: the default ones outside
    !> of the root, the project's own (unrooted ones only in a
    !> subdirectory).  Port of project.el:project--dir-ignores.
    subroutine project_dir_ignores(project, dir, ignores)
        class(project_type), intent(in) :: project
        character(len=*), intent(in) :: dir
        type(string_type), allocatable, intent(out) :: ignores(:)
        type(string_type), allocatable :: all(:)
        character(len=:), allocatable :: root
        integer :: k

        root = project%root()

        if (.not. is_file_in_directory(dir, root)) then
            ! The defaults.
            call default_ignores("", ignores)
            return
        end if

        all = project%ignores(root)

        if (is_file_equal(root, dir)) then
            call move_alloc(all, ignores)
            return
        end if

        allocate (ignores(0))

        ! FIXME: Update the "rooted" ignores to relate to DIR instead.
        do k = 1, size(all)
            if (.not. starts_with(all(k)%text, "./")) call append_string(ignores, all(k)%text)
        end do
    end subroutine project_dir_ignores

    !> The live buffers whose `default_directory' is under the root of
    !> PROJECT.  Port of project.el:project-buffers.
    function project_buffers(self) result(bufs)
        class(project_type), intent(in) :: self
        type(handle_type), allocatable :: bufs(:)
        type(handle_type), allocatable :: all(:)
        character(len=:), allocatable :: root
        integer :: k

        root = expanded(file_name_as_directory(self%root()))
        all = buffer_list()
        allocate (bufs(0))

        do k = 1, size(all)
            if (.not. allocated(buffers(all(k)%index)%directory)) cycle
            if (starts_with(expanded(buffers(all(k)%index)%directory), root)) bufs = [bufs, all(k)]
        end do
    end function project_buffers

    !> The files under DIR (sorted, absolute unless
    !> `project_files_relative_names'), except those IGNORES match, and
    !> only those matching the space-separated patterns FILE_PATTERNS when
    !> given; listed with `find_program'.  Port of
    !> project.el:project--files-in-directory.
    recursive subroutine project_files_in_directory(dir, ignores, files, status, file_patterns)
        character(len=*), intent(in) :: dir
        type(string_type), intent(in) :: ignores(:)
        type(string_type), allocatable, intent(out) :: files(:)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: file_patterns
        character(len=:), allocatable :: directory, localdir, command, names, text, program
        type(value_type) :: result
        type(handle_type) :: old, tmp
        integer :: code, k, pt, nul, perm, line_start

        allocate (files(0))
        directory = file_name_as_directory(dir)
        ! Make sure ~/ etc. in local directory name is expanded and not left
        ! for the shell command to interpret.
        localdir = expanded(directory)
        names = ""

        if (present(file_patterns)) then
            names = shell_quote_argument("(")//" -name "//join_quoted(file_patterns)//" "//shell_quote_argument(")")
        end if

        program = "find"
        if (allocated(find_program)) program = find_program
        command = program//" -H . "//find_ignores_arguments(ignores, "./")//" -type f "//names//" -print0"
        call temp_begin(old, tmp, directory)
        call execute_process_shell_command(command, result, status)
        text = buffer_string()
        call temp_end(old, tmp)
        if (.not. status%ok()) return
        code = exit_code(result)

        if (code /= 0) then
            perm = index(text, "Permission denied"//achar(10))

            if (code /= 127 .and. perm > 0) then
                ! (As upstream: after the NUL before the line, or after
                ! the first character when there is none.)
                line_start = index(text(1:perm), achar(0), back=.true.) + 1
                if (line_start == 1) line_start = 2
                call set_error(status, FM_GENERIC_ERROR, "File listing failed: "//text(line_start:perm + 16))
            else
                call set_error(status, FM_GENERIC_ERROR, "File listing failed: "//text)
            end if

            return
        end if

        pt = 1

        do
            nul = index(text(pt:), achar(0))
            if (nul == 0) exit
            nul = pt + nul - 1

            if (nul - 1 >= pt + 2) then
                call append_string(files, text(pt + 2:nul - 1))
            else
                call append_string(files, "")
            end if

            pt = nul + 1
        end do

        call sort_strings(files)

        if (.not. project_files_relative_names) then
            do k = 1, size(files)
                files(k)%text = localdir//files(k)%text
            end do
        end if

    contains

        ! The patterns of PATTERNS (separated by whitespace), quoted and
        ! joined with " -o -name ".
        function join_quoted(patterns) result(s)
            character(len=*), intent(in) :: patterns
            character(len=:), allocatable :: s
            integer :: i, j

            s = ""
            i = 1

            do while (i <= len(patterns))
                if (verify(patterns(i:i), " "//achar(9)//achar(10)) == 0) then
                    i = i + 1
                    cycle
                end if

                j = scan(patterns(i:), " "//achar(9)//achar(10))

                if (j == 0) then
                    j = len(patterns) + 1
                else
                    j = i + j - 1
                end if

                if (len(s) > 0) s = s//" -o -name "
                s = s//shell_quote_argument(patterns(i:j - 1))
                i = j
            end do
        end function join_quoted
    end subroutine project_files_in_directory

    !> The exit status in RESULT (a process's result: an integer, -1 if
    !> it was cut by a signal).  Not a port.
    integer function exit_code(result)
        use value_mod, only: xfixnum, V_INTEGER
        type(value_type), intent(in) :: result

        exit_code = -1
        if (vtag(result) == V_INTEGER) exit_code = int(xfixnum(result))
    end function exit_code

    !> Convert IGNORES and DIR to a list of arguments for `find'.
    !> IGNORES is a list of glob patterns.  DIR is an absolute directory,
    !> used as the root of the ignore globs.  Port of
    !> xref.el:xref--find-ignores-arguments.
    function find_ignores_arguments(ignores, dir) result(args)
        type(string_type), intent(in) :: ignores(:)
        character(len=*), intent(in) :: dir
        character(len=:), allocatable :: args
        character(len=:), allocatable :: d, ignore
        integer :: k

        args = ""
        if (size(ignores) == 0) return
        d = file_name_as_directory(dir)
        args = shell_quote_argument("(")//" -path "

        do k = 1, size(ignores)
            ignore = ignores(k)%text
            if (ends_with(ignore, "/")) ignore = ignore//"*"

            if (starts_with(ignore, "./")) then
                ignore = d//ignore(3:)
            else if (.not. starts_with(ignore, "*")) then
                ignore = "*/"//ignore
            end if

            if (k > 1) args = args//" -o -path "
            args = args//shell_quote_argument(ignore)
        end do

        args = args//" "//shell_quote_argument(")")//" -prune -o "
    end function find_ignores_arguments

    ! ------------------------------------------------------------------
    ! The transient project
    ! ------------------------------------------------------------------

    !> The chosen directory.  Port of project.el:project-root (the method
    !> for a transient project).
    function transient_project_root(self) result(root)
        class(transient_project_type), intent(in) :: self
        character(len=:), allocatable :: root

        root = ""
        if (allocated(self%directory)) root = self%directory
    end function transient_project_root

    ! ------------------------------------------------------------------
    ! The VC-aware project
    ! ------------------------------------------------------------------

    !> Forget the VC-aware project of every directory.  Port of
    !> project.el:project--clear-cache.
    subroutine project_clear_cache()
        if (allocated(vc_cache)) deallocate (vc_cache)
        allocate (vc_cache(0))
    end subroutine project_clear_cache

    !> The index in `vc_cache' of DIR's entry if it is younger than
    !> TIMEOUT seconds (0 for none).  Port of project.el:project--get-cached.
    integer function cached_index(dir, timeout) result(k)
        character(len=*), intent(in) :: dir
        real, intent(in) :: timeout
        integer(int64) :: now, rate
        integer :: i

        k = 0
        if (.not. allocated(vc_cache)) return

        do i = 1, size(vc_cache)
            if (vc_cache(i)%dir == dir .and. len(vc_cache(i)%dir) == len(dir)) then
                k = i
                exit
            end if
        end do

        if (k == 0) return
        if (timeout < 0) return
        call system_clock(now, rate)
        if (real(now - vc_cache(k)%count, real64)/real(rate, real64) >= timeout) k = 0
    end function cached_index

    !> Cache PROJECT (none unless FOUND) for DIR.  Port of
    !> project.el:project--set-cached.
    subroutine set_cached(dir, found, project)
        character(len=*), intent(in) :: dir
        logical, intent(in) :: found
        type(vc_project_type), intent(in) :: project
        type(vc_cache_entry_type) :: entry
        integer(int64) :: rate
        integer :: i

        if (.not. allocated(vc_cache)) allocate (vc_cache(0))
        entry%dir = dir
        entry%found = found
        entry%project = project
        call system_clock(entry%count, rate)

        do i = 1, size(vc_cache)
            if (vc_cache(i)%dir == dir .and. len(vc_cache(i)%dir) == len(dir)) then
                vc_cache(i) = entry
                return
            end if
        end do

        vc_cache = [vc_cache, entry]
    end subroutine set_cached

    !> The project of DIR from the VC-aware backend.
    !>
    !> The value is cached, and depending on whether `get_current_project'
    !> prompts, the timeout is `project_vc_cache_timeout' or
    !> `project_vc_non_essential_cache_timeout'.  Port of
    !> project.el:project-try-vc.
    recursive subroutine try_vc_project(dir, project, status)
        character(len=*), intent(in) :: dir
        class(project_type), allocatable, intent(out) :: project
        type(status_type), intent(out) :: status
        type(vc_project_type) :: res
        real :: timeout
        logical :: found
        integer :: k

        status = status_type()
        timeout = project_vc_cache_timeout
        if (non_essential) timeout = project_vc_non_essential_cache_timeout
        k = cached_index(dir, timeout)

        if (k /= 0) then
            if (vc_cache(k)%found) allocate (project, source=vc_cache(k)%project)
            return
        end if

        call try_vc_search(dir, res, found, status)
        if (.not. status%ok()) return
        call set_cached(dir, found, res)
        if (found) allocate (project, source=res)
    end subroutine try_vc_project

    !> Whether directory D has a file matching one of `marker_regexps',
    !> the matches kept in `last_matches'.  Not a port.
    logical function has_marker(d)
        character(len=*), intent(in) :: d
        type(string_type), allocatable :: names(:)
        type(status_type) :: st
        integer :: i, j

        has_marker = .false.
        if (allocated(last_matches)) deallocate (last_matches)
        allocate (last_matches(0))
        if (fortmacs_directory_entries(directory_file_name(expanded(d)), names) /= 0) return

        do i = 1, size(names)
            do j = 1, size(marker_regexps)
                if (string_match(marker_regexps(j)%text, names(i)%text, st) > 0) then
                    call append_string(last_matches, names(i)%text)
                    exit
                end if
            end do
        end do

        has_marker = size(last_matches) > 0
    end function has_marker

    !> Search the VC-aware project of DIR: the nearest directory with a
    !> marker of `project_vc_backend_markers' (for the backends of
    !> `vc_handled_backends') or `project_vc_extra_root_markers'; the root
    !> of the superproject of a Git submodule when
    !> `project_vc_merge_submodules'.  Port of
    !> project.el:project-try-vc--search.
    recursive subroutine try_vc_search(dir, project, found, status, extra_markers)
        character(len=*), intent(in) :: dir
        type(vc_project_type), intent(out) :: project
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: extra_markers
        type(string_type), allocatable :: markers(:), saved_regexps(:), saved_matches(:)
        type(vc_project_type) :: parent
        character(len=:), allocatable :: root, backend, stop, up, marker
        logical :: has_root, parent_found, use_extra
        integer :: b, k

        status = status_type()
        found = .false.
        allocate (markers(0))

        if (allocated(vc_handled_backends)) then
            do b = 1, size(vc_handled_backends)
                marker = backend_marker(trim(vc_handled_backends(b)))
                if (len(marker) > 0) call append_string(markers, marker)
            end do
        end if

        use_extra = .true.
        if (present(extra_markers)) use_extra = extra_markers
        if (use_extra) call append_strings(markers, trimmed_strings(project_vc_extra_root_markers))
        if (allocated(marker_regexps)) call move_alloc(marker_regexps, saved_regexps)
        if (allocated(last_matches)) call move_alloc(last_matches, saved_matches)
        allocate (marker_regexps(0))

        do k = 1, size(markers)
            call append_string(marker_regexps, wildcard_to_regexp(markers(k)%text))
        end do

        stop = locate_dominating_stop_dir_regexp
        if (allocated(vc_ignore_dir_regexp)) stop = vc_ignore_dir_regexp
        call locate_dominating_file(dir, "", root, has_root, stop, has_marker)
        backend = ""

        if (has_root .and. allocated(vc_handled_backends)) then
            do b = 1, size(vc_handled_backends)
                marker = backend_marker(trim(vc_handled_backends(b)))

                if (len(marker) > 0) then
                    if (has_string(last_matches, marker)) then
                        backend = trim(vc_handled_backends(b))
                        exit
                    end if
                end if
            end do
        end if

        if (allocated(saved_regexps)) call move_alloc(saved_regexps, marker_regexps)
        if (allocated(saved_matches)) call move_alloc(saved_matches, last_matches)

        do while (has_root .and. backend == "Git")
            if (.not. project_vc_merge_submodules) exit
            if (.not. is_submodule(root)) exit
            up = file_name_directory(directory_file_name(root))
            call vc_git_root(up, root, has_root)
        end do

        if (.not. has_root) return

        if (len(backend) == 0) then
            ! Avoid submodules scan.
            call try_vc_search(root, parent, parent_found, status, extra_markers=.false.)
            if (.not. status%ok()) return
            if (parent_found) backend = parent%backend
        end if

        project%backend = backend
        project%directory = root
        found = .true.
    end subroutine try_vc_search

    !> The root marker of BACKEND in `project_vc_backend_markers' ("" for
    !> none).  Not a port.
    function backend_marker(backend) result(marker)
        character(len=*), intent(in) :: backend
        character(len=:), allocatable :: marker
        integer :: k

        marker = ""
        if (.not. allocated(project_vc_backend_markers)) return
        k = findloc(project_vc_backend_markers%backend, backend, dim=1)
        if (k > 0) marker = trim(project_vc_backend_markers(k)%marker)
    end function backend_marker

    !> Whether ROOT is a Git submodule (its .git a file pointing into the
    !> modules of another repository).  Port of
    !> project.el:project--submodule-p.
    logical function is_submodule(root)
        character(len=*), intent(in) :: root
        character(len=:), allocatable :: gitfile, text
        type(status_type) :: st
        logical :: found

        is_submodule = .false.
        gitfile = expanded(".git", root)
        if (is_file_directory(gitfile)) return
        call file_text(gitfile, text, found)
        if (.not. found) return
        ! Kind of a hack to distinguish a submodule from other cases of .git
        ! files pointing elsewhere.
        is_submodule = string_match("\`gitdir: .+/\.git/\(worktrees/.*\)?modules/", text, st) > 0
    end function is_submodule

    !> The root of a VC-aware project.  Port of project.el:project-root
    !> (the method for a VC-aware project).
    function vc_project_root(self) result(root)
        class(vc_project_type), intent(in) :: self
        character(len=:), allocatable :: root

        root = ""
        if (allocated(self%directory)) root = self%directory
    end function vc_project_root

    !> The directories of `project_vc_external_roots_function' outside of
    !> the root.  Port of project.el:project-external-roots (the method
    !> for a VC-aware project).
    function vc_project_external_roots(self) result(roots)
        class(vc_project_type), intent(in) :: self
        type(string_type), allocatable :: roots(:)
        type(string_type), allocatable :: given(:), root(:)
        integer :: k

        allocate (given(0))

        if (associated(project_vc_external_roots_function)) then
            call project_vc_external_roots_function(given)

            do k = 1, size(given)
                given(k)%text = file_name_as_directory(given(k)%text)
            end do
        end if

        allocate (root(1))
        root(1)%text = self%root()
        roots = subtract_project_directories(combine_project_directories(given), root)
    end function vc_project_external_roots

    !> The VC backend of DIR in PROJECT: the project's own under its root,
    !> else the backend responsible for DIR ("" for none).  Port of
    !> project.el:project-vc--backend.
    function project_vc_backend(project, dir) result(backend)
        class(vc_project_type), intent(in) :: project
        character(len=*), intent(in) :: dir
        character(len=:), allocatable :: backend
        type(status_type) :: st
        integer :: b

        backend = ""

        if (is_file_in_directory(dir, project%root())) then
            if (allocated(project%backend)) backend = project%backend
            return
        end if

        ! Don't presume other DIRs are under the same VCS as the project's
        ! root.
        b = vc_responsible_backend(dir, st, no_error=.true.)
        if (st%ok() .and. b > 0) backend = vc_backends(b)%name
    end function project_vc_backend

    !> The files of a VC-aware project in DIRS (its root when absent),
    !> listed by the VC backend.  Port of project.el:project-files (the
    !> method for a VC-aware project).
    recursive subroutine vc_project_list_files(self, files, status, dirs)
        class(vc_project_type), intent(in) :: self
        type(string_type), allocatable, intent(out) :: files(:)
        type(status_type), intent(out) :: status
        type(string_type), intent(in), optional :: dirs(:)
        type(string_type), allocatable :: roots(:), ignores(:), more(:), defaults(:)
        character(len=:), allocatable :: backend
        integer :: k

        allocate (files(0))
        status = status_type()

        if (present(dirs)) then
            roots = dirs
        else
            allocate (roots(1))
            roots(1)%text = self%root()
        end if

        do k = 1, size(roots)
            ignores = trimmed_strings(project_vc_ignores)
            backend = project_vc_backend(self, roots(k)%text)

            if (backend == "Git") then
                call vc_git_project_list_files(roots(k)%text, ignores, more, status)
            else if (len(backend) > 0) then
                ! vc-default-project-list-files.
                call project_files_in_directory(roots(k)%text, project_vc_ignores_of(roots(k)%text, backend, ignores), &
                                                more, status)
            else
                call default_ignores("", defaults)
                call append_strings(ignores, defaults)
                call project_files_in_directory(roots(k)%text, ignores, more, status)
            end if

            if (.not. status%ok()) return
            call append_strings(files, more)
        end do
    end subroutine vc_project_list_files

    !> The files of the Git repository at DIR, with `git ls-files', except
    !> those EXTRA_IGNORES match (globs as in `project_ignores'): tracked
    !> and, with `project_vc_include_untracked', untracked files; the files
    !> of the submodules too with `project_vc_merge_submodules'.  Port of
    !> project.el:vc-git-project-list-files.
    recursive subroutine vc_git_project_list_files(dir, extra_ignores, files, status)
        character(len=*), intent(in) :: dir
        type(string_type), intent(in) :: extra_ignores(:)
        type(string_type), allocatable, intent(out) :: files(:)
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: args(:)
        type(string_type) :: none(0)
        type(string_type), allocatable :: submodules(:), sub_files(:), kept(:)
        type(handle_type) :: old, tmp
        character(len=:), allocatable :: directory, gitver, text, i, module
        logical :: dedup, saved_literal, new_enough, sparse
        integer :: k, j, pt, nul

        allocate (files(0))
        status = status_type()

        gitver = vc_git_program_version()
        new_enough = is_version_less_or_equal("2.13", gitver, status)
        if (.not. status%ok()) return

        if (size(extra_ignores) > 0 .and. .not. new_enough) then
            ! Need newer Git to use negative pathspec like we do.
            call project_files_in_directory(dir, project_vc_ignores_of(dir, "Git", extra_ignores), files, status)
            return
        end if

        directory = expanded(file_name_as_directory(dir))
        dedup = is_version_less_or_equal("2.31", gitver, status)
        if (.not. status%ok()) return
        sparse = is_version_less_or_equal("2.35", gitver, status)
        if (.not. status%ok()) return
        allocate (args(0))
        call append_arg("ls-files")
        call append_arg("-z")
        call append_arg("-c")
        call append_arg("--exclude-standard")
        if (sparse) call append_arg("--sparse")
        if (project_vc_include_untracked) call append_arg("-o")
        if (dedup) call append_arg("--deduplicate")

        if (size(extra_ignores) > 0) then
            call append_arg("--")

            do k = 1, size(extra_ignores)
                i = extra_ignores(k)%text

                if (index(i, "**") == 0) then
                    if (starts_with(i, "./")) then
                        ! ./abc -> abc
                        i = i(3:)
                    else
                        ! abc -> **/abc
                        i = "**/"//i
                        ! FIXME: '**/abc' should also match a directory with
                        ! that name, but doesn't (git 2.25.1).  Maybe we should
                        ! replace such entries with two.
                        ! abc/ -> abc/**
                        if (ends_with(i, "/")) i = i//"**"
                    end if
                end if

                call append_arg(":(exclude,glob,top)"//i)
            end do
        end if

        call temp_begin(old, tmp, directory)
        call git_submodules(submodules)
        saved_literal = vc_git_use_literal_pathspecs
        vc_git_use_literal_pathspecs = .false.
        call vc_git_command(0, none, args, status, current=.true.)
        vc_git_use_literal_pathspecs = saved_literal
        text = buffer_string()
        call temp_end(old, tmp)
        if (.not. status%ok()) return
        pt = 1

        do while (pt <= len(text))
            nul = index(text(pt:), achar(0))

            if (nul == 0) then
                nul = len(text) + 1
            else
                nul = pt + nul - 1
            end if

            if (nul > pt) then
                associate (file => text(pt:nul - 1))
                    ! Directories occur for sparse directories only, when the
                    ! sparse index is enabled.
                    if (.not. has_string(submodules, file) .and. .not. is_directory_name(file)) then
                        if (project_files_relative_names) then
                            call append_string(files, file)
                        else
                            call append_string(files, directory//file)
                        end if
                    end if
                end associate
            end if

            pt = nul + 1
        end do

        if (project_vc_merge_submodules) then
            ! Unfortunately, 'ls-files --recurse-submodules' conflicts with '-o'.
            do k = 1, size(submodules)
                module = submodules(k)%text
                if (.not. is_file_directory(directory//module)) cycle
                call vc_git_project_list_files(directory//module, extra_ignores, sub_files, status)
                if (.not. status%ok()) return

                if (project_files_relative_names) then
                    do j = 1, size(sub_files)
                        sub_files(j)%text = file_name_as_directory(module)//sub_files(j)%text
                    end do
                end if

                call append_strings(files, sub_files)
            end do
        end if

        ! 'git ls-files' returns duplicate entries for merge conflicts.
        if (.not. dedup .and. size(files) > 1) then
            allocate (kept(0))
            call append_string(kept, files(1)%text)

            do k = 2, size(files)
                if (files(k)%text /= files(k - 1)%text .or. len(files(k)%text) /= len(files(k - 1)%text)) then
                    call append_string(kept, files(k)%text)
                end if
            end do

            call move_alloc(kept, files)
        end if

    contains

        subroutine append_arg(arg)
            character(len=*), intent(in) :: arg
            type(string_type) :: item

            item%text = arg
            args = [args, item]
        end subroutine append_arg
    end subroutine vc_git_project_list_files

    !> The paths of the submodules in the .gitmodules file of
    !> `default_directory'.  Port of project.el:project--git-submodules.
    subroutine git_submodules(paths)
        type(string_type), allocatable, intent(out) :: paths(:)
        character(len=:), allocatable :: text
        type(handle_type) :: old, tmp
        type(status_type) :: st
        integer(int64) :: found
        logical :: ok

        allocate (paths(0))
        ! 'git submodule foreach' is much slower.
        call file_text(".gitmodules", text, ok)
        if (.not. ok) return
        call temp_begin(old, tmp, default_directory())
        call insert(text, st)

        do
            call re_search_forward("^[ \t]*path *= *\(.+\)", found, st, noerror=SEARCH_NOERROR)
            if (.not. st%ok() .or. found == 0) exit
            call append_string(paths, match_string(1))
        end do

        call temp_end(old, tmp)
    end subroutine git_submodules

    !> The ignores of a VC-aware project in DIR.  Port of
    !> project.el:project-ignores (the method for a VC-aware project).
    function vc_project_ignores(self, dir) result(ignores)
        class(vc_project_type), intent(in) :: self
        character(len=*), intent(in) :: dir
        type(string_type), allocatable :: ignores(:)

        ignores = project_vc_ignores_of(dir, project_vc_backend(self, dir), trimmed_strings(project_vc_ignores))
    end function vc_project_ignores

    !> The ignores of DIR under BACKEND: the patterns of the backend's
    !> ignore file (made "rooted" where they contain a slash), then
    !> EXTRA_IGNORES, then the directories of `vc_directory_exclusions'.
    !> Port of project.el:project--vc-ignores.
    function project_vc_ignores_of(dir, backend, extra_ignores) result(ignores)
        character(len=*), intent(in) :: dir, backend
        type(string_type), intent(in) :: extra_ignores(:)
        type(string_type), allocatable :: ignores(:)
        type(string_type), allocatable :: entries(:)
        character(len=:), allocatable :: entry
        type(status_type) :: st
        integer :: k, slash

        allocate (ignores(0))

        if (len(backend) > 0) then
            call vc_ignore_completion_table(backend, dir, entries)

            do k = 1, size(entries)
                entry = entries(k)%text

                if (entry(1:1) == "!") then
                    ! No support for whitelisting (yet).
                    cycle
                end if

                slash = int(string_match("\(/\)[^/]", entry, st))

                if (slash > 0) then
                    ! FIXME: This seems to be Git-specific.  And / in the entry
                    ! (start or even the middle) means the pattern is
                    ! "rooted".  Or actually it is then relative to its
                    ! respective .gitignore (of which there could be
                    ! several), but we only support .gitignore at the root.
                    if (slash == 1) then
                        call append_string(ignores, "./"//entry(2:))
                    else
                        call append_string(ignores, "./"//entry)
                    end if
                else
                    call append_string(ignores, entry)
                end if
            end do
        end if

        call append_strings(ignores, extra_ignores)

        if (allocated(vc_directory_exclusions)) then
            do k = 1, size(vc_directory_exclusions)
                call append_string(ignores, trim(vc_directory_exclusions(k))//"/")
            end do
        end if
    end function project_vc_ignores_of

    !> The patterns of the files BACKEND ignores in DIR: the lines of its
    !> ignore file, without comments and blank lines.  Port of
    !> vc.el:vc-default-ignore-completion-table, with vc.el:vc--read-lines.
    subroutine vc_ignore_completion_table(backend, dir, entries)
        character(len=*), intent(in) :: backend, dir
        type(string_type), allocatable, intent(out) :: entries(:)
        character(len=:), allocatable :: file, text
        type(status_type) :: st
        logical :: found
        integer :: b, pt, nl

        allocate (entries(0))
        b = vc_backend_named(backend)
        if (b <= 0) return
        if (.not. associated(vc_backends(b)%find_ignore_file)) return
        call vc_backends(b)%find_ignore_file(dir, file, found)
        if (.not. found) return
        if (.not. file_exists(file)) return
        call file_text(file, text, found)
        if (.not. found) return
        pt = 1

        do while (pt <= len(text))
            nl = index(text(pt:), achar(10))

            if (nl == 0) then
                nl = len(text) + 1
            else
                nl = pt + nl - 1
            end if

            if (nl > pt) then
                ! Commented or empty lines.
                if (string_match("\`\(?:#\|[ "//achar(9)//achar(13)//achar(10)//"]*\'\)", text(pt:nl - 1), st) == 0) then
                    call append_string(entries, text(pt:nl - 1))
                end if
            end if

            pt = nl + 1
        end do
    end subroutine vc_ignore_completion_table

    !> The live buffers under the root of a VC-aware project, except those
    !> of submodules when they are not merged.  Port of
    !> project.el:project-buffers (the method for a VC-aware project).
    function vc_project_buffers(self) result(bufs)
        class(vc_project_type), intent(in) :: self
        type(handle_type), allocatable :: bufs(:)
        type(handle_type), allocatable :: all(:)
        type(string_type), allocatable :: modules(:), paths(:)
        type(handle_type) :: old, tmp
        character(len=:), allocatable :: root, dd
        logical :: inside
        integer :: k, j

        root = expanded(file_name_as_directory(self%root()))
        allocate (modules(0))

        if (.not. project_vc_merge_submodules .and. .not. is_submodule(root)) then
            call temp_begin(old, tmp, root)
            call git_submodules(paths)
            call temp_end(old, tmp)

            do k = 1, size(paths)
                call append_string(modules, root//paths(k)%text//"/")
            end do
        end if

        all = buffer_list()
        allocate (bufs(0))

        do k = 1, size(all)
            dd = "./"
            if (allocated(buffers(all(k)%index)%directory)) dd = buffers(all(k)%index)%directory
            dd = expanded(dd)
            if (.not. starts_with(dd, root)) cycle
            inside = .false.

            do j = 1, size(modules)
                if (starts_with(dd, modules(j)%text)) inside = .true.
            end do

            if (.not. inside) bufs = [bufs, all(k)]
        end do
    end function vc_project_buffers

    !> The name of a VC-aware project: `project_vc_name' if set, else the
    !> base name of its root.  Port of project.el:project-name (the method
    !> for a VC-aware project).
    function vc_project_name(self) result(name)
        class(vc_project_type), intent(in) :: self
        character(len=:), allocatable :: name

        if (allocated(project_vc_name)) then
            if (len(project_vc_name) > 0) then
                name = project_vc_name
                return
            end if
        end if

        name = file_name_nondirectory(directory_file_name(self%root()))
    end function vc_project_name

    !> A sorted and culled list of directory names: the directories of
    !> DIRS, without non-existing directories and directories a parent
    !> of whose is already in the list.  Port of
    !> project.el:project-combine-directories.
    function combine_project_directories(dirs) result(combined)
        type(string_type), intent(in) :: dirs(:)
        type(string_type), allocatable :: combined(:)
        type(string_type), allocatable :: sorted(:)
        integer :: k

        allocate (sorted(size(dirs)))

        do k = 1, size(dirs)
            sorted(k)%text = file_name_as_directory(expanded(dirs(k)%text))
        end do

        call sort_strings(sorted)
        allocate (combined(0))

        ! Delete subdirectories from the list.
        do k = 1, size(sorted)
            if (size(combined) > 0) then
                if (starts_with(sorted(k)%text, combined(size(combined))%text)) cycle
            end if

            call append_string(combined, sorted(k)%text)
        end do

        combined = pack(combined, [(file_exists(combined(k)%text), k=1, size(combined))])
    end function combine_project_directories

    !> The elements of FILES that are outside of DIRS (directory names).
    !> Port of project.el:project-subtract-directories.
    function subtract_project_directories(files, dirs) result(outside)
        type(string_type), intent(in) :: files(:), dirs(:)
        type(string_type), allocatable :: outside(:)
        logical :: inside
        integer :: k, j

        allocate (outside(0))

        do k = 1, size(files)
            inside = .false.

            do j = 1, size(dirs)
                if (is_file_in_directory(files(k)%text, dirs(j)%text)) inside = .true.
            end do

            if (.not. inside) call append_string(outside, files(k)%text)
        end do
    end function subtract_project_directories

    ! ------------------------------------------------------------------
    ! Buffer conditions
    ! ------------------------------------------------------------------

    !> A buffer condition: the buffer's name matches REGEXP.  Not a port.
    function project_buffer_name_regexp(regexp) result(c)
        character(len=*), intent(in) :: regexp
        type(project_buffer_condition_type) :: c

        allocate (c%nodes(1))
        c%nodes(1)%kind = CONDITION_NAME
        c%nodes(1)%text = regexp
    end function project_buffer_name_regexp

    !> A buffer condition: TEST gives .true. for the buffer.  Not a port.
    function project_buffer_test(test) result(c)
        procedure(project_buffer_test_procedure) :: test
        type(project_buffer_condition_type) :: c

        allocate (c%nodes(1))
        c%nodes(1)%kind = CONDITION_TEST
        c%nodes(1)%test => test
    end function project_buffer_test

    !> A buffer condition: the buffer's major mode is the mode named MODE.
    !> Not a port.
    function project_major_mode(mode) result(c)
        character(len=*), intent(in) :: mode
        type(project_buffer_condition_type) :: c

        allocate (c%nodes(1))
        c%nodes(1)%kind = CONDITION_MAJOR_MODE
        c%nodes(1)%text = mode
    end function project_major_mode

    !> A buffer condition: the buffer's major mode is derived from the
    !> mode named MODE.  Not a port.
    function project_derived_mode(mode) result(c)
        character(len=*), intent(in) :: mode
        type(project_buffer_condition_type) :: c

        allocate (c%nodes(1))
        c%nodes(1)%kind = CONDITION_DERIVED_MODE
        c%nodes(1)%text = mode
    end function project_derived_mode

    !> A buffer condition: the negation of C1.  Not a port.
    function project_not(c1) result(c)
        type(project_buffer_condition_type), intent(in) :: c1
        type(project_buffer_condition_type) :: c

        allocate (c%nodes(1))
        c%nodes(1)%kind = CONDITION_NOT
        c%nodes(1)%count = 1
        c%nodes = [c%nodes, c1%nodes]
    end function project_not

    !> A buffer condition: all of C1, C2, ... (up to 8) are met.
    !> Not a port.
    function project_and(c1, c2, c3, c4, c5, c6, c7, c8) result(c)
        type(project_buffer_condition_type), intent(in) :: c1
        type(project_buffer_condition_type), intent(in), optional :: c2, c3, c4, c5, c6, c7, c8
        type(project_buffer_condition_type) :: c

        c = combined(CONDITION_AND, c1, c2, c3, c4, c5, c6, c7, c8)
    end function project_and

    !> A buffer condition: one of C1, C2, ... (up to 8) is met.
    !> Not a port.
    function project_or(c1, c2, c3, c4, c5, c6, c7, c8) result(c)
        type(project_buffer_condition_type), intent(in) :: c1
        type(project_buffer_condition_type), intent(in), optional :: c2, c3, c4, c5, c6, c7, c8
        type(project_buffer_condition_type) :: c

        c = combined(CONDITION_OR, c1, c2, c3, c4, c5, c6, c7, c8)
    end function project_or

    ! The condition of KIND combining C1, C2, ...  Not a port.
    function combined(kind, c1, c2, c3, c4, c5, c6, c7, c8) result(c)
        integer, intent(in) :: kind
        type(project_buffer_condition_type), intent(in) :: c1
        type(project_buffer_condition_type), intent(in), optional :: c2, c3, c4, c5, c6, c7, c8
        type(project_buffer_condition_type) :: c

        allocate (c%nodes(1))
        c%nodes(1)%kind = kind
        call add(c1)
        if (present(c2)) call add(c2)
        if (present(c3)) call add(c3)
        if (present(c4)) call add(c4)
        if (present(c5)) call add(c5)
        if (present(c6)) call add(c6)
        if (present(c7)) call add(c7)
        if (present(c8)) call add(c8)

    contains

        subroutine add(sub)
            type(project_buffer_condition_type), intent(in) :: sub

            c%nodes(1)%count = c%nodes(1)%count + 1
            c%nodes = [c%nodes, sub%nodes]
        end subroutine add
    end function combined

    ! Not a port.
    subroutine conditions_append(self, condition)
        class(project_buffer_conditions_type), intent(inout) :: self
        type(project_buffer_condition_type), intent(in) :: condition
        type(project_buffer_condition_type), allocatable :: grown(:)
        integer :: n

        n = 0
        if (allocated(self%conditions)) n = size(self%conditions)
        allocate (grown(n + 1))
        if (n > 0) grown(1:n) = self%conditions
        grown(n + 1) = condition
        call move_alloc(grown, self%conditions)
    end subroutine conditions_append

    ! Not a port.
    subroutine conditions_clear(self)
        class(project_buffer_conditions_type), intent(inout) :: self

        if (allocated(self%conditions)) deallocate (self%conditions)
        allocate (self%conditions(0))
    end subroutine conditions_clear

    !> Whether buffer BUFFER matches any of CONDITIONS.  Port of
    !> project.el:project--buffer-check.
    recursive logical function is_project_buffer_matching(buffer, conditions) result(matches)
        type(handle_type), intent(in) :: buffer
        type(project_buffer_conditions_type), intent(in) :: conditions
        integer :: k, next

        matches = .false.
        if (.not. allocated(conditions%conditions)) return

        do k = 1, size(conditions%conditions)
            if (.not. allocated(conditions%conditions(k)%nodes)) cycle
            if (size(conditions%conditions(k)%nodes) == 0) cycle

            if (node_matches(buffer, conditions%conditions(k)%nodes, 1, next)) then
                matches = .true.
                return
            end if
        end do
    end function is_project_buffer_matching

    !> Whether BUFFER matches the condition whose first node is NODES(AT);
    !> NEXT is the index after its nodes.  Port of
    !> project.el:project--buffer-check (one condition).
    recursive logical function node_matches(buffer, nodes, at, next) result(matches)
        type(handle_type), intent(in) :: buffer
        type(project_condition_node_type), intent(in) :: nodes(:)
        integer, intent(in) :: at
        integer, intent(out) :: next
        character(len=:), allocatable :: mode
        type(status_type) :: st
        logical :: sub
        integer :: k, start

        matches = .false.
        next = at + 1
        mode = ""
        if (allocated(buffers(buffer%index)%major_mode)) mode = buffers(buffer%index)%major_mode

        select case (nodes(at)%kind)
        case (CONDITION_NAME)
            matches = string_match(trim(nodes(at)%text), buffers(buffer%index)%name, st) > 0
        case (CONDITION_TEST)
            if (associated(nodes(at)%test)) matches = nodes(at)%test(buffer)
        case (CONDITION_MAJOR_MODE)
            matches = mode == trim(nodes(at)%text)
        case (CONDITION_DERIVED_MODE)
            matches = provided_mode_derived_p(mode, trim(nodes(at)%text))
        case (CONDITION_NOT)
            start = next
            matches = .not. node_matches(buffer, nodes, start, next)
        case (CONDITION_OR)
            do k = 1, nodes(at)%count
                start = next
                sub = node_matches(buffer, nodes, start, next)
                if (sub) matches = .true.
            end do
        case (CONDITION_AND)
            matches = .true.

            do k = 1, nodes(at)%count
                start = next
                sub = node_matches(buffer, nodes, start, next)
                if (.not. sub) matches = .false.
            end do
        case default
        end select
    end function node_matches

    !> Whether BUFFER visits a file (the condition `buffer-file-name').
    !> Not a port.
    logical function is_file_buffer(buffer)
        type(handle_type), intent(in) :: buffer

        is_file_buffer = allocated(buffers(buffer%index)%filename)
    end function is_file_buffer

    ! ------------------------------------------------------------------
    ! The known projects
    ! ------------------------------------------------------------------

    !> Initialize the project list from the contents of
    !> `project_list_file'.  Port of project.el:project--read-project-list.
    recursive subroutine read_project_list()
        type(namelist_group_type), allocatable :: groups(:)
        character(len=:), allocatable :: text, name
        type(status_type) :: st
        logical :: found, wrong
        integer :: g, k, v

        if (allocated(project_list)) deallocate (project_list)
        allocate (project_list(0))
        if (.not. file_exists(project_list_file)) return
        call file_text(project_list_file, text, found)
        if (.not. found) return
        call read_namelist(text, groups, st)

        if (.not. st%ok()) then
            call message("Failed to read the projects list file: "//format_error(st))
            return
        end if

        wrong = .false.

        do g = 1, size(groups)
            if (groups(g)%name /= "project") cycle
            if (.not. allocated(groups(g)%items)) cycle

            do k = 1, size(groups(g)%items)
                if (groups(g)%items(k)%name /= "known_project_roots") cycle
                if (.not. groups(g)%items(k)%subscripted) cycle

                do v = 1, size(groups(g)%items(k)%values)
                    if (groups(g)%items(k)%values(v)%kind /= NAMELIST_STRING) then
                        wrong = .true.
                        cycle
                    end if

                    name = groups(g)%items(k)%values(v)%string
                    call append_string(project_list, file_name_as_directory(abbreviate_file_name(name)))
                end do
            end do
        end do

        if (wrong) then
            call message("Contents of "//project_list_file//" are in wrong format, resetting")
            deallocate (project_list)
            allocate (project_list(0))
        end if
    end subroutine read_project_list

    !> Initialize the project list if it isn't already initialized.
    !> Port of project.el:project--ensure-read-project-list.
    recursive subroutine ensure_read_project_list()
        if (list_read) return
        list_read = .true.
        call read_project_list()

        if (associated(project_prune_zombie_projects%list_first_read)) then
            if (size(project_list) > 0) call delete_zombie_projects(project_prune_zombie_projects%list_first_read)
        end if
    end subroutine ensure_read_project_list

    !> Save the project list in `project_list_file'.  Port of
    !> project.el:project--write-project-list.
    recursive subroutine write_project_list(status)
        type(status_type), intent(out) :: status
        type(namelist_group_type) :: group
        type(namelist_item_type) :: item
        type(handle_type) :: old, tmp
        type(status_type) :: st
        character(len=:), allocatable :: text, dir, wrote
        integer :: k

        if (associated(project_prune_zombie_projects%list_write)) then
            if (size(project_list) > 0) call delete_zombie_projects(project_prune_zombie_projects%list_write)
        end if

        group%name = "project"
        item%name = "known_project_roots"
        item%component = ""

        do k = 1, size(project_list)
            call item%append_string(expanded(project_list(k)%text))
        end do

        if (size(project_list) == 0) then
            call item%append_logical(.false.)
        else
            item%subscripted = .true.
            item%lower = 1
            item%upper = size(project_list)
        end if

        call group%append(item)
        text = "! List of known projects, automatically generated by `project'."//achar(10)//achar(10)// &
               namelist_text(group)
        dir = file_name_directory(expanded(project_list_file))
        if (.not. file_exists(dir)) call create_directory(dir, st, parents=.true.)
        call temp_begin(old, tmp, default_directory())
        call insert(text, st)
        call write_region(project_list_file, .false., status, message=wrote)
        call temp_end(old, tmp)
    end subroutine write_project_list

    !> Add project root ROOT to the front of the project list.
    !> Save the result in `project_list_file' if the list of projects has
    !> changed, unless NO_WRITE is .true..  If STABLE is .true., don't move
    !> ROOT to the front of the project list if it's already present
    !> further down the project list.  Port of
    !> project.el:project--remember-dir.
    recursive subroutine remember_dir(root, no_write, stable, status)
        character(len=*), intent(in) :: root
        logical, intent(in) :: no_write, stable
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: kept(:)
        character(len=:), allocatable :: dir
        logical :: present_already
        integer :: k

        status = status_type()
        call ensure_read_project_list()
        dir = abbreviate_file_name(root)

        if (stable) then
            present_already = has_string(project_list, dir)
        else
            present_already = .false.

            if (size(project_list) > 0) then
                present_already = project_list(1)%text == dir .and. len(project_list(1)%text) == len(dir)
            end if
        end if

        if (present_already) return
        allocate (kept(0))
        call append_string(kept, dir)

        do k = 1, size(project_list)
            if (.not. stable .and. project_list(k)%text == dir .and. len(project_list(k)%text) == len(dir)) cycle
            call append_string(kept, project_list(k)%text)
        end do

        call move_alloc(kept, project_list)
        if (.not. no_write) call write_project_list(status)
    end subroutine remember_dir

    !> Add project PROJECT to the front of the project list.
    !> If project PROJECT satisfies `project_list_exclude', then nothing is
    !> done.  Save the result in `project_list_file' if the list of
    !> projects has changed.  NO_WRITE .true. means to suppress saving
    !> `project_list_file'.  STABLE means don't move PROJECT to the front
    !> of the project list if it's already present further down the
    !> project list.  INTERACTIVE says so in messages.  Port of
    !> project.el:project-remember-project.
    recursive subroutine remember_project(project, status, no_write, stable, interactive)
        class(project_type), intent(in) :: project
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_write, stable, interactive
        character(len=:), allocatable :: root
        type(status_type) :: st
        logical :: excluded, interact, nw, sb
        integer :: k

        status = status_type()
        root = project%root()
        interact = .false.
        if (present(interactive)) interact = interactive
        nw = .false.
        if (present(no_write)) nw = no_write
        sb = .false.
        if (present(stable)) sb = stable
        excluded = .false.

        if (allocated(project_list_exclude)) then
            do k = 1, size(project_list_exclude)
                if (associated(project_list_exclude(k)%test)) then
                    if (project_list_exclude(k)%test(project)) excluded = .true.
                else if (len_trim(project_list_exclude(k)%regexp) > 0) then
                    if (string_match(trim(project_list_exclude(k)%regexp), root, st) > 0) excluded = .true.
                end if
            end do
        end if

        if (excluded) then
            if (interact) call message("Current project is blacklisted!")
        else
            call remember_dir(root, nw, sb, status)
            if (.not. status%ok()) return
            if (interact) call message("Current project remembered")
        end if
    end subroutine remember_project

    !> The command: `remember_project' of the current project.  Port of
    !> the command of project.el:project-remember-project.
    recursive subroutine cmd_remember_project(status)
        type(status_type), intent(out) :: status
        class(project_type), allocatable :: pr

        call get_current_project(pr, status, maybe_prompt=.true.)
        if (.not. status%ok()) return
        call remember_project(pr, status, interactive=.true.)
    end subroutine cmd_remember_project

    !> Remove directory PROJECT_ROOT of a missing project from the project
    !> list.  If the directory was in the list before the removal, save the
    !> result in `project_list_file'.  Announce the project's removal from
    !> the list with the message BEFORE, the root, then AFTER.  Port of
    !> project.el:project--remove-from-project-list.
    recursive subroutine remove_from_project_list(project_root, before, after, status)
        character(len=*), intent(in) :: project_root, before, after
        type(status_type), intent(out) :: status
        type(string_type), allocatable :: kept(:)
        integer :: k

        status = status_type()
        call ensure_read_project_list()
        if (.not. has_string(project_list, project_root)) return
        allocate (kept(0))

        do k = 1, size(project_list)
            if (project_list(k)%text == project_root .and. len(project_list(k)%text) == len(project_root)) cycle
            call append_string(kept, project_list(k)%text)
        end do

        call move_alloc(kept, project_list)
        call message(before//project_root//after)
        call write_project_list(status)
    end subroutine remove_from_project_list

    !> Remove directory PROJECT_ROOT from the project list.
    !> PROJECT_ROOT is the root directory of a known project listed in the
    !> project list.  Port of project.el:project-forget-project.
    recursive subroutine forget_project(project_root, status)
        character(len=*), intent(in) :: project_root
        type(status_type), intent(out) :: status

        call remove_from_project_list(project_root, "Project `", "' removed from known projects", status)
    end subroutine forget_project

    !> The command: read a project and forget it.  Port of the command of
    !> project.el:project-forget-project.
    recursive subroutine cmd_forget_project(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: dir

        call execute_prompter(dir, status)
        if (.not. status%ok()) return
        call forget_project(dir, status)
    end subroutine cmd_forget_project

    !> The root directories of all known projects.  Port of
    !> project.el:project-known-project-roots.
    recursive function known_project_roots() result(roots)
        type(string_type), allocatable :: roots(:)

        call ensure_read_project_list()
        roots = project_list
    end function known_project_roots

    !> Forget the known projects whose roots TEST accepts and that no
    !> longer exist.  Port of project.el:project--delete-zombie-projects.
    recursive subroutine delete_zombie_projects(test)
        procedure(project_root_test_procedure), optional :: test
        type(string_type), allocatable :: roots(:)
        type(status_type) :: st
        logical :: selected
        integer :: k

        roots = known_project_roots()

        do k = 1, size(roots)
            selected = .true.
            if (present(test)) selected = test(roots(k)%text)

            if (selected .and. .not. file_exists(roots(k)%text)) then
                call forget_project(roots(k)%text, st)
                if (.not. st%ok()) return
            end if
        end do
    end subroutine delete_zombie_projects

    !> Forget all known projects that don't exist any more.  INTERACTIVE
    !> applies `project_prune_zombie_projects''s INTERACTIVELY test.
    !> Port of project.el:project-forget-zombie-projects.
    recursive subroutine forget_zombie_projects(interactive)
        logical, intent(in), optional :: interactive
        logical :: interact

        call project_clear_cache()
        interact = .false.
        if (present(interactive)) interact = interactive

        if (interact .and. associated(project_prune_zombie_projects%interactively)) then
            call delete_zombie_projects(project_prune_zombie_projects%interactively)
        else
            call delete_zombie_projects()
        end if
    end subroutine forget_zombie_projects

    ! The command.
    recursive subroutine cmd_forget_zombie_projects(status)
        type(status_type), intent(out) :: status

        status = status_type()
        call forget_zombie_projects(interactive=.true.)
    end subroutine cmd_forget_zombie_projects

    !> Default function used in `project_prune_zombie_projects'.
    !> Return .true. if PROJECT (a root) is not a remote project.  Port of
    !> project.el:project-prune-zombies-default (no remote files: always
    !> .true.).
    logical function is_local_project(project)
        character(len=*), intent(in) :: project

        is_local_project = .true.
    end function is_local_project

    !> Remember projects below a directory DIR.
    !> RECURSIVE, if .true. (interactively, the prefix argument) means
    !> recurse into subdirectories of DIR to find more projects.
    !> Display a message at the end summarizing what was found.
    !> COUNT is the number of detected projects.  Port of
    !> project.el:project-remember-projects-under.
    recursive subroutine remember_projects_under(dir, count, status, recursive)
        character(len=*), intent(in) :: dir
        integer, intent(out) :: count
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: recursive
        type(string_type), allocatable :: dirs(:), known(:)
        class(project_type), allocatable :: pr
        character(len=:), allocatable :: root
        logical :: recurse
        integer :: k
        character(len=24) :: number

        status = status_type()
        call project_clear_cache()
        call ensure_read_project_list()
        recurse = .false.
        if (present(recursive)) recurse = recursive
        allocate (dirs(0))
        call list_subdirectories(file_name_as_directory(expanded(dir)), recurse, dirs)
        known = project_list
        count = 0

        do k = 1, size(dirs)
            if (.not. is_file_directory(dirs(k)%text)) cycle
            call find_project_in_directory(dirs(k)%text, pr, status)
            if (.not. status%ok()) return
            if (.not. allocated(pr)) cycle
            root = pr%root()
            if (has_string(known, root)) cycle
            call remember_project(pr, status, no_write=.true.)
            if (.not. status%ok()) return
            call append_string(known, root)
            call message("Found "//root//"...")
            count = count + 1
        end do

        if (count == 0) then
            call message("No projects were found")
        else
            call write_project_list(status)
            if (.not. status%ok()) return

            if (count == 1) then
                call message("1 project was found")
            else
                write (number, "(i0)") count
                call message(trim(number)//" projects were found")
            end if
        end if
    end subroutine remember_projects_under

    !> The files of directory DIR (absolute, without . and ..), and with
    !> RECURSE those of its subdirectories too (directory-files and
    !> directory-files-recursively with INCLUDE-DIRECTORIES).  Not a port.
    recursive subroutine list_subdirectories(dir, recurse, files)
        character(len=*), intent(in) :: dir
        logical, intent(in) :: recurse
        type(string_type), allocatable, intent(inout) :: files(:)
        type(string_type), allocatable :: names(:)
        type(string_type), allocatable :: entries(:)
        integer :: k

        if (fortmacs_directory_entries(directory_file_name(dir), names) /= 0) return
        allocate (entries(0))

        do k = 1, size(names)
            if (names(k)%text == "." .or. names(k)%text == "..") cycle
            call append_string(entries, dir//names(k)%text)
        end do

        call sort_strings(entries)

        do k = 1, size(entries)
            if (recurse .and. is_file_directory(entries(k)%text)) then
                call list_subdirectories(file_name_as_directory(entries(k)%text), recurse, files)
            end if

            call append_string(files, entries(k)%text)
        end do
    end subroutine list_subdirectories

    !> The command: read a directory, with the prefix argument as RECURSIVE.
    !> Port of the command of project.el:project-remember-projects-under.
    recursive subroutine cmd_remember_projects_under(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: dir
        integer :: count

        call read_directory_name("Directory: ", dir, status, mustmatch=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        call remember_projects_under(dir, count, status, current_prefix_arg%kind /= PREFIX_NONE)
    end subroutine cmd_remember_projects_under

    !> Forget all known projects below a directory DIR.
    !> RECURSIVE, if .true. (interactively, the prefix argument), means
    !> recurse into subdirectories under DIR to remove those projects from
    !> the index.  Display a message at the end summarizing what was
    !> forgotten.  COUNT is the number of forgotten projects.  Port of
    !> project.el:project-forget-projects-under.
    recursive subroutine forget_projects_under(dir, count, status, recursive)
        character(len=*), intent(in) :: dir
        integer, intent(out) :: count
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: recursive
        type(string_type), allocatable :: roots(:)
        logical :: recurse, forget
        integer :: k
        character(len=24) :: number

        status = status_type()
        call project_clear_cache()
        recurse = .false.
        if (present(recursive)) recurse = recursive
        roots = known_project_roots()
        count = 0

        do k = 1, size(roots)
            if (recurse) then
                forget = is_file_in_directory(roots(k)%text, dir)
            else
                forget = is_file_equal(file_name_directory(roots(k)%text), dir)
            end if

            if (forget) then
                call forget_project(roots(k)%text, status)
                if (.not. status%ok()) return
                count = count + 1
            end if
        end do

        if (count == 0) then
            call message("No projects were forgotten")
        else
            call write_project_list(status)
            if (.not. status%ok()) return

            if (count == 1) then
                call message("1 project was forgotten")
            else
                write (number, "(i0)") count
                call message(trim(number)//" projects were forgotten")
            end if
        end if
    end subroutine forget_projects_under

    !> The command: read a directory, with the prefix argument as RECURSIVE.
    !> Port of the command of project.el:project-forget-projects-under.
    recursive subroutine cmd_forget_projects_under(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: dir
        integer :: count

        call read_directory_name("Directory: ", dir, status, mustmatch=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        call forget_projects_under(dir, count, status, current_prefix_arg%kind /= PREFIX_NONE)
    end subroutine cmd_forget_projects_under

    ! ------------------------------------------------------------------
    ! Prompting for a project
    ! ------------------------------------------------------------------

    !> Prompt the user for a directory that is one of the known project
    !> roots.  The project is chosen among projects known from the project
    !> list, see `project_list_file'.  If PROMPT is present, use it as the
    !> prompt string.  If TEST is present, filter possible project choices
    !> using this function; see `project_prompter' for more details.
    !> Unless REQUIRE_KNOWN is .true., it's also possible to enter an
    !> arbitrary directory not in the list of known projects.  Port of
    !> project.el:project-prompt-project-dir.
    recursive subroutine read_project_dir(dir, status, prompt, test, require_known)
        character(len=:), allocatable, intent(out) :: dir
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: prompt
        procedure(project_root_test_procedure), optional :: test
        logical, intent(in), optional :: require_known
        class(project_type), allocatable :: p
        type(string_type), allocatable :: choices(:)
        type(string_type), allocatable :: saved(:)
        character(len=:), allocatable :: current_root, full_prompt
        logical :: known_only
        integer :: k

        call ensure_read_project_list()

        if (associated(project_prune_zombie_projects%prompt)) then
            call delete_zombie_projects(project_prune_zombie_projects%prompt)
        end if

        current_root = ""
        call get_current_project(p, status)
        if (.not. status%ok()) return

        if (allocated(p)) then
            current_root = p%root()

            if (present(test)) then
                if (.not. test(current_root)) current_root = ""
            end if
        end if

        known_only = .false.
        if (present(require_known)) known_only = require_known
        allocate (choices(0))

        do k = 1, size(project_list)
            if (present(test)) then
                if (.not. test(project_list(k)%text)) cycle
            end if

            call append_string(choices, project_list(k)%text)
        end do

        if (.not. known_only) call append_string(choices, DIR_CHOICE)

        if (present(prompt)) then
            full_prompt = prompt//": "
        else if (len(current_root) > 0) then
            full_prompt = "Select project (default current project): "
        else
            full_prompt = "Select project: "
        end if

        ! project--dir-history is the known roots, and no input is added.
        if (allocated(project_dir_history%items)) call move_alloc(project_dir_history%items, saved)
        allocate (project_dir_history%items(size(project_list)))

        do k = 1, size(project_list)
            project_dir_history%items(k)%text = project_list(k)%text
        end do

        dir = ""

        ! If the user simply pressed RET (and CURRENT is none), do this
        ! again until they don't.
        do while (len(dir) == 0)
            if (len(current_root) > 0) then
                call completing_read(full_prompt, completion_list_table(string_list(choices), category="project-file"), &
                                     dir, status, require_match=REQUIRE_MATCH_YES, history="project_dir_history", &
                                     default=current_root)
            else
                call completing_read(full_prompt, completion_list_table(string_list(choices), category="project-file"), &
                                     dir, status, require_match=REQUIRE_MATCH_YES, history="project_dir_history")
            end if

            if (.not. status%ok()) exit
        end do

        if (allocated(project_dir_history%items)) deallocate (project_dir_history%items)
        if (allocated(saved)) call move_alloc(saved, project_dir_history%items)
        if (.not. status%ok()) return

        if (dir == DIR_CHOICE) then
            call read_directory_name("Select directory: ", dir, status, dir=default_directory(), &
                                     mustmatch=REQUIRE_MATCH_YES)
        end if
    end subroutine read_project_dir

    !> Prompt the user for a project, by name, that is one of the known
    !> project roots.  The project is chosen among projects known from the
    !> project list, see `project_list_file'.  If PROMPT is present, use it
    !> as the prompt string.  If TEST is present, filter possible project
    !> choices using this function; see `project_prompter' for more
    !> details.  Unless REQUIRE_KNOWN is .true., it's also possible to enter
    !> an arbitrary directory not in the list of known projects.  Port of
    !> project.el:project-prompt-project-name.
    recursive subroutine read_project_name(dir, status, prompt, test, require_known)
        character(len=:), allocatable, intent(out) :: dir
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: prompt
        procedure(project_root_test_procedure), optional :: test
        logical, intent(in), optional :: require_known
        class(project_type), allocatable :: p, proj
        type(string_type), allocatable :: choices(:)
        type(string_type), allocatable :: roots(:), names(:), name_roots(:)
        type(string_type), allocatable :: saved(:)
        character(len=:), allocatable :: current_name, full_prompt, name
        logical :: known_only
        integer :: k

        if (associated(project_prune_zombie_projects%prompt)) then
            call delete_zombie_projects(project_prune_zombie_projects%prompt)
        end if

        current_name = ""
        call get_current_project(p, status)
        if (.not. status%ok()) return

        if (allocated(p)) then
            current_name = p%name()

            if (present(test)) then
                if (.not. test(p%root())) current_name = ""
            end if
        end if

        roots = known_project_roots()
        allocate (names(0), name_roots(0))

        do k = 1, size(roots)
            ! We filter out directories that no longer map to a project,
            ! since they don't have a clean project name.
            if (present(test)) then
                if (.not. test(roots(k)%text)) cycle
            end if

            call find_project_in_directory(roots(k)%text, proj, status)
            if (.not. status%ok()) return
            if (.not. allocated(proj)) cycle
            name = proj%name()
            call append_string(names, name)
            call append_string(name_roots, proj%root())
        end do

        known_only = .false.
        if (present(require_known)) known_only = require_known
        allocate (choices(0))
        if (.not. known_only) call append_string(choices, DIR_CHOICE)

        do k = size(names), 1, -1
            call append_string(choices, names(k)%text)
        end do

        if (present(prompt)) then
            full_prompt = prompt//": "
        else if (len(current_name) > 0) then
            full_prompt = "Select project (default current project): "
        else
            full_prompt = "Select project: "
        end if

        if (allocated(project_name_history%items)) call move_alloc(project_name_history%items, saved)
        allocate (project_name_history%items(size(names)))

        do k = 1, size(names)
            project_name_history%items(k)%text = names(k)%text
        end do

        dir = ""
        name = ""

        ! If the user simply pressed RET (and CURRENT is none), do this
        ! again until they don't.
        do while (len(name) == 0)
            if (len(current_name) > 0) then
                call completing_read(full_prompt, completion_list_table(string_list(choices), category="project-file"), &
                                     name, status, require_match=REQUIRE_MATCH_YES, history="project_name_history", &
                                     default=current_name)
            else
                call completing_read(full_prompt, completion_list_table(string_list(choices), category="project-file"), &
                                     name, status, require_match=REQUIRE_MATCH_YES, history="project_name_history")
            end if

            if (.not. status%ok()) exit
        end do

        if (allocated(project_name_history%items)) deallocate (project_name_history%items)
        if (allocated(saved)) call move_alloc(saved, project_name_history%items)
        if (.not. status%ok()) return

        if (name == DIR_CHOICE) then
            call read_directory_name("Select directory: ", dir, status, dir=default_directory(), &
                                     mustmatch=REQUIRE_MATCH_YES)
            return
        end if

        k = 0

        do while (k < size(names))
            k = k + 1
            if (names(k)%text == name .and. len(names(k)%text) == len(name)) exit
        end do

        if (k > 0) dir = name_roots(k)%text
    end subroutine read_project_name

    !> Read a project with completion from the known list.  Port of
    !> project.el:project-read-project.
    recursive subroutine read_project(project, status)
        class(project_type), allocatable, intent(out) :: project
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: dir

        ! Will prompt again if the entered directory is not a project
        ! anymore.
        call execute_prompter(dir, status)
        if (.not. status%ok()) return
        call get_current_project(project, status, maybe_prompt=.true., directory=dir)
    end subroutine read_project

    ! ------------------------------------------------------------------
    ! Finding files
    ! ------------------------------------------------------------------

    !> Ensure FILENAME is in PROJECT.
    !>
    !> Usually, just return FILENAME.  But if
    !> `project_current_directory_override' is set, adjust it to be
    !> relative to PROJECT instead.
    !>
    !> This supports using a relative file name from the current buffer
    !> when switching projects with `switch_project' and then using a
    !> command like `find_project_file'.  Port of
    !> project.el:project--find-default-from.
    recursive function find_default_from(filename, project) result(name)
        character(len=*), intent(in) :: filename
        class(project_type), intent(in) :: project
        character(len=:), allocatable :: name
        class(project_type), allocatable :: filename_proj
        type(status_type) :: st

        name = filename
        if (.not. allocated(project_current_directory_override%value)) return
        if (len(project_current_directory_override%value) == 0) return
        call get_current_project(filename_proj, st, directory=default_directory())
        if (.not. allocated(filename_proj)) return
        name = file_name_as_directory(project%root())//file_relative_name(filename, filename_proj%root())
    end function find_default_from

    !> Visit a file (with completion) in the current project.
    !>
    !> The file name at point, if any, is available as part of "future
    !> history".  If none, the current buffer's file name is used.
    !>
    !> If INCLUDE_ALL is .true., or with prefix argument when called
    !> interactively, include all files under the project root, except for
    !> VCS directories listed in `vc_directory_exclusions'.  Port of
    !> project.el:project-find-file (the file name at point,
    !> `thing_at_point' filename, is Deferred).
    recursive subroutine find_project_file(status, include_all)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: include_all
        class(project_type), allocatable :: pr
        type(string_type), allocatable :: dirs(:), defaults(:)
        logical :: saved_relative

        call get_current_project(pr, status, maybe_prompt=.true.)
        if (.not. status%ok()) return
        allocate (dirs(1))
        dirs(1)%text = pr%root()
        call suggested_names(pr, defaults)
        saved_relative = project_files_relative_names
        project_files_relative_names = .true.
        call find_file_in_project(defaults, dirs, pr, status, include_all)
        project_files_relative_names = saved_relative
    end subroutine find_project_file

    ! The command.
    recursive subroutine cmd_find_project_file(status)
        type(status_type), intent(out) :: status

        call find_project_file(status, current_prefix_arg%kind /= PREFIX_NONE)
    end subroutine cmd_find_project_file

    !> The file names suggested as "future history": the current buffer's
    !> file name (relative to PROJECT through `find_default_from').
    !> Not a port.
    recursive subroutine suggested_names(project, names)
        class(project_type), intent(in) :: project
        type(string_type), allocatable, intent(out) :: names(:)

        allocate (names(0))
        if (current == 0) return
        if (allocated(buffers(current)%filename)) call append_string(names, find_default_from(buffers(current)%filename, &
                                                                                              project))
    end subroutine suggested_names

    !> Visit a file (with completion) in the current project or external
    !> roots.
    !>
    !> The file name at point, if any, is available as part of "future
    !> history".  If none, the current buffer's file name is used.
    !>
    !> If INCLUDE_ALL is .true., or with prefix argument when called
    !> interactively, include all files under the project root, except for
    !> VCS directories listed in `vc_directory_exclusions'.  Port of
    !> project.el:project-or-external-find-file (the file name at point is
    !> Deferred).
    recursive subroutine find_project_or_external_file(status, include_all)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: include_all
        class(project_type), allocatable :: pr
        type(string_type), allocatable :: dirs(:), defaults(:)

        call get_current_project(pr, status, maybe_prompt=.true.)
        if (.not. status%ok()) return
        allocate (dirs(0))
        call append_string(dirs, pr%root())
        call append_strings(dirs, pr%external_roots())
        call suggested_names(pr, defaults)
        call find_file_in_project(defaults, dirs, pr, status, include_all)
    end subroutine find_project_or_external_file

    ! The command.
    recursive subroutine cmd_find_project_or_external_file(status)
        type(status_type), intent(out) :: status

        call find_project_or_external_file(status, current_prefix_arg%kind /= PREFIX_NONE)
    end subroutine cmd_find_project_or_external_file

    !> Edit file FILENAME.
    !> Interactively, prompt for FILENAME, defaulting to the root directory
    !> of the current project.  Port of project.el:project-root-find-file.
    recursive subroutine find_file_in_project_root(filename, status)
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status

        call find_file(filename, status)
    end subroutine find_file_in_project_root

    ! The command.
    recursive subroutine cmd_find_file_in_project_root(status)
        type(status_type), intent(out) :: status
        class(project_type), allocatable :: pr
        character(len=:), allocatable :: file

        call get_current_project(pr, status, maybe_prompt=.true.)
        if (.not. status%ok()) return
        call read_file_name("Find file in root: ", file, status, dir=pr%root(), mustmatch=REQUIRE_MATCH_CONFIRM)
        if (.not. status%ok()) return
        call find_file_in_project_root(file, status)
    end subroutine cmd_find_file_in_project_root

    !> Complete a file name in DIRS in PROJECT and visit the result.
    !>
    !> SUGGESTED_FILENAME is a file name, or part of it, which is used as
    !> part of "future history".
    !>
    !> If INCLUDE_ALL is .true., or with prefix argument when called
    !> interactively, include all files from DIRS, except for VCS
    !> directories listed in `vc_directory_exclusions'.  Port of
    !> project.el:project-find-file-in.
    recursive subroutine find_file_in_project(suggested_filename, dirs, project, status, include_all)
        type(string_type), intent(in) :: suggested_filename(:), dirs(:)
        class(project_type), intent(in) :: project
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: include_all
        type(string_type), allocatable :: vc_dirs_ignores(:), all_files(:), more(:)
        character(len=:), allocatable :: file, saved_dir
        logical :: all, saved_ignore_case
        integer :: k

        all = .false.
        if (present(include_all)) all = include_all
        allocate (vc_dirs_ignores(0))

        if (all .and. allocated(vc_directory_exclusions)) then
            do k = 1, size(vc_directory_exclusions)
                call append_string(vc_dirs_ignores, trim(vc_directory_exclusions(k))//"/")
            end do
        end if

        if (all) then
            allocate (all_files(0))

            do k = 1, size(dirs)
                call project_files_in_directory(dirs(k)%text, vc_dirs_ignores, more, status)
                if (.not. status%ok()) return
                call append_strings(all_files, more)
            end do
        else
            call project%list_files(all_files, status, dirs)
            if (.not. status%ok()) return
        end if

        saved_ignore_case = completion_ignore_case%value
        completion_ignore_case = read_file_name_completion_ignore_case
        saved_dir = default_directory()
        call set_default_directory(file_name_as_directory(project%root()))
        call read_project_file_name(project, "Find file", all_files, file, status, "file_name_history", &
                                    suggested_filename)
        call set_default_directory(saved_dir)
        completion_ignore_case = saved_ignore_case
        if (.not. status%ok()) return

        if (len(file) == 0) then
            call set_error(status, FM_USER_ERROR, "You didn't specify the file")
            return
        end if

        call find_file(expanded(file, file_name_as_directory(project%root())), status)
    end subroutine find_file_in_project

    !> Call `project_read_file_name_function' with appropriate history.
    !> An error condition occurs for no files.  Port of
    !> project.el:project--read-file-name (the relativizing of
    !> `project_file_history_behavior' is Deferred).
    recursive subroutine read_project_file_name(project, prompt, all_files, file, status, history, mb_default)
        class(project_type), intent(in) :: project
        character(len=*), intent(in) :: prompt
        type(string_type), intent(in) :: all_files(:)
        character(len=:), allocatable, intent(out) :: file
        type(status_type), intent(out) :: status
        character(len=*), intent(in) :: history
        type(string_type), intent(in) :: mb_default(:)
        type(history_setting_type), pointer :: hist
        type(string_type), allocatable :: saved(:)

        file = ""

        if (size(all_files) == 0) then
            call set_error(status, FM_USER_ERROR, "Empty file list")
            return
        end if

        ! (let ((history-add-new-input nil)) ...): the history as it was.
        hist => find_history(history, create=.true.)
        if (allocated(hist%items)) saved = hist%items

        if (associated(project_read_file_name_function)) then
            call project_read_file_name_function(prompt, all_files, file, status, history, mb_default)
        else
            call read_project_file_relative(prompt, all_files, file, status, history, mb_default)
        end if

        if (allocated(hist%items)) deallocate (hist%items)
        if (allocated(saved)) call move_alloc(saved, hist%items)
        if (.not. status%ok()) return
        ! (The `project' text property of the entry, for relativizing, is
        ! Deferred.)
        call hist%append(file)
    end subroutine read_project_file_name

    !> Read a file name, prompting with PROMPT.
    !> ALL_FILES is a list of possible file name completions.
    !>
    !> HISTORY has the same meaning as in `completing_read'.
    !>
    !> MB_DEFAULT is used as part of "future history", to be inserted by
    !> the user at will.  Port of project.el:project--read-file-cpd-relative.
    recursive subroutine read_project_file_relative(prompt, all_files, file, status, history, mb_default)
        character(len=*), intent(in) :: prompt
        type(string_type), intent(in) :: all_files(:)
        character(len=:), allocatable, intent(out) :: file
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: history
        type(string_type), intent(in), optional :: mb_default(:)
        type(value_type), allocatable :: substrings(:)
        type(value_type) :: common
        type(string_type), allocatable :: defaults(:)
        character(len=:), allocatable :: common_prefix, cpd, full_prompt, relname
        integer :: cpd_length, k

        file = ""
        allocate (substrings(size(all_files)))

        do k = 1, size(all_files)
            substrings(k) = value_type(text=all_files(k)%text)
        end do

        call try_completion("", completion_list_table(make_list(substrings)), common, status)
        if (.not. status%ok()) return
        common_prefix = ""
        if (vtag(common) == V_STRING) common_prefix = string_value(common)
        cpd = ""
        if (len(common_prefix) > 0) cpd = file_name_directory(common_prefix)
        cpd_length = len(cpd)

        if (size(all_files) > 0) then
            if (.not. is_file_name_absolute(all_files(1)%text)) cpd = default_directory()//cpd
        end if

        full_prompt = prompt//" in "//cpd

        if (cpd_length == 0 .and. size(all_files) > 0) then
            if (is_file_name_absolute(all_files(1)%text)) full_prompt = prompt
        end if

        do k = 1, size(all_files)
            substrings(k) = value_type(text=all_files(k)%text(cpd_length + 1:))
        end do

        allocate (defaults(0))
        if (present(mb_default)) defaults = mb_default

        if (len(cpd) > 0) then
            call completing_read_strict(full_prompt, completion_list_table(make_list(substrings), category="project-file"), &
                                        relname, status, history, defaults, cpd)
        else
            call completing_read_strict(full_prompt, completion_list_table(make_list(substrings), category="project-file"), &
                                        relname, status, history, defaults)
        end if

        if (.not. status%ok()) return

        if (len(cpd) > 0) then
            file = expanded(relname, cpd)
        else
            file = expanded(relname)
        end if
    end subroutine read_project_file_relative

    !> The text of the string value V.  Not a port.
    function string_value(v) result(s)
        use value_mod, only: xstring
        type(value_type), intent(in) :: v
        character(len=:), allocatable :: s

        s = xstring(v)
    end function string_value

    !> Read a file name, prompting with PROMPT, among ALL_FILES as they are.
    !> Port of project.el:project--read-file-absolute.
    recursive subroutine read_project_file_absolute(prompt, all_files, file, status, history, mb_default)
        character(len=*), intent(in) :: prompt
        type(string_type), intent(in) :: all_files(:)
        character(len=:), allocatable, intent(out) :: file
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: history
        type(string_type), intent(in), optional :: mb_default(:)
        type(value_type), allocatable :: names(:)
        type(string_type), allocatable :: defaults(:)
        character(len=:), allocatable :: new_prompt
        logical :: names_absolute
        integer :: k

        names_absolute = .false.
        if (size(all_files) > 0) names_absolute = is_file_name_absolute(all_files(1)%text)
        new_prompt = prompt
        if (.not. names_absolute) new_prompt = prompt//" in "//default_directory()
        allocate (names(size(all_files)))

        do k = 1, size(all_files)
            names(k) = value_type(text=all_files(k)%text)
        end do

        allocate (defaults(0))
        if (present(mb_default)) defaults = mb_default

        ! TODO: The names are intentionally not absolute in many cases.
        ! Probably better to rename this function.
        if (names_absolute) then
            call completing_read_strict(new_prompt, completion_list_table(make_list(names), category="project-file"), &
                                        file, status, history, defaults)
        else
            call completing_read_strict(new_prompt, completion_list_table(make_list(names), category="project-file"), &
                                        file, status, history, defaults, default_directory())
        end if

        if (.not. status%ok()) return
        if (.not. is_file_name_absolute(file)) file = expanded(file)
    end subroutine read_project_file_absolute

    !> Read with PROMPT a name in COLLECTION (confirming a name not in it),
    !> with the history HISTORY limited to the names under
    !> COMMON_PARENT_DIRECTORY (relative to it), and MB_DEFAULT (made
    !> relative to it) as "future history".  Port of
    !> project.el:project--completing-read-strict.
    recursive subroutine completing_read_strict(prompt, collection, result, status, history, mb_default, &
                                                common_parent_directory)
        character(len=*), intent(in) :: prompt
        type(completion_table_type), intent(in) :: collection
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: history
        type(string_type), intent(in) :: mb_default(:)
        character(len=*), intent(in), optional :: common_parent_directory
        type(history_setting_type), pointer :: hist
        type(string_type), allocatable :: saved(:), kept(:)
        type(string_type) :: item
        type(string_type), allocatable :: defaults(:)
        character(len=:), allocatable :: abs_cpd, s, histname
        logical :: saved_non_essential
        integer :: k

        allocate (defaults(size(mb_default)))

        do k = 1, size(mb_default)
            defaults(k)%text = mb_default(k)%text

            if (present(common_parent_directory)) then
                if (is_file_name_absolute(mb_default(k)%text)) then
                    defaults(k)%text = file_relative_name(mb_default(k)%text, common_parent_directory)
                end if
            end if
        end do

        histname = "minibuffer_history"
        if (present(history)) histname = history
        hist => find_history(histname, create=.true.)
        if (allocated(hist%items)) saved = hist%items

        if (present(common_parent_directory)) then
            abs_cpd = expanded(common_parent_directory)
            allocate (kept(0))

            if (allocated(saved)) then
                do k = 1, size(saved)
                    s = expanded(saved(k)%text)

                    if (starts_with(s, abs_cpd) .and. len(s) /= len(abs_cpd)) then
                        item%text = s(len(abs_cpd) + 1:)
                        kept = [kept, item]
                    end if
                end do
            end if

            call move_alloc(kept, hist%items)
        end if

        ! Avoid new remote connections.
        saved_non_essential = non_essential
        non_essential = .true.
        ! (minibuffer-with-setup-hook ... minibuffer-default-add-function):
        ! the defaults for M-n only.
        pending_defaults = defaults
        minibuffer_setup_function => set_pending_defaults
        call completing_read(prompt//": ", collection, result, status, require_match=REQUIRE_MATCH_CONFIRM, &
                             history=histname)
        minibuffer_setup_function => null()
        non_essential = saved_non_essential
        if (allocated(hist%items)) deallocate (hist%items)
        if (allocated(saved)) call move_alloc(saved, hist%items)
    end subroutine completing_read_strict

    !> The minibuffer setup of `completing_read_strict': the defaults M-n
    !> offers.  Port of project.el:project--completing-read-strict (its
    !> setup function; the completions of
    !> `minibuffer_default_add_completions' are Deferred).
    subroutine set_pending_defaults()
        if (.not. allocated(pending_defaults)) return
        if (size(pending_defaults) > 0) minibuffer_default = pending_defaults
    end subroutine set_pending_defaults

    !> Start Dired in a directory inside the current project.
    !> The current buffer's `default_directory' is available as part of
    !> "future history".  Port of project.el:project-find-dir.
    recursive subroutine find_project_dir(status)
        type(status_type), intent(out) :: status
        class(project_type), allocatable :: project
        type(string_type), allocatable :: all_files(:), all_dirs(:), defaults(:)
        character(len=:), allocatable :: dir, saved_dir, d
        logical :: saved_relative, saved_ignore_case
        integer :: k

        call get_current_project(project, status, maybe_prompt=.true.)
        if (.not. status%ok()) return
        saved_relative = project_files_relative_names
        project_files_relative_names = .true.
        call project%list_files(all_files, status)
        project_files_relative_names = saved_relative
        if (.not. status%ok()) return
        saved_ignore_case = completion_ignore_case%value
        completion_ignore_case = read_file_name_completion_ignore_case
        ! FIXME: This misses directories without any files directly inside.
        allocate (all_dirs(0))
        call append_string(all_dirs, "./")

        do k = 1, size(all_files)
            d = file_name_directory(all_files(k)%text)
            ! Some completion UIs show duplicates.
            if (len(d) > 0 .and. .not. has_string(all_dirs(2:), d)) call append_string(all_dirs, d)
        end do

        saved_dir = default_directory()
        call set_default_directory(file_name_as_directory(project%root()))
        allocate (defaults(0))
        call append_string(defaults, find_default_from(default_directory(), project))
        call read_project_file_name(project, "Dired", all_dirs, dir, status, "file_name_history", defaults)
        call set_default_directory(saved_dir)
        completion_ignore_case = saved_ignore_case
        if (.not. status%ok()) return
        call dired(dir, status)
    end subroutine find_project_dir

    ! ------------------------------------------------------------------
    ! Commands in the project root
    ! ------------------------------------------------------------------

    !> The root of the current project (prompting for one).  Not a port.
    recursive subroutine current_root(root, status)
        character(len=:), allocatable, intent(out) :: root
        type(status_type), intent(out) :: status
        class(project_type), allocatable :: pr

        root = ""
        call get_current_project(pr, status, maybe_prompt=.true.)
        if (status%ok()) root = pr%root()
    end subroutine current_root

    !> Execute COMMAND interactively with `default_directory' bound to the
    !> root of the current project.  Not a port.
    recursive subroutine execute_in_root(command, status)
        type(command_type), intent(in), target :: command
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: root, saved

        call current_root(root, status)
        if (.not. status%ok()) return
        saved = default_directory()
        call set_default_directory(root)
        call execute_interactively(command, status)
        call restore_directory(saved)
    end subroutine execute_in_root

    !> Make SAVED the `default_directory' of the buffer that had it bound
    !> if it is still current (let's unbinding).  Not a port.
    subroutine restore_directory(saved)
        character(len=*), intent(in) :: saved

        call set_default_directory(saved)
    end subroutine restore_directory

    !> Start Dired in the current project's root.  Port of
    !> project.el:project-dired.
    recursive subroutine dired_in_project(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: root

        call current_root(root, status)
        if (.not. status%ok()) return
        call dired(root, status)
    end subroutine dired_in_project

    !> Run VC-Dir in the current project's root.  Port of
    !> project.el:project-vc-dir.
    recursive subroutine vc_dir_in_project(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: root

        call current_root(root, status)
        if (.not. status%ok()) return
        call vc_dir(root, status)
    end subroutine vc_dir_in_project

    !> Start an inferior shell in the current project's root directory.
    !> If a buffer already exists for running a shell in the project's
    !> root, switch to it.  Otherwise, create a new shell buffer.
    !> With \[universal_argument] prefix arg, create a new inferior shell
    !> buffer even if one already exists.
    !> With numeric prefix arg, switch to the session with that number, or
    !> create it if it doesn't already exist.  Port of
    !> project.el:project-shell.
    recursive subroutine shell_in_project(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: root, saved, base_name, shell_buffer_name
        type(handle_type) :: shell_buffer, result
        character(len=24) :: number

        call current_root(root, status)
        if (.not. status%ok()) return
        saved = default_directory()
        call set_default_directory(root)
        base_name = project_prefixed_buffer_name("shell")

        if (current_prefix_arg%kind == PREFIX_NUMBER) then
            write (number, "(i0)") prefix_numeric_value(current_prefix_arg)
            shell_buffer_name = base_name//"<"//trim(number)//">"
        else if (current_prefix_arg%kind /= PREFIX_NONE) then
            shell_buffer_name = generate_new_buffer_name(base_name)
        else
            shell_buffer_name = base_name
        end if

        shell_buffer = get_buffer(shell_buffer_name)

        if (is_buffer_live(shell_buffer) .and. current_prefix_arg%kind == PREFIX_NONE) then
            if (comint_check_process(shell_buffer)) then
                call pop_to_buffer_same_window(shell_buffer, status)
            else
                call shell(result, status, buffer_name=buffers(shell_buffer%index)%name)
            end if
        else
            call shell(result, status, buffer_name=shell_buffer_name)
        end if

        call restore_directory(saved)
    end subroutine shell_in_project

    !> Run `async_shell_command' in the current project's root directory.
    !> Port of project.el:project-async-shell-command.
    recursive subroutine async_shell_command_in_project(status)
        type(status_type), intent(out) :: status

        call execute_in_root(commands%async_shell_command, status)
    end subroutine async_shell_command_in_project

    !> Run `shell_command' in the current project's root directory.
    !> Port of project.el:project-shell-command.
    recursive subroutine shell_command_in_project(status)
        type(status_type), intent(out) :: status

        call execute_in_root(commands%shell_command, status)
    end subroutine shell_command_in_project

    !> Execute an extended command in project root.  Port of
    !> project.el:project-execute-extended-command.
    recursive subroutine execute_extended_command_in_project(status)
        type(status_type), intent(out) :: status

        call execute_in_root(commands%execute_extended_command, status)
    end subroutine execute_extended_command_in_project

    !> The name of a buffer of MODE for the current project: "*NAME-MODE*",
    !> NAME the project's name (or that of `default_directory' outside of
    !> projects).  Port of project.el:project-prefixed-buffer-name.
    recursive function project_prefixed_buffer_name(mode) result(name)
        use character_mod, only: downcase_ascii
        character(len=*), intent(in) :: mode
        character(len=:), allocatable :: name
        class(project_type), allocatable :: proj
        type(status_type) :: st

        call get_current_project(proj, st)

        if (allocated(proj)) then
            name = proj%name()
        else
            name = file_name_nondirectory(directory_file_name(default_directory()))
        end if

        name = "*"//name//"-"//downcase_ascii(mode)//"*"
    end function project_prefixed_buffer_name

    !> Run `compile' in the project root.  Port of
    !> project.el:project-compile (`project_compilation_buffer_name_function'
    !> is Deferred with `compilation_buffer_name_function').
    recursive subroutine compile_in_project(status)
        type(status_type), intent(out) :: status

        call execute_in_root(commands%compile, status)
    end subroutine compile_in_project

    !> Run `recompile' in the project root with an appropriate buffer.
    !> EDIT_COMMAND (the prefix argument) lets the user edit the command.
    !> Port of project.el:project-recompile.
    recursive subroutine recompile_in_project(status, edit_command)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: edit_command
        character(len=:), allocatable :: root, saved, saved_compilation_directory
        logical :: had

        call current_root(root, status)
        if (.not. status%ok()) return
        saved = default_directory()
        call set_default_directory(root)
        ! The former overrides the latter in `recompile'.
        had = allocated(compilation_directory%value)
        if (had) saved_compilation_directory = compilation_directory%value
        compilation_directory = root
        call recompile(status, edit_command)

        if (had) then
            compilation_directory = saved_compilation_directory
        else
            call compilation_directory%clear()
        end if

        call restore_directory(saved)
    end subroutine recompile_in_project

    ! The command.
    recursive subroutine cmd_recompile_in_project(status)
        type(status_type), intent(out) :: status

        call recompile_in_project(status, current_prefix_arg%kind /= PREFIX_NONE)
    end subroutine cmd_recompile_in_project

    ! ------------------------------------------------------------------
    ! Project buffers
    ! ------------------------------------------------------------------

    !> Read a buffer of the current project, offering the other buffer
    !> as the default; a new name makes a buffer in the project root.
    !> Port of project.el:project--read-project-buffer (uniquify's names
    !> are Deferred).
    recursive subroutine read_project_buffer(buffer, status)
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status
        class(project_type), allocatable :: pr
        type(handle_type), allocatable :: bufs(:)
        type(handle_type) :: other
        type(string_type), allocatable :: names(:)
        character(len=:), allocatable :: other_name, result, saved_dir
        logical :: saved_ignore_case
        integer :: k

        buffer = null_handle()
        call get_current_project(pr, status, maybe_prompt=.true.)
        if (.not. status%ok()) return
        other = other_buffer(current_buffer())
        bufs = pr%buffers()
        allocate (names(0))
        other_name = ""

        do k = 1, size(bufs)
            if (is_project_buffer_matching(bufs(k), project_ignore_buffer_conditions)) cycle
            call append_string(names, buffers(bufs(k)%index)%name)

            if (bufs(k)%index == other%index .and. bufs(k)%generation == other%generation) then
                other_name = buffers(bufs(k)%index)%name
            end if
        end do

        saved_ignore_case = completion_ignore_case%value
        completion_ignore_case = read_buffer_completion_ignore_case

        ! Forgo the use of `read_buffer_function' in favor of showing
        ! shorter buffer names.  (Hiding the internal buffers from the
        ! completions of an empty input is Deferred.)
        if (len(other_name) > 0) then
            call completing_read(format_prompt("Switch to buffer", other_name), &
                                 completion_list_table(string_list(names), category="project-buffer"), result, status, &
                                 default=other_name)
        else
            call completing_read(format_prompt("Switch to buffer", ""), &
                                 completion_list_table(string_list(names), category="project-buffer"), result, status)
        end if

        completion_ignore_case = saved_ignore_case
        if (.not. status%ok()) return
        buffer = get_buffer(result)
        if (is_buffer_live(buffer)) return

        ! XXX: This check hardcodes the default buffer-belonging relation
        ! which `project_buffers' is allowed to override.
        if (is_file_in_directory(default_directory(), pr%root())) then
            call get_or_make_buffer(result, buffer, status)
        else
            saved_dir = default_directory()
            call set_default_directory(pr%root())
            call get_or_make_buffer(result, buffer, status)
            call set_default_directory(saved_dir)
            if (status%ok()) buffers(buffer%index)%directory = pr%root()
        end if
    end subroutine read_project_buffer

    !> Display buffer BUFFER in the selected window.
    !> When called interactively, prompts for a buffer belonging to the
    !> current project.  Two buffers belong to the same project if their
    !> projects, as reported by `get_current_project' in each buffer, are
    !> identical.  Port of project.el:project-switch-to-buffer.
    recursive subroutine switch_to_project_buffer(buffer, status)
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status

        call switch_to_buffer(buffers(buffer%index)%name, status)
    end subroutine switch_to_project_buffer

    ! The command.
    recursive subroutine cmd_switch_to_project_buffer(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer

        call read_project_buffer(buffer, status)
        if (.not. status%ok()) return
        call switch_to_project_buffer(buffer, status)
    end subroutine cmd_switch_to_project_buffer

    !> Display BUFFER in some window, without selecting it.
    !> When called interactively, prompts for a buffer belonging to the
    !> current project.  Two buffers belong to the same project if their
    !> projects, as reported by `get_current_project' in each buffer, are
    !> identical.
    !>
    !> This function uses `display_buffer' as a subroutine, which see for
    !> how it is determined where the buffer will be displayed.  Port of
    !> project.el:project-display-buffer.
    recursive subroutine display_project_buffer(buffer, status)
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        type(handle_type) :: window

        call display_buffer(buffer, window, status)
    end subroutine display_project_buffer

    ! The command.
    recursive subroutine cmd_display_project_buffer(status)
        type(status_type), intent(out) :: status
        type(handle_type) :: buffer

        call read_project_buffer(buffer, status)
        if (.not. status%ok()) return
        call display_project_buffer(buffer, status)
    end subroutine cmd_display_project_buffer

    !> Display a list of project buffers.
    !> The list is displayed in a buffer named "*Buffer List*".
    !>
    !> By default, all project buffers are listed except those whose names
    !> start with a space (which are for internal use).  With prefix
    !> argument ARG, show only buffers that are visiting files.  Port of
    !> project.el:project-list-buffers.
    recursive subroutine list_project_buffers(status, arg)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: arg
        class(project_type), allocatable :: pr
        logical :: files_only

        call get_current_project(pr, status, maybe_prompt=.true.)
        if (.not. status%ok()) return
        files_only = .false.
        if (present(arg)) files_only = arg

        if (associated(project_buffers_viewer)) then
            call project_buffers_viewer(pr, files_only, status)
        else
            call list_project_buffers_in_buffer_menu(pr, files_only, status)
        end if
    end subroutine list_project_buffers

    ! The command.
    recursive subroutine cmd_list_project_buffers(status)
        type(status_type), intent(out) :: status

        call list_project_buffers(status, current_prefix_arg%kind /= PREFIX_NONE)
    end subroutine cmd_list_project_buffers

    !> List buffers for PROJECT in Buffer-menu mode.
    !> If FILES_ONLY is .true., only show the file-visiting buffers.
    !> Port of project.el:project-list-buffers-buffer-menu.
    recursive subroutine list_project_buffers_in_buffer_menu(project, files_only, status)
        class(project_type), intent(in) :: project
        logical, intent(in) :: files_only
        type(status_type), intent(out) :: status
        type(handle_type) :: menu, window
        type(buffer_menu_project_type) :: entry
        integer :: k

        ! The menu's buffer, to know the project of its listing.
        call get_or_make_buffer("*Buffer List*", menu, status)
        if (.not. status%ok()) return
        if (.not. allocated(buffer_menu_projects)) allocate (buffer_menu_projects(0))
        entry%serial = buffers(menu%index)%serial
        allocate (entry%project, source=project)
        k = findloc(buffer_menu_projects%serial, entry%serial, dim=1)

        if (k > 0) then
            buffer_menu_projects(k) = entry
        else
            buffer_menu_projects = [buffer_menu_projects, entry]
        end if

        call list_buffers_noselect(menu, status, files_only=files_only, buffer_list_function=menu_project_buffers)
        if (.not. status%ok()) return
        call display_buffer(menu, window, status)
    end subroutine list_project_buffers_in_buffer_menu

    !> The buffer-list function of a Buffer Menu listing project buffers:
    !> the buffers of its project but the menu itself.  Port of
    !> project.el:project-list-buffers-buffer-menu (its buffer-list
    !> function; the menu filters the internal and non-file buffers).
    subroutine menu_project_buffers(bufs)
        type(handle_type), allocatable, intent(out) :: bufs(:)
        type(handle_type), allocatable :: all(:)
        integer :: k, m

        allocate (bufs(0))
        if (.not. allocated(buffer_menu_projects)) return
        m = findloc(buffer_menu_projects%serial, buffers(current)%serial, dim=1)
        if (m == 0) return
        all = buffer_menu_projects(m)%project%buffers()

        do k = 1, size(all)
            if (all(k)%index /= current) bufs = [bufs, all(k)]
        end do
    end subroutine menu_project_buffers

    !> The buffers of project PR to close: those matching
    !> `project_close_buffer_conditions'.  Port of
    !> project.el:project--buffers-to-kill.
    function project_buffers_to_close(pr) result(bufs)
        class(project_type), intent(in) :: pr
        type(handle_type), allocatable :: bufs(:)
        type(handle_type), allocatable :: all(:)
        integer :: k

        all = pr%buffers()
        allocate (bufs(0))

        do k = 1, size(all)
            if (is_project_buffer_matching(all(k), project_close_buffer_conditions)) bufs = [all(k), bufs]
        end do
    end function project_buffers_to_close

    !> Close the buffers belonging to the current project.
    !> Two buffers belong to the same project if their projects, as
    !> reported by `get_current_project' in each buffer, are identical.
    !> Only the buffers that match a condition in
    !> `project_close_buffer_conditions' will be closed.  If NO_CONFIRM is
    !> .true., the command will not ask the user for confirmation.
    !> NO_CONFIRM is always .false. when the command is invoked
    !> interactively.
    !>
    !> If PROJECT is present, close buffers for that project instead.
    !> Port of project.el:project-kill-buffers.
    recursive subroutine close_project_buffers(status, no_confirm, project)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_confirm
        class(project_type), intent(in), optional :: project
        class(project_type), allocatable :: pr
        type(handle_type), allocatable :: bufs(:)
        logical :: confirmed, quiet
        integer :: k
        character(len=24) :: number

        if (present(project)) then
            allocate (pr, source=project)
        else
            call get_current_project(pr, status, maybe_prompt=.true.)
            if (.not. status%ok()) return
        end if

        status = status_type()
        bufs = project_buffers_to_close(pr)
        quiet = .false.
        if (present(no_confirm)) quiet = no_confirm

        if (quiet) then
            confirmed = .true.
        else if (size(bufs) == 0) then
            call message("No buffers to kill")
            return
        else
            write (number, "(i0)") size(bufs)
            call read_yes_or_no("Kill "//trim(number)//" buffers in "//pr%name()//"? ", confirmed, status)
            if (.not. status%ok()) return
        end if

        if (.not. confirmed) return

        do k = 1, size(bufs)
            if (is_buffer_live(bufs(k))) call close_buffer(bufs(k), status)
            if (.not. status%ok()) return
        end do
    end subroutine close_project_buffers

    ! The command.
    recursive subroutine cmd_close_project_buffers(status)
        type(status_type), intent(out) :: status

        call close_project_buffers(status)
    end subroutine cmd_close_project_buffers

    !> Like `save_some_buffers', but only for this project's buffers.  ARG
    !> as for `save_some_buffers'.  Port of
    !> project.el:project-save-some-buffers.
    recursive subroutine save_some_project_buffers(arg, status)
        logical, intent(in) :: arg
        type(status_type), intent(out) :: status
        class(project_type), allocatable :: pr
        character(len=:), allocatable :: saved

        if (allocated(save_root)) saved = save_root
        call get_current_project(pr, status)
        if (.not. status%ok()) return

        ! save-some-buffers-root: the buffers in the project root, else in
        ! `default_directory'.
        if (allocated(pr)) then
            save_root = pr%root()
        else
            save_root = default_directory()
        end if

        call save_some_buffers(arg, .true., status, is_buffer_under_save_root)

        if (allocated(saved)) then
            save_root = saved
        else
            deallocate (save_root)
        end if
    end subroutine save_some_project_buffers

    ! The command.
    recursive subroutine cmd_save_some_project_buffers(status)
        type(status_type), intent(out) :: status

        call save_some_project_buffers(current_prefix_arg%kind /= PREFIX_NONE, status)
    end subroutine cmd_save_some_project_buffers

    !> Whether the current buffer's `default_directory' is in the root
    !> `save_some_project_buffers' saves.  Port of
    !> files.el:save-some-buffers-root (its test).
    logical function is_buffer_under_save_root()
        is_buffer_under_save_root = .false.
        if (allocated(save_root)) is_buffer_under_save_root = is_file_in_directory(default_directory(), save_root)
    end function is_buffer_under_save_root

    ! ------------------------------------------------------------------
    ! Running any command; switching projects
    ! ------------------------------------------------------------------

    !> Run the next command in the current project.
    !>
    !> If the command is one of the project commands (`project_aware' set),
    !> it gets passed the project to use with the variable
    !> `project_current_directory_override'.  Otherwise,
    !> `default_directory' is temporarily set to the current project's root.
    !>
    !> If OVERRIDING_MAP is present, it will be used as
    !> `overriding_terminal_local_map' to provide shorter bindings from that
    !> map which will take priority over the global ones.  PROMPT_BEFORE
    !> and PROMPT_AFTER surround the root in the prompt (default
    !> "[execute in " and "]:").  Port of project.el:project-any-command.
    recursive subroutine execute_any_command_in_project(status, overriding_map, prompt_before, prompt_after)
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: overriding_map
        character(len=*), intent(in), optional :: prompt_before, prompt_after
        class(project_type), allocatable :: pr
        type(value_type), allocatable :: keys(:)
        type(value_type) :: binding
        type(handle_type) :: saved_map
        type(command_type), pointer :: command
        character(len=:), allocatable :: root, before, after, saved_dir, saved_override
        logical :: eof, had_override

        call get_current_project(pr, status, maybe_prompt=.true.)
        if (.not. status%ok()) return
        root = pr%root()
        before = "[execute in "
        if (present(prompt_before)) before = prompt_before
        after = "]:"
        if (present(prompt_after)) after = prompt_after
        saved_map = overriding_terminal_local_map
        if (present(overriding_map)) overriding_terminal_local_map = overriding_map
        call read_key_sequence(keys, binding, eof, status, prompt=before//root//after)
        overriding_terminal_local_map = saved_map
        if (.not. status%ok()) return
        command => command_of(binding)
        if (.not. associated(command)) return

        if (command%project_aware) then
            ! (let ((project-current-directory-override root)) ...)
            had_override = allocated(project_current_directory_override%value)
            if (had_override) saved_override = project_current_directory_override%value
            project_current_directory_override = root
            call execute_interactively(command, status)

            if (had_override) then
                project_current_directory_override = saved_override
            else
                call project_current_directory_override%clear()
            end if
        else
            saved_dir = default_directory()
            call set_default_directory(root)
            call execute_interactively(command, status)
            call restore_directory(saved_dir)
        end if
    end subroutine execute_any_command_in_project

    ! The command.
    recursive subroutine cmd_execute_any_command_in_project(status)
        type(status_type), intent(out) :: status

        call execute_any_command_in_project(status)
    end subroutine cmd_execute_any_command_in_project

    !> Run the next command in the current project.
    !> Works like `execute_any_command_in_project', but also mixes in the
    !> shorter bindings from `project_prefix_map'.  Port of
    !> project.el:project-prefix-or-any-command.
    recursive subroutine execute_prefix_or_any_command_in_project(status)
        type(status_type), intent(out) :: status

        call execute_any_command_in_project(status, project_prefix_map, "[execute in ", "]:")
    end subroutine execute_prefix_or_any_command_in_project

    !> The keys of `project_prefix_map' as a prompt for switching projects.
    !> Port of project.el:project--keymap-prompt.
    function keymap_prompt() result(prompt)
        use keymap_mod, only: keymap_entries
        character(len=:), allocatable :: prompt
        type(value_type), allocatable :: events(:), bindings(:), range_bindings(:)
        integer(int64), allocatable :: range_from(:), range_to(:)
        integer :: k

        prompt = ""
        call keymap_entries(project_prefix_map, events, bindings, range_from, range_to, range_bindings)

        do k = size(events), 1, -1
            if (vtag(events(k)) /= V_STRING .and. is_character_event(events(k))) then
                if (len(prompt) > 0) prompt = prompt//" "
                prompt = prompt//key_description(events(k:k))
            end if
        end do
    end function keymap_prompt

    !> Whether EVENT is a character.  Not a port.
    logical function is_character_event(event)
        use value_mod, only: V_INTEGER, xfixnum
        type(value_type), intent(in) :: event

        is_character_event = .false.
        if (vtag(event) == V_INTEGER) is_character_event = xfixnum(event) >= 0 .and. xfixnum(event) < 4194304
    end function is_character_event

    !> The dispatch menu of `project_switch_commands' as a prompt for
    !> switching projects: each entry's key and label.  Port of
    !> project.el:project--menu-prompt (the faces of
    !> `project_key_prompt_style' are Deferred).
    function menu_prompt() result(prompt)
        character(len=:), allocatable :: prompt
        integer :: k

        prompt = ""
        if (.not. allocated(project_switch_commands)) return

        do k = 1, size(project_switch_commands)
            if (k > 1) prompt = prompt//"  "
            prompt = prompt//switch_command_key(project_switch_commands(k))//" "//trim(project_switch_commands(k)%label)
        end do
    end function menu_prompt

    !> The key of the switch command ENTRY, as text: its own key, else its
    !> first key in `project_prefix_map'.  Not a port.
    function switch_command_key(entry) result(key)
        type(project_switch_command_type), intent(in) :: entry
        character(len=:), allocatable :: key
        type(value_type) :: found

        key = ""

        if (entry%key /= " ") then
            key = entry%key
            return
        end if

        if (.not. associated(entry%command)) return
        call where_is_internal(value_type(name=trim(entry%command%name)), found, firstonly=.true., &
                               keymap=project_prefix_map)
        if (allocated(found%a)) key = key_sequence_description(found)
    end function switch_command_key

    !> The description of the key sequence KEYS (an array value).
    !> Not a port.
    function key_sequence_description(keys) result(s)
        use value_mod, only: list_elements
        type(value_type), intent(in) :: keys
        character(len=:), allocatable :: s
        type(value_type), allocatable :: events(:)

        call list_elements(keys, events)
        s = key_description(events)
    end function key_sequence_description

    !> Read the command to run in project DIR from the dispatch menu.
    !> Port of project.el:project--switch-project-command.
    recursive subroutine switch_project_command(command, status, dir)
        type(command_type), pointer, intent(out) :: command
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: dir
        type(handle_type) :: commands_map, saved_map
        type(value_type), allocatable :: choice(:)
        type(value_type) :: binding, found, global
        type(command_type), pointer :: global_command
        character(len=:), allocatable :: prompt, head
        logical :: eof, listed, have_choice
        integer :: k

        command => null()
        call make_sparse_keymap(commands_map)
        call set_keymap_parent(commands_map, project_prefix_map, status)

        if (.not. status%ok()) then
            call destroy_temp_map(commands_map)
            return
        end if

        if (allocated(project_switch_commands)) then
            do k = 1, size(project_switch_commands)
                if (.not. associated(project_switch_commands(k)%command)) cycle
                if (project_switch_commands(k)%key == " ") cycle
                call keymap_set(commands_map, project_switch_commands(k)%key, trim(project_switch_commands(k)%command%name), &
                                status)

                if (.not. status%ok()) then
                    call destroy_temp_map(commands_map)
                    return
                end if
            end do
        end if

        have_choice = .false.

        do while (.not. associated(command))
            if (project_switch_use_entire_map) then
                prompt = keymap_prompt()
            else
                prompt = menu_prompt()
            end if

            if (have_choice) prompt = prompt//" Unrecognized input: "//key_description(choice)

            if (present(dir)) then
                head = "Command in `"//dir//"': "
            else
                head = "Command: "
            end if

            saved_map = overriding_terminal_local_map
            overriding_terminal_local_map = commands_map
            call read_key_sequence(choice, binding, eof, status, prompt=head//prompt)
            overriding_terminal_local_map = saved_map
            if (.not. status%ok()) exit
            if (eof) exit
            have_choice = .true.
            call lookup_key(make_keymap_ref(commands_map), choice, found, status)
            if (.not. status%ok()) exit
            command => command_of(found)

            if (associated(command)) then
                listed = project_switch_use_entire_map

                if (.not. listed .and. allocated(project_switch_commands)) then
                    do k = 1, size(project_switch_commands)
                        if (associated(project_switch_commands(k)%command, command)) listed = .true.
                    end do
                end if

                if (.not. listed) command => null()
            end if

            call lookup_key(make_keymap_ref(current_global_map()), choice, global, status)
            if (.not. status%ok()) exit
            global_command => command_of(global)

            if (associated(global_command)) then
                ! (keyboard-escape-quit is not ported.)
                if (associated(global_command, commands%keyboard_quit)) then
                    call execute_interactively(global_command, status)
                    if (.not. status%ok()) exit
                end if
            end if
        end do

        call message("")
        call destroy_temp_map(commands_map)
    end subroutine switch_project_command

    !> Destroy the temporary keymap MAP.  Not a port.
    subroutine destroy_temp_map(map)
        use keymap_mod, only: destroy_keymap
        type(handle_type), intent(inout) :: map
        type(status_type) :: st

        call destroy_keymap(map, st)
    end subroutine destroy_temp_map

    !> "Switch" to another project by running a command.
    !> The available commands are presented as a dispatch menu made from
    !> `project_switch_commands'.
    !>
    !> When called in a program, it will use the project corresponding to
    !> directory DIR.  Port of project.el:project-switch-project.
    recursive subroutine switch_project(dir, status)
        character(len=*), intent(in) :: dir
        type(status_type), intent(out) :: status
        class(project_type), allocatable :: pr
        type(command_type), pointer :: command
        type(handle_type) :: buffer

        call get_current_project(pr, status, maybe_prompt=.true., directory=dir)
        if (.not. status%ok()) return
        call remember_project(pr, status)
        if (.not. status%ok()) return
        call switch_project_command(command, status, dir)
        if (.not. status%ok()) return
        if (.not. associated(command)) return
        buffer = current_buffer()
        call project_current_directory_override%set_local(dir)
        call execute_interactively(command, status)
        if (is_buffer_live(buffer)) call set_buffer_keeping(buffer)

    contains

        ! (with-current-buffer buffer (kill-local-variable ...)).
        subroutine set_buffer_keeping(b)
            type(handle_type), intent(in) :: b
            type(handle_type) :: old
            type(status_type) :: st2

            old = current_buffer()
            call set_buffer(b, st2)
            call project_current_directory_override%clear_local()
            call set_buffer(old, st2)
        end subroutine set_buffer_keeping
    end subroutine switch_project

    ! The command.
    recursive subroutine cmd_switch_project(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: dir

        call execute_prompter(dir, status)
        if (.not. status%ok()) return
        call switch_project(dir, status)
    end subroutine cmd_switch_project

    ! ------------------------------------------------------------------
    ! Setup
    ! ------------------------------------------------------------------

    !> Set up projects: the defaults of the settings, the commands and
    !> `project_prefix_map' on C-x p in CTL_X_MAP.  Not a port.
    subroutine init_project(ctl_x_map)
        use value_mod, only: value_type
        type(handle_type), intent(in) :: ctl_x_map
        type(value_type) :: key(1)
        type(status_type) :: st

        if (.not. allocated(project_find_functions%entries)) call project_find_functions%append(try_vc_project)
        if (.not. associated(project_prompter)) project_prompter => read_project_dir
        if (.not. associated(project_read_file_name_function)) project_read_file_name_function => read_project_file_relative
        if (.not. associated(project_buffers_viewer)) project_buffers_viewer => list_project_buffers_in_buffer_menu
        if (.not. allocated(project_vc_ignores)) allocate (character(len=0) :: project_vc_ignores(0))
        if (.not. allocated(project_vc_extra_root_markers)) allocate (character(len=0) :: project_vc_extra_root_markers(0))
        if (.not. allocated(project_vc_name)) project_vc_name = ""
        if (.not. allocated(project_list_file)) project_list_file = state_directory()//"projects"
        if (.not. allocated(project_list_exclude)) allocate (project_list_exclude(0))

        if (.not. associated(project_prune_zombie_projects%prompt)) then
            project_prune_zombie_projects%prompt => is_local_project
        end if

        if (.not. allocated(project_vc_backend_markers)) then
            project_vc_backend_markers = [project_vc_backend_marker_type("Git", ".git"), &
                                          project_vc_backend_marker_type("Hg", ".hg"), &
                                          project_vc_backend_marker_type("Bzr", ".bzr"), &
                                          project_vc_backend_marker_type("SVN", ".svn"), &
                                          project_vc_backend_marker_type("DARCS", "_darcs"), &
                                          project_vc_backend_marker_type("Fossil", ".fslckout"), &
                                          project_vc_backend_marker_type("Got", ".got")]
        end if

        if (.not. allocated(project_close_buffer_conditions%conditions)) then
            ! All file-visiting buffers are included.
            call project_close_buffer_conditions%append(project_buffer_test(is_file_buffer))
            ! Most of temp and logging buffers (aside from hidden ones):
            call project_close_buffer_conditions%append(project_and(project_major_mode("fundamental_mode"), &
                                                                    project_buffer_name_regexp("\`[^ ]")))
            ! non-text buffer such as xref, occur, vc, log, ...
            call project_close_buffer_conditions%append(project_and(project_derived_mode("special_mode"), &
                                                                    project_not(project_major_mode("help_mode")), &
                                                                    project_not(project_derived_mode("gnus_mode"))))
            call project_close_buffer_conditions%append(project_derived_mode("compilation_mode"))
            call project_close_buffer_conditions%append(project_derived_mode("dired_mode"))
            call project_close_buffer_conditions%append(project_derived_mode("diff_mode"))
            call project_close_buffer_conditions%append(project_derived_mode("comint_mode"))
            call project_close_buffer_conditions%append(project_derived_mode("eshell_mode"))
            call project_close_buffer_conditions%append(project_derived_mode("change_log_mode"))
        end if

        if (.not. allocated(project_ignore_buffer_conditions%conditions)) call project_ignore_buffer_conditions%clear()

        if (.not. allocated(project_switch_commands)) then
            allocate (project_switch_commands(4))
            project_switch_commands(1)%command => commands%find_project_file
            project_switch_commands(1)%label = "Find file"
            project_switch_commands(2)%command => commands%find_project_dir
            project_switch_commands(2)%label = "Find directory"
            project_switch_commands(3)%command => commands%vc_dir_in_project
            project_switch_commands(3)%label = "VC-Dir"
            project_switch_commands(4)%command => commands%execute_any_command_in_project
            project_switch_commands(4)%label = "Other"
        end if

        call forward_setting(project_current_directory_override)
        call forward_setting(project_dir_history)
        call forward_setting(project_name_history)
        if (.not. allocated(vc_cache)) allocate (vc_cache(0))
        commands%find_project_file%execute => cmd_find_project_file
        commands%find_project_or_external_file%execute => cmd_find_project_or_external_file
        commands%find_file_in_project_root%execute => cmd_find_file_in_project_root
        commands%find_project_dir%execute => find_project_dir
        commands%dired_in_project%execute => dired_in_project
        commands%vc_dir_in_project%execute => vc_dir_in_project
        commands%shell_in_project%execute => shell_in_project
        commands%async_shell_command_in_project%execute => async_shell_command_in_project
        commands%shell_command_in_project%execute => shell_command_in_project
        commands%compile_in_project%execute => compile_in_project
        commands%recompile_in_project%execute => cmd_recompile_in_project
        commands%switch_to_project_buffer%execute => cmd_switch_to_project_buffer
        commands%display_project_buffer%execute => cmd_display_project_buffer
        commands%list_project_buffers%execute => cmd_list_project_buffers
        commands%close_project_buffers%execute => cmd_close_project_buffers
        commands%save_some_project_buffers%execute => cmd_save_some_project_buffers
        commands%remember_project%execute => cmd_remember_project
        commands%forget_project%execute => cmd_forget_project
        commands%execute_extended_command_in_project%execute => execute_extended_command_in_project
        commands%execute_any_command_in_project%execute => cmd_execute_any_command_in_project
        commands%execute_prefix_or_any_command_in_project%execute => execute_prefix_or_any_command_in_project
        commands%remember_projects_under%execute => cmd_remember_projects_under
        commands%forget_zombie_projects%execute => cmd_forget_zombie_projects
        commands%forget_projects_under%execute => cmd_forget_projects_under
        commands%switch_project%execute => cmd_switch_project
        ! The commands that take the project from
        ! `project_current_directory_override' (upstream's "project-"
        ! names and the `project-aware' property).
        commands%find_project_file%project_aware = .true.
        commands%find_project_or_external_file%project_aware = .true.
        commands%find_file_in_project_root%project_aware = .true.
        commands%find_project_dir%project_aware = .true.
        commands%dired_in_project%project_aware = .true.
        commands%vc_dir_in_project%project_aware = .true.
        commands%shell_in_project%project_aware = .true.
        commands%async_shell_command_in_project%project_aware = .true.
        commands%shell_command_in_project%project_aware = .true.
        commands%compile_in_project%project_aware = .true.
        commands%recompile_in_project%project_aware = .true.
        commands%switch_to_project_buffer%project_aware = .true.
        commands%display_project_buffer%project_aware = .true.
        commands%list_project_buffers%project_aware = .true.
        commands%close_project_buffers%project_aware = .true.
        commands%save_some_project_buffers%project_aware = .true.
        commands%remember_project%project_aware = .true.
        commands%forget_project%project_aware = .true.
        commands%execute_extended_command_in_project%project_aware = .true.
        commands%execute_any_command_in_project%project_aware = .true.
        commands%execute_prefix_or_any_command_in_project%project_aware = .true.
        commands%switch_project%project_aware = .true.
        call make_sparse_keymap(project_prefix_map)
        call keymap_set(project_prefix_map, "!", "shell_command_in_project", st)
        call keymap_set(project_prefix_map, "&", "async_shell_command_in_project", st)
        call keymap_set(project_prefix_map, "f", "find_project_file", st)
        call keymap_set(project_prefix_map, "F", "find_project_or_external_file", st)
        call keymap_set(project_prefix_map, "b", "switch_to_project_buffer", st)
        call keymap_set(project_prefix_map, "s", "shell_in_project", st)
        call keymap_set(project_prefix_map, "d", "find_project_dir", st)
        call keymap_set(project_prefix_map, "D", "dired_in_project", st)
        call keymap_set(project_prefix_map, "v", "vc_dir_in_project", st)
        call keymap_set(project_prefix_map, "c", "compile_in_project", st)
        call keymap_set(project_prefix_map, "k", "close_project_buffers", st)
        call keymap_set(project_prefix_map, "p", "switch_project", st)
        call keymap_set(project_prefix_map, "x", "execute_extended_command_in_project", st)
        call keymap_set(project_prefix_map, "o", "execute_any_command_in_project", st)
        call keymap_set(project_prefix_map, "C-b", "list_project_buffers", st)
        call keymap_set(project_prefix_map, "C-x s", "save_some_project_buffers", st)
        key(1) = value_type(int(iachar("p"), int64))
        call define_key(ctl_x_map, key, make_keymap_ref(project_prefix_map), st)
    end subroutine init_project
end module project_mod
