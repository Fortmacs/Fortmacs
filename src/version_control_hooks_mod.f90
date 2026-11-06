! version_control_hooks_mod.f90 -- resident support for version-control.
! Ported from GNU Emacs 31.1 lisp/vc/vc-hooks.el.
!
! Copyright (C) 1992-1996, 1998-2026 Free Software Foundation, Inc.
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

!> The resident part of VERSION_CONTROL: the backends (a `version_control_backend_type' each, with
!> its operations as procedure pointers, registered by the backend
!> modules), the per-file properties VERSION_CONTROL caches (backend, state, working
!> revision, checkout time), finding a file's backend, state and
!> revision, `version_control_mode' in the mode line, `version_control_refresh_state' on
!> `find_file_hook', the saving hooks of `basic_save_buffer', version
!> backups, and `version_control_prefix_map' on C-x v.
!>
!> Deferred: the backends other than Git, SUBVERSION and Hg (`version_control_handled_backends' still
!> names them; they are skipped as if none of their files were found),
!> file name handlers, the vc-*-state faces on the mode-line string (it
!> has no text properties: faces, help-echo and the mouse menu
!> `version_control_mode_line_map'), the menus (`version_control_menu_map'), `version_control_auto_revert_mode'
!> (Auto-Revert mode), the *vc-dir* resynchronization after saving and
!> reverting (VC-Dir), `version_control_use_incoming_outgoing_prefixes' and the
!> C-x v keys of the commands not ported yet.
module version_control_hooks_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    use commands_mod, only: commands
    use error_mod, only: status_type, set_error, format_error, GENERIC_ERROR_CODE, VERSION_CONTROL_NOT_SUPPORTED_CODE
    use handle_mod, only: handle_type
    use fortmacs_widget_for_object_collections_mod, only: fortmacs_widget_for_object_collections_part_type
    use value_mod, only: make_keymap_reference, value_type
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, close_buffer, get_file_buffer, is_buffer_live
    use file_input_output_mod, only: expand_file_name, file_name_directory, file_name_nondirectory, file_name_as_directory, &
                                     file_exists, is_file_directory, file_modes, copy_file, file_attributes, file_status_type, &
                                     user_real_uid
    use files_mod, only: locate_dominating_file, locate_dominating_stop_directory_regular_expression, find_file_hook, &
                         close_buffer_hook, version_control_before_save_hook, version_control_after_save_hook, file_truename, &
                         find_file_noselect, abbreviate_file_name, delete_file, after_revert_hook
    use search_mod, only: string_match
    use keymap_mod, only: make_sparse_keymap, keymap_set, define_key
    use display_engine_mod, only: message
    use utility_functions_mod, only: read_yes_or_no
    use faces_mod, only: named_face_type, display_when, face_attributes
    use string_type_mod, only: string_type
    use data_mod, only: choice_setting_type
    implicit none(type, external)
    private

    public :: initialize_version_control_hooks, register_version_control_backend, version_control_backend_named, &
              version_control_backend_name
    public :: version_control_file_clearprops, version_control_file_set_backend, version_control_file_get_backend, &
              version_control_file_set_state, version_control_file_get_state, &
              version_control_file_set_working_revision, version_control_file_get_working_revision, &
              version_control_file_set_checkout_time, &
              version_control_file_get_checkout_time, version_control_file_set_git_symbolic_reference, &
              version_control_file_get_git_symbolic_reference, &
              version_control_clear_context, version_control_touch_reset, version_control_touched
    public :: version_control_find_root, version_control_registered, version_control_backend, version_control_checkout_model, &
              version_control_state, version_control_state_refresh, &
              is_version_control_up_to_date, version_control_working_revision, version_control_symbolic_working_revision, &
              version_control_short_revision, &
              version_control_version_backup_file_name, version_control_delete_automatic_version_backups, &
              version_control_make_version_backup, &
              version_control_before_save, version_control_after_save, version_control_mode_line, version_control_mode_line_state, &
              version_control_default_mode_line_string, &
              version_control_refresh_state, version_control_state_name, version_control_execute_root, &
              version_control_execute_responsible, is_version_control_make_version_backups, &
              version_control_not_supported
    public :: version_control_backend_type, version_control_backends, version_control_directory_update_procedure
    public :: version_control_file_set_incoming_revision, version_control_file_get_incoming_revision, &
              version_control_file_clear_incoming_revisions

    !> The kinds of a switches option (vc-diff-switches and the like):
    !> VERSION_CONTROL_SWITCHES_UNSPECIFIED (unspecified: try the next option), VERSION_CONTROL_SWITCHES_NONE (no switches at all),
    !> or
    !> VERSION_CONTROL_SWITCHES_LIST, an array of strings (a string is an array of one).
    integer, parameter, public :: VERSION_CONTROL_SWITCHES_UNSPECIFIED = 0, VERSION_CONTROL_SWITCHES_NONE = 1, &
                                  VERSION_CONTROL_SWITCHES_LIST = 2

    !> The value of a switches option: its kind and, for
    !> VERSION_CONTROL_SWITCHES_LIST, the switches.
    type, public :: version_control_switches_type
        integer :: kind = VERSION_CONTROL_SWITCHES_UNSPECIFIED
        type(string_type), allocatable :: list(:)
    end type version_control_switches_type

    ! ------------------------------------------------------------------
    ! Customization variables (the rest is in version_control_mod)
    ! ------------------------------------------------------------------

    !> Regexp matching directory names that are not under VERSION_CONTROL's control.
    !> The default regexp prevents fruitless and time-consuming attempts
    !> to determine the VERSION_CONTROL status in directories in which filenames are
    !> interpreted as hostnames.  Port of vc-hooks.el vc-ignore-dir-regexp.
    character(len=:), allocatable, public :: version_control_ignore_directory_regular_expression

    !> List of version control backends for which VERSION_CONTROL will be used.
    !> Entries in this list will be tried in order to determine whether a
    !> file is under that sort of version control.
    !> Removing an entry from the list prevents VERSION_CONTROL from being activated
    !> when visiting a file managed by that backend.
    !> An empty list disables VERSION_CONTROL altogether.  Port of vc-hooks.el
    !> vc-handled-backends.
    character(len=8), allocatable, public :: version_control_handled_backends(:)

    !> List of directory names to be ignored when walking directory trees.
    !> Port of vc-hooks.el vc-directory-exclusion-list.
    character(len=8), allocatable, public :: version_control_directory_exclusions(:)

    !> If true, backups of registered files are made as with other files.
    !> If false (the default), files covered by version control don't get backups.
    !> Port of vc-hooks.el vc-make-backup-files.
    logical, public :: version_control_create_backup_files = .false.

    !> Values of `version_control_follow_symlinks' besides .true. and .false.
    integer, parameter, public :: VERSION_CONTROL_FOLLOW_SYMLINKS_ASK = 2

    !> What to do if visiting a symbolic link to a file under version control.
    !> Editing such a file through the link bypasses the version control system,
    !> which is dangerous and probably not what you want.
    !>
    !> If this variable is .true., VERSION_CONTROL follows the link and visits the real file,
    !> telling you about it in the echo area.  If it is VERSION_CONTROL_FOLLOW_SYMLINKS_ASK, VERSION_CONTROL asks for
    !> confirmation whether it should follow the link.  If .false., the link is
    !> visited and a warning displayed.  Port of vc-hooks.el vc-follow-symlinks.
    type(choice_setting_type), public :: version_control_follow_symlinks = &
                                         choice_setting_type(name="version_control_follow_symlinks", &
                                                             choice=VERSION_CONTROL_FOLLOW_SYMLINKS_ASK)

    !> Values of `version_control_display_status' besides .true. and .false.
    integer, parameter, public :: VERSION_CONTROL_DISPLAY_STATUS_NO_BACKEND = 2

    !> If true, display revision number and lock status in mode line.
    !> If false, only the backend name is displayed.  When the value
    !> is VERSION_CONTROL_DISPLAY_STATUS_NO_BACKEND, then no backend name is displayed before the
    !> revision number and lock status.  Port of vc-hooks.el vc-display-status.
    type(choice_setting_type), public :: version_control_display_status = &
                                         choice_setting_type(name="version_control_display_status", value=.true.)

    !> If true, identify work files by searching for version headers.
    !> Port of vc-hooks.el vc-consult-headers.
    logical, public :: version_control_consult_headers = .true.

    !> Whether to mark conflicted file as resolved upon saving.
    !>
    !> If this is true and there are no more conflict markers in the file,
    !> VERSION_CONTROL will mark the conflicts in the saved file as resolved.  This is
    !> only meaningful for VERSION_CONTROLS that handle conflicts by inserting conflict
    !> markers in a conflicted file.
    !>
    !> When saving a conflicted file, VERSION_CONTROL first tries to use the value
    !> of `version_control_BACKEND_resolve_conflicts', for handling backend-specific
    !> settings.  It defaults to this option if that option has the special
    !> value `default'.  Port of vc-hooks.el vc-resolve-conflicts.
    logical, public :: version_control_resolve_conflicts = .true.

    !> If true, VERSION_CONTROL backend functions should return short revisions if possible.
    !> This is set to true when calling `version_control_short_revision', which will
    !> then call the `working_revision' backend function.  Port of
    !> vc-hooks.el vc-use-short-revision.
    logical, public :: version_control_use_short_revision = .false.

    ! ------------------------------------------------------------------
    ! States and checkout models
    ! ------------------------------------------------------------------

    !> The version control states of `version_control_state': none known (VERSION_CONTROL_STATE_UNKNOWN), and
    !> `up-to-date', `edited', locked by another user (USER, whose name
    !> `version_control_state_locker' gives), `needs-update', `needs-merge',
    !> `unlocked-changes', `added', `removed', `conflict', `missing',
    !> `ignored' and `unregistered'.
    integer, parameter, public :: VERSION_CONTROL_STATE_UNKNOWN = 0, VERSION_CONTROL_UP_TO_DATE = 1, VERSION_CONTROL_EDITED = 2, &
                                  VERSION_CONTROL_LOCKED = 3, &
                                  VERSION_CONTROL_NEEDS_UPDATE = 4, VERSION_CONTROL_NEEDS_MERGE = 5, &
                                  VERSION_CONTROL_UNLOCKED_CHANGES = 6, VERSION_CONTROL_ADDED = 7, &
                                  VERSION_CONTROL_REMOVED = 8, VERSION_CONTROL_CONFLICT = 9, VERSION_CONTROL_MISSING = 10, &
                                  VERSION_CONTROL_IGNORED = 11, &
                                  VERSION_CONTROL_UNREGISTERED = 12

    !> The checkout models of `version_control_checkout_model': unknown (VERSION_CONTROL_MODEL_UNKNOWN),
    !> `implicit', `locking' and `announce'.
    integer, parameter, public :: VERSION_CONTROL_MODEL_UNKNOWN = 0, VERSION_CONTROL_MODEL_IMPLICIT = 1, &
                                  VERSION_CONTROL_MODEL_LOCKING = 2, VERSION_CONTROL_MODEL_ANNOUNCE = 3

    ! ------------------------------------------------------------------
    ! Backends
    ! ------------------------------------------------------------------

    !> Kinds of a log's LIMIT: none (unlimited), a number of entries,
    !> or a revision before which to stop.
    integer, parameter, public :: VERSION_CONTROL_LIMIT_NONE = 0, VERSION_CONTROL_LIMIT_NUMBER = 1, &
                                  VERSION_CONTROL_LIMIT_REVISION = 2

    !> The LIMIT of the log commands: none, a number or a revision string.
    type, public :: version_control_log_limit_type
        integer :: kind = VERSION_CONTROL_LIMIT_NONE
        integer(int64) :: n = 0
        character(len=:), allocatable :: revision
    end type version_control_log_limit_type

    !> Rename states of a file's backend-specific information: none, a copy
    !> and a rename.
    integer, parameter, public :: VERSION_CONTROL_RENAME_NONE = 0, VERSION_CONTROL_RENAME_COPY = 1, &
                                  VERSION_CONTROL_RENAME_RENAME = 2

    !> One (FILE STATE EXTRA) entry of a backend's `directory_status_files': the
    !> file's name relative to the directory, its state, and the
    !> backend-specific EXTRA information (Git's permissions and renames,
    !> vc-git-extra-fileinfo).
    type, public :: version_control_directory_entry_type
        character(len=:), allocatable :: name
        integer :: state = 0
        logical :: has_extra = .false.
        integer :: old_permission = 0, new_permission = 0
        integer :: rename_state = VERSION_CONTROL_RENAME_NONE
        character(len=:), allocatable :: original_name
    end type version_control_directory_entry_type

    !> The information shown for a file or directory in a VC-Dir buffer
    !> (vc-dir-fileinfo): NAME, STATE, EXTRA (as in `version_control_directory_entry_type'),
    !> MARKED, NEEDS_UPDATE (to keep track of not updated files during a
    !> global refresh), DIRECTORY (allocated for directories, to
    !> distinguish files and directories) and DISPLAY_STATE (a pseudo-state
    !> for display only, allocated when set).
    type, public :: version_control_directory_fileinfo_type
        type(version_control_directory_entry_type) :: entry
        logical :: marked = .false.
        logical :: needs_update = .false.
        character(len=:), allocatable :: directory
        character(len=:), allocatable :: display_state
    end type version_control_directory_fileinfo_type

    abstract interface
        !> A backend's `registered': whether FILE is registered.
        !> Not a port.
        logical function version_control_file_test_procedure(file)
            implicit none(type, external)
            character(len=*), intent(in) :: file
        end function version_control_file_test_procedure
        !> A backend's `state'.  Not a port.
        integer function version_control_file_state_procedure(file)
            implicit none(type, external)
            character(len=*), intent(in) :: file
        end function version_control_file_state_procedure
        !> A backend function of FILE giving a string, or none (FOUND .false.):
        !> `working_revision', `root', `responsible_p', `mode_line_string',
        !> `find_ignore_file'.  Not a port.
        subroutine version_control_file_string_procedure(file, s, found)
            implicit none(type, external)
            character(len=*), intent(in) :: file
            character(len=:), allocatable, intent(out) :: s
            logical, intent(out) :: found
        end subroutine version_control_file_string_procedure
        !> A backend's `checkout_model' of FILES.  Not a port.
        integer function version_control_files_model_procedure(files)
            import :: string_type
            implicit none(type, external)
            type(string_type), intent(in) :: files(:)
        end function version_control_files_model_procedure
        !> A backend's `find_file_hook', run in the visiting buffer.
        !> Not a port.
        subroutine version_control_buffer_procedure()
            implicit none(type, external)
        end subroutine version_control_buffer_procedure
        !> A backend's `working_revision_symbol', as S (a subroutine:
        !> gfortran frees a pointer component to a function with a
        !> deferred-length result).  Not a port.
        subroutine version_control_symbol_procedure(s)
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: s
        end subroutine version_control_symbol_procedure
        !> A backend operation on FILES: `revert_files', `delete_files',
        !> `mark_resolved'.  Not a port.
        recursive subroutine version_control_files_procedure(files, status)
            import :: string_type, status_type
            implicit none(type, external)
            type(string_type), intent(in) :: files(:)
            type(status_type), intent(out) :: status
        end subroutine version_control_files_procedure
        !> A backend's `register' of FILES (COMMENT ignored by most).
        !> Not a port.
        recursive subroutine version_control_register_procedure(files, status, comment)
            import :: string_type, status_type
            implicit none(type, external)
            type(string_type), intent(in) :: files(:)
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: comment
        end subroutine version_control_register_procedure
        !> A backend operation on FILE: `delete_file', `unregister'.
        !> Not a port.
        recursive subroutine version_control_file_procedure(file, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: file
            type(status_type), intent(out) :: status
        end subroutine version_control_file_procedure
        !> A backend's `revert' of FILE (CONTENTS_DONE, the backup file
        !> already copied back, or absent).  Not a port.
        recursive subroutine version_control_revert_procedure(file, status, contents_done)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: file
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: contents_done
        end subroutine version_control_revert_procedure
        !> A backend's `diff' of FILES between REV1 and REV2 (absent: the
        !> working revision and the work file) into BUFFER, asynchronously
        !> with ASYNCHRONOUS.  CHANGES is false when there are none (exit status 0
        !> of a synchronous diff).  Not a port.
        recursive subroutine version_control_diff_procedure(files, buffer, asynchronous, status, rev1, rev2)
            import :: string_type, status_type, handle_type
            implicit none(type, external)
            type(string_type), intent(in) :: files(:)
            type(handle_type), intent(in) :: buffer
            logical, intent(in) :: asynchronous
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: rev1, rev2
        end subroutine version_control_diff_procedure
        !> A backend's `find_revision': insert revision REVISION of FILE into
        !> BUFFER.  Not a port.
        recursive subroutine version_control_find_revision_procedure(file, revision, buffer, status)
            import :: status_type, handle_type
            implicit none(type, external)
            character(len=*), intent(in) :: file, revision
            type(handle_type), intent(in) :: buffer
            type(status_type), intent(out) :: status
        end subroutine version_control_find_revision_procedure
        !> A backend's `checkout' of FILE at REVISION (absent: the head).
        !> Not a port.
        recursive subroutine version_control_checkout_procedure(file, status, revision)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: file
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: revision
        end subroutine version_control_checkout_procedure
        !> A backend's `rename_file' from OLD to NEW.  Not a port.
        recursive subroutine version_control_rename_procedure(old, new, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: old, new
            type(status_type), intent(out) :: status
        end subroutine version_control_rename_procedure
        !> A backend's `previous_revision' of REVISION for FILE.  Not a port.
        subroutine version_control_previous_revision_procedure(file, revision, previous, found)
            implicit none(type, external)
            character(len=*), intent(in) :: file, revision
            character(len=:), allocatable, intent(out) :: previous
            logical, intent(out) :: found
        end subroutine version_control_previous_revision_procedure
        !> A backend's `revision_completion_table' for FILES.  Not a port.
        subroutine version_control_revision_table_procedure(files, table)
            import :: string_type
            implicit none(type, external)
            type(string_type), intent(in) :: files(:)
            type(string_type), allocatable, intent(out) :: table(:)
        end subroutine version_control_revision_table_procedure
        !> A backend's `create_repository' in `default_directory'.  Not a port.
        recursive subroutine version_control_status_procedure(status)
            import :: status_type
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine version_control_status_procedure
        !> A backend's `print_log': insert the log of FILES into BUFFER (a
        !> short log with SHORTLOG), starting at START_REVISION ("" for the
        !> current one), with LIMIT.  LIMIT_UNSUPPORTED is the return value
        !> `limit-unsupported'.  Not a port.
        recursive subroutine version_control_print_log_procedure(files, buffer, shortlog, start_revision, limit, status, &
                                                                 limit_unsupported)
            import :: string_type, handle_type, version_control_log_limit_type, status_type
            implicit none(type, external)
            type(string_type), intent(in) :: files(:)
            type(handle_type), intent(in) :: buffer
            logical, intent(in) :: shortlog
            character(len=*), intent(in) :: start_revision
            type(version_control_log_limit_type), intent(in) :: limit
            type(status_type), intent(out) :: status
            logical, intent(out) :: limit_unsupported
        end subroutine version_control_print_log_procedure
        !> The UPDATE function given to a backend's `directory_status_files': add
        !> ENTRIES to the VC-Dir buffer BUFFER; MORE_TO_COME is true when
        !> more updates will come from the asynchronous process.
        !> Not a port.
        recursive subroutine version_control_directory_update_procedure(entries, buffer, more_to_come)
            import :: version_control_directory_entry_type, handle_type
            implicit none(type, external)
            type(version_control_directory_entry_type), intent(in) :: entries(:)
            type(handle_type), intent(in) :: buffer
            logical, intent(in) :: more_to_come
        end subroutine version_control_directory_update_procedure
        !> A backend's `directory_status_files': compute the states of FILES (all
        !> files when empty) in DIRECTORY asynchronously and pass them to UPDATE
        !> for BUFFER.  Not a port.
        recursive subroutine version_control_directory_status_files_procedure(directory, files, update, buffer, status)
            import :: string_type, handle_type, status_type, version_control_directory_update_procedure
            implicit none(type, external)
            character(len=*), intent(in) :: directory
            type(string_type), intent(in) :: files(:)
            procedure(version_control_directory_update_procedure) :: update
            type(handle_type), intent(in) :: buffer
            type(status_type), intent(out) :: status
        end subroutine version_control_directory_status_files_procedure
        !> A backend's `directory_extra_headers' of DIRECTORY, as the parts of the text.
        !> Not a port.
        recursive subroutine version_control_directory_extra_headers_procedure(directory, parts)
            import :: fortmacs_widget_for_object_collections_part_type
            implicit none(type, external)
            character(len=*), intent(in) :: directory
            type(fortmacs_widget_for_object_collections_part_type), allocatable, intent(out) :: parts(:)
        end subroutine version_control_directory_extra_headers_procedure
        !> A backend's `directory_printer': insert INFO at point.  Not a port.
        recursive subroutine version_control_directory_printer_procedure(info)
            import :: version_control_directory_fileinfo_type
            implicit none(type, external)
            type(version_control_directory_fileinfo_type), intent(in) :: info
        end subroutine version_control_directory_printer_procedure
        !> A backend's `status_fileinfo_extra' of FILE, set in ENTRY.
        !> Not a port.
        recursive subroutine version_control_status_fileinfo_extra_procedure(file, entry)
            import :: version_control_directory_entry_type
            implicit none(type, external)
            character(len=*), intent(in) :: file
            type(version_control_directory_entry_type), intent(inout) :: entry
        end subroutine version_control_status_fileinfo_extra_procedure
        !> A backend's `show_log_entry': move to the log entry for REVISION
        !> (FOUND, when present, tells whether it was found).  Not a port.
        recursive subroutine version_control_show_log_entry_procedure(revision, found)
            implicit none(type, external)
            character(len=*), intent(in) :: revision
            logical, intent(out), optional :: found
        end subroutine version_control_show_log_entry_procedure
        !> A backend's `checkin' of FILES with COMMENT (REVISION, the new
        !> revision, for the backends that take one).  Not a port.
        recursive subroutine version_control_checkin_procedure(files, comment, status, revision)
            import :: string_type, status_type
            implicit none(type, external)
            type(string_type), intent(in) :: files(:)
            character(len=*), intent(in) :: comment
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: revision
        end subroutine version_control_checkin_procedure
    end interface

    !> Values of a backend's `revision_granularity': whether revisions are
    !> kept per file (VERSION_CONTROL_REVISION_GRANULARITY_FILE) or for the whole
    !> repository (VERSION_CONTROL_REVISION_GRANULARITY_REPOSITORY).
    integer, parameter, public :: VERSION_CONTROL_REVISION_GRANULARITY_FILE = 0, VERSION_CONTROL_REVISION_GRANULARITY_REPOSITORY = 1

    !> One entry of a backend's `file_name_changes': a file's OLD name and
    !> its NEW name in a revision (OLD unallocated for an added file, NEW
    !> unallocated for a deleted one).
    type, public :: version_control_file_name_change_type
        character(len=:), allocatable :: old, new
    end type version_control_file_name_change_type

    abstract interface
        !> A backend's `create_tag': in DIRECTORY, make a tag called NAME, or a
        !> branch with IS_BRANCH (then check it out).  Not a port.
        recursive subroutine version_control_create_tag_procedure(directory, name, is_branch, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: directory, name
            logical, intent(in) :: is_branch
            type(status_type), intent(out) :: status
        end subroutine version_control_create_tag_procedure
        !> A backend's `retrieve_tag': in DIRECTORY, check out the tag or branch
        !> NAME ("" for the latest revisions); UPDATE says whether to update
        !> the buffers of the files.  Not a port.
        recursive subroutine version_control_retrieve_tag_procedure(directory, name, update, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: directory, name
            logical, intent(in) :: update
            type(status_type), intent(out) :: status
        end subroutine version_control_retrieve_tag_procedure
        !> A backend's `pull' or `push' of the current branch (prompting for
        !> the command with PROMPT), giving the asynchronous PROCESS.
        !> Not a port.
        recursive subroutine version_control_pull_procedure(prompt, status, process)
            import :: status_type, handle_type
            implicit none(type, external)
            logical, intent(in) :: prompt
            type(status_type), intent(out) :: status
            type(handle_type), intent(out) :: process
        end subroutine version_control_pull_procedure
        !> A backend's `incoming_revision': the REVISION at the head of the
        !> branch at UPSTREAM_LOCATION ("" for where `version_control_pull' pulls from),
        !> fetched again with REFRESH; FOUND is false when there is no such
        !> branch.  Not a port.
        recursive subroutine version_control_incoming_revision_procedure(upstream_location, refresh, revision, found, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: upstream_location
            logical, intent(in) :: refresh
            character(len=:), allocatable, intent(out) :: revision
            logical, intent(out) :: found
            type(status_type), intent(out) :: status
        end subroutine version_control_incoming_revision_procedure
        !> A backend's `mergebase': the common ancestor BASE of REV1 and
        !> REV2 ("" for the working revision).  Not a port.
        recursive subroutine version_control_mergebase_procedure(rev1, rev2, base, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: rev1, rev2
            character(len=:), allocatable, intent(out) :: base
            type(status_type), intent(out) :: status
        end subroutine version_control_mergebase_procedure
        !> A backend's `annotate_command': start the command annotating
        !> revision REVISION of FILE, its output in BUFFER.  Not a port.
        recursive subroutine version_control_annotate_command_procedure(file, buffer, revision, status)
            import :: handle_type, status_type
            implicit none(type, external)
            character(len=*), intent(in) :: file
            type(handle_type), intent(in) :: buffer
            character(len=*), intent(in) :: revision
            type(status_type), intent(out) :: status
        end subroutine version_control_annotate_command_procedure
        !> A backend's `annotate_time': the time of the annotated line at
        !> point as fractional days, moving point past its annotation
        !> (FOUND false if there is none).  Not a port.
        recursive subroutine version_control_annotate_time_procedure(days, found)
            import :: real64
            implicit none(type, external)
            real(real64), intent(out) :: days
            logical, intent(out) :: found
        end subroutine version_control_annotate_time_procedure
        !> A backend's `annotate_extract_revision_at_line': the REVISION of
        !> the annotated line at point and, if the backend gives one, the
        !> FILE it had there ("" otherwise); FOUND false if there is none.
        !> Not a port.
        recursive subroutine version_control_annotate_revision_procedure(revision, file, found)
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: revision, file
            logical, intent(out) :: found
        end subroutine version_control_annotate_revision_procedure
        !> A backend's `file_name_changes' of revision REVISION: the files it
        !> added, deleted, copied or renamed.  Not a port.
        recursive subroutine version_control_file_name_changes_procedure(revision, changes)
            import :: version_control_file_name_change_type
            implicit none(type, external)
            character(len=*), intent(in) :: revision
            type(version_control_file_name_change_type), allocatable, intent(out) :: changes(:)
        end subroutine version_control_file_name_changes_procedure
    end interface

    !> A VERSION_CONTROL backend: its name (as in `version_control_handled_backends') and the
    !> functions of the backend API it implements (vc-BACKEND-FUN;
    !> unassociated ones use vc-default-FUN or a `version_control_not_supported' error condition occurs).
    type :: version_control_backend_type
        character(len=:), allocatable :: name
        procedure(version_control_file_test_procedure), pointer, nopass :: registered => null()
        procedure(version_control_file_state_procedure), pointer, nopass :: state => null()
        procedure(version_control_file_string_procedure), pointer, nopass :: working_revision => null()
        procedure(version_control_symbol_procedure), pointer, nopass :: working_revision_symbol => null()
        procedure(version_control_file_string_procedure), pointer, nopass :: mode_line_string => null()
        procedure(version_control_file_string_procedure), pointer, nopass :: root => null()
        procedure(version_control_file_string_procedure), pointer, nopass :: responsible_p => null()
        procedure(version_control_file_string_procedure), pointer, nopass :: find_ignore_file => null()
        procedure(version_control_files_model_procedure), pointer, nopass :: checkout_model => null()
        procedure(version_control_buffer_procedure), pointer, nopass :: find_file_hook => null()
        procedure(version_control_file_test_procedure), pointer, nopass :: make_version_backups_p => null()
        procedure(version_control_register_procedure), pointer, nopass :: register => null()
        procedure(version_control_file_procedure), pointer, nopass :: unregister => null()
        procedure(version_control_revert_procedure), pointer, nopass :: revert => null()
        procedure(version_control_files_procedure), pointer, nopass :: revert_files => null()
        procedure(version_control_diff_procedure), pointer, nopass :: diff => null()
        procedure(version_control_find_revision_procedure), pointer, nopass :: find_revision => null()
        procedure(version_control_checkout_procedure), pointer, nopass :: checkout => null()
        procedure(version_control_file_procedure), pointer, nopass :: delete_file => null()
        procedure(version_control_files_procedure), pointer, nopass :: delete_files => null()
        procedure(version_control_rename_procedure), pointer, nopass :: rename_file => null()
        procedure(version_control_previous_revision_procedure), pointer, nopass :: previous_revision => null()
        procedure(version_control_revision_table_procedure), pointer, nopass :: revision_completion_table => null()
        procedure(version_control_files_procedure), pointer, nopass :: mark_resolved => null()
        procedure(version_control_status_procedure), pointer, nopass :: create_repository => null()
        procedure(version_control_checkin_procedure), pointer, nopass :: checkin => null()
        !> The backend's Log Edit mode (vc-default-log-edit-mode is
        !> `log_edit_mode').
        procedure(version_control_status_procedure), pointer, nopass :: log_edit_mode => null()
        procedure(version_control_print_log_procedure), pointer, nopass :: print_log => null()
        !> The backend's Log View mode (vc-default-log-view-mode is
        !> `log_view_mode').
        procedure(version_control_status_procedure), pointer, nopass :: log_view_mode => null()
        procedure(version_control_show_log_entry_procedure), pointer, nopass :: show_log_entry => null()
        !> Whether revisions are kept per file or for the whole repository
        !> (vc-BACKEND-revision-granularity).
        integer :: revision_granularity = VERSION_CONTROL_REVISION_GRANULARITY_FILE
        !> Whether `version_control_retrieve_tag' asks to update the buffers
        !> (vc-default-update-on-retrieve-tag is true).
        logical :: update_on_retrieve_tag = .true.
        procedure(version_control_create_tag_procedure), pointer, nopass :: create_tag => null()
        procedure(version_control_retrieve_tag_procedure), pointer, nopass :: retrieve_tag => null()
        procedure(version_control_pull_procedure), pointer, nopass :: pull => null()
        procedure(version_control_pull_procedure), pointer, nopass :: push => null()
        procedure(version_control_incoming_revision_procedure), pointer, nopass :: incoming_revision => null()
        procedure(version_control_mergebase_procedure), pointer, nopass :: mergebase => null()
        procedure(version_control_directory_status_files_procedure), pointer, nopass :: directory_status_files => null()
        procedure(version_control_directory_extra_headers_procedure), pointer, nopass :: directory_extra_headers => null()
        procedure(version_control_directory_printer_procedure), pointer, nopass :: directory_printer => null()
        procedure(version_control_status_fileinfo_extra_procedure), pointer, nopass :: status_fileinfo_extra => null()
        !> vc-BACKEND-diff-switches, if the backend has it.
        type(version_control_switches_type), pointer :: diff_switches => null()
        !> vc-BACKEND-register-switches, if the backend has it.
        type(version_control_switches_type), pointer :: register_switches => null()
        ! VC-Annotate (vc-annotate.el).
        procedure(version_control_annotate_command_procedure), pointer, nopass :: annotate_command => null()
        procedure(version_control_annotate_time_procedure), pointer, nopass :: annotate_time => null()
        procedure(version_control_annotate_revision_procedure), pointer, nopass :: annotate_extract_revision_at_line => null()
        procedure(version_control_previous_revision_procedure), pointer, nopass :: next_revision => null()
        procedure(version_control_file_name_changes_procedure), pointer, nopass :: file_name_changes => null()
        !> vc-BACKEND-annotate-switches, if the backend has it.
        type(version_control_switches_type), pointer :: annotate_switches => null()
    end type version_control_backend_type

    !> The capacity of `version_control_backends' (upstream's vc-handled-backends has 8).
    integer, parameter, public :: MAXIMUM_VERSION_CONTROL_BACKENDS = 16

    !> The backends ported, registered by their modules (version_control_git_mod).
    !> (A fixed array: gfortran 13 frees an associated procedure pointer
    !> component whose function returns an allocatable string when an
    !> array of this type is deallocated, aborting.)
    type(version_control_backend_type), target :: version_control_backends(MAXIMUM_VERSION_CONTROL_BACKENDS)

    ! The number of entries of `version_control_backends' in use.
    integer :: version_control_backend_count = 0

    !> List of `version_control_directory' buffers.  Port of vc-hooks.el vc-dir-buffers.
    type(handle_type), allocatable, public :: version_control_directory_buffers(:)

    abstract interface
        !> `version_control_directory_resynch_file': update the entries for FILE_NAME in any
        !> directory buffers that list it.  Not a port.
        recursive subroutine version_control_directory_resynch_file_procedure(file_name)
            implicit none(type, external)
            character(len=*), intent(in) :: file_name
        end subroutine version_control_directory_resynch_file_procedure
    end interface

    !> vc-dir.el's `version_control_directory_resynch_file', associated once VC-Dir is loaded
    !> (version_control_directory_mod), for vc-hooks.el, vc-dispatcher.el and vc.el.
    procedure(version_control_directory_resynch_file_procedure), pointer, public :: version_control_directory_resynch_file => null()

    ! ------------------------------------------------------------------
    ! Per-file properties
    ! ------------------------------------------------------------------

    !> The per-file properties (upstream's vc-file-prop-obarray), by property.
    integer, parameter, public :: VERSION_CONTROL_PROPERTY_BACKEND = 1, VERSION_CONTROL_PROPERTY_STATE = 2, &
                                  VERSION_CONTROL_PROPERTY_WORKING_REVISION = 3, &
                                  VERSION_CONTROL_PROPERTY_CHECKOUT_TIME = 4, VERSION_CONTROL_PROPERTY_GIT_SYMBOLIC_REFERENCE = 5
    integer, parameter :: NPROPS = 5

    !> The backend of an unregistered file (the property `none').
    integer, parameter, public :: VERSION_CONTROL_BACKEND_NONE = -1

    !> One entry of a repository's `vc-incoming-revision' property: the
    !> incoming revision found for UPSTREAM_LOCATION ("" for the default
    !> location), REVISION unallocated when there is none.
    type :: version_control_incoming_revision_type
        character(len=:), allocatable :: upstream_location
        character(len=:), allocatable :: revision
    end type version_control_incoming_revision_type

    !> VERSION_CONTROL's properties of a file (an expanded name): which are set, the
    !> backend (an index of `version_control_backends', or VERSION_CONTROL_BACKEND_NONE), the
    !> state (and the locker of VERSION_CONTROL_LOCKED), the working revision, the
    !> checkout time (seconds and nanoseconds; none when unset), Git's
    !> symbolic ref and, for a repository's root, the incoming revisions
    !> (unallocated when unset).
    type :: version_control_properties_type
        character(len=:), allocatable :: file
        logical :: set(NPROPS) = .false.
        integer :: backend = 0
        integer :: state = VERSION_CONTROL_STATE_UNKNOWN
        character(len=:), allocatable :: locker
        character(len=:), allocatable :: working_revision
        integer(int64) :: checkout_second = 0
        integer(int32) :: checkout_nsec = 0
        character(len=:), allocatable :: git_symbolic_reference
        type(version_control_incoming_revision_type), allocatable :: incoming_revisions(:)
    end type version_control_properties_type

    type(version_control_properties_type), allocatable :: properties(:)

    !> The properties set since `version_control_touch_reset' (vc-touched-properties),
    !> for version_control_mod's with-vc-properties.
    logical :: touched(NPROPS) = .false.

    !> Version control prefix map (C-x v).  Port of vc-hooks.el
    !> vc-prefix-map.
    type(handle_type), public :: version_control_prefix_map

    ! Faces.
    !> Base face for VERSION_CONTROL state indicator.  Port of vc-hooks.el vc-state-base.
    type(named_face_type), target, public :: version_control_state_base
    !> Face for VERSION_CONTROL modeline state when the file is up to date.
    type(named_face_type), target, public :: version_control_up_to_date_state
    !> Face for VERSION_CONTROL modeline state when the file needs update.
    type(named_face_type), target, public :: version_control_needs_update_state
    !> Face for VERSION_CONTROL modeline state when the file locked.
    type(named_face_type), target, public :: version_control_locked_state
    !> Face for VERSION_CONTROL modeline state when the file is locally added.
    type(named_face_type), target, public :: version_control_locally_added_state
    !> Face for VERSION_CONTROL modeline state when the file contains merge conflicts.
    type(named_face_type), target, public :: version_control_conflict_state
    !> Face for VERSION_CONTROL modeline state when the file was removed from the VERSION_CONTROL system.
    type(named_face_type), target, public :: version_control_removed_state
    !> Face for VERSION_CONTROL modeline state when the file is missing from the file system.
    type(named_face_type), target, public :: version_control_missing_state
    !> Face for VERSION_CONTROL modeline state when the file is edited.
    type(named_face_type), target, public :: version_control_edited_state
    !> Face for VERSION_CONTROL modeline state when the file is registered, but ignored.
    type(named_face_type), target, public :: version_control_ignored_state

contains

    ! ------------------------------------------------------------------
    ! Backends
    ! ------------------------------------------------------------------

    !> Make the backend named NAME known to VERSION_CONTROL (as loading vc-BACKEND.el
    !> does): B is its index in `version_control_backends' (0 when it is full), whose operations the
    !> backend module then sets.  A backend of the same name keeps its
    !> entry, which the module sets again.  Not a port.
    subroutine register_version_control_backend(name, b)
        character(len=*), intent(in) :: name
        integer, intent(out) :: b
        integer :: k

        do k = 1, version_control_backend_count
            if (version_control_backends(k)%name == name) then
                b = k
                return
            end if
        end do

        b = 0
        if (version_control_backend_count == MAXIMUM_VERSION_CONTROL_BACKENDS) return
        version_control_backend_count = version_control_backend_count + 1
        version_control_backends(version_control_backend_count)%name = name
        b = version_control_backend_count
    end subroutine register_version_control_backend

    !> The index in `version_control_backends' of the backend named NAME (0 if none
    !> is ported).  Not a port.
    integer function version_control_backend_named(name) result(b)
        character(len=*), intent(in) :: name
        integer :: k

        b = 0

        do k = 1, version_control_backend_count
            if (version_control_backends(k)%name == trim(name)) then
                b = k
                return
            end if
        end do
    end function version_control_backend_named

    !> The name of backend B ("" for none).  Not a port.
    function version_control_backend_name(b) result(name)
        integer, intent(in) :: b
        character(len=:), allocatable :: name

        name = ""
        if (b > 0) name = version_control_backends(b)%name
    end function version_control_backend_name

    !> Signal `version_control_not_supported' for FUNCTION of backend B.  Not a port.
    subroutine version_control_not_supported(status, function, b)
        type(status_type), intent(inout) :: status
        character(len=*), intent(in) :: function
        integer, intent(in) :: b

        call set_error(status, VERSION_CONTROL_NOT_SUPPORTED_CODE, function//", "//version_control_backend_name(b))
    end subroutine version_control_not_supported

    !> Backend B's `registered' of FILE (vc-default-registered checks
    !> master templates, which no ported backend has).  Not a port.
    recursive logical function execute_registered(b, file) result(r)
        integer, intent(in) :: b
        character(len=*), intent(in) :: file

        r = .false.
        if (associated(version_control_backends(b)%registered)) r = version_control_backends(b)%registered(file)
    end function execute_registered

    !> Backend B's `root' of FILE: the root directory (FOUND .false. for
    !> none); an error if it doesn't implement it.  Not a port.
    subroutine version_control_execute_root(b, file, root, found, status)
        integer, intent(in) :: b
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: root
        logical, intent(out) :: found
        type(status_type), intent(out) :: status

        root = ""
        found = .false.

        if (.not. associated(version_control_backends(b)%root)) then
            call version_control_not_supported(status, "root", b)
            return
        end if

        call version_control_backends(b)%root(file, root, found)
    end subroutine version_control_execute_root

    !> Backend B's `responsible_p' of FILE: the directory it is
    !> responsible for FILE from (vc-default-responsible-p: none).
    !> Not a port.
    subroutine version_control_execute_responsible(b, file, directory, found)
        integer, intent(in) :: b
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: directory
        logical, intent(out) :: found

        directory = ""
        found = .false.
        if (associated(version_control_backends(b)%responsible_p)) call version_control_backends(b)%responsible_p(file, directory, &
                                                                                                                  found)
    end subroutine version_control_execute_responsible

    !> Backend B's `make_version_backups_p' of FILE.  Port of
    !> vc-hooks.el:vc-default-make-version-backups-p as the default:
    !> return true if unmodified versions should be backed up locally.
    !> The default is to switch off this feature.
    logical function is_version_control_make_version_backups(b, file) result(r)
        integer, intent(in) :: b
        character(len=*), intent(in) :: file

        r = .false.
        if (associated(version_control_backends(b)%make_version_backups_p)) r = version_control_backends( &
                                                                                b)%make_version_backups_p(file)
    end function is_version_control_make_version_backups

    ! ------------------------------------------------------------------
    ! Per-file properties
    ! ------------------------------------------------------------------

    !> The index of FILE's properties in `properties' (0 if FILE has none).
    !> Not a port.
    integer function property_index(file) result(k)
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: name
        type(status_type) :: st

        k = 0
        if (.not. allocated(properties)) return
        call expand_file_name(file, name, st)
        if (.not. st%ok()) name = file

        do k = 1, size(properties)
            if (properties(k)%file == name .and. len(properties(k)%file) == len(name)) return
        end do

        k = 0
    end function property_index

    !> Find or make FILE's properties in `properties', their index K.
    !> Not a port.
    subroutine make_property_entry(file, k)
        character(len=*), intent(in) :: file
        integer, intent(out) :: k
        type(version_control_properties_type) :: entry
        type(status_type) :: st

        k = property_index(file)
        if (k > 0) return
        if (.not. allocated(properties)) allocate (properties(0))
        call expand_file_name(file, entry%file, st)
        if (.not. st%ok()) entry%file = file
        properties = [properties, entry]
        k = size(properties)
    end subroutine make_property_entry

    !> Note that property PROPERTY was set (cl-pushnew onto vc-touched-properties).
    !> Not a port.
    subroutine touch(property)
        integer, intent(in) :: property

        touched(property) = .true.
    end subroutine touch

    !> Start recording which properties are set (let-binding
    !> `vc-touched-properties' to (t)).  Not a port.
    subroutine version_control_touch_reset()
        touched = .false.
    end subroutine version_control_touch_reset

    !> Whether property PROPERTY was set since `version_control_touch_reset'.  Not a port.
    logical function version_control_touched(property)
        integer, intent(in) :: property

        version_control_touched = touched(property)
    end function version_control_touched

    !> Set per-file VERSION_CONTROL property `vc-backend' for FILE to backend B (0 for
    !> unknown, VERSION_CONTROL_BACKEND_NONE for none).  Port of vc-hooks.el:vc-file-setprop.
    subroutine version_control_file_set_backend(file, b)
        character(len=*), intent(in) :: file
        integer, intent(in) :: b
        integer :: k

        call touch(VERSION_CONTROL_PROPERTY_BACKEND)
        call make_property_entry(file, k)
        properties(k)%backend = b
        properties(k)%set(VERSION_CONTROL_PROPERTY_BACKEND) = b /= 0
    end subroutine version_control_file_set_backend

    !> Get per-file VERSION_CONTROL property `vc-backend' for FILE (0 for none).  Port of
    !> vc-hooks.el:vc-file-getprop.
    integer function version_control_file_get_backend(file) result(b)
        character(len=*), intent(in) :: file
        integer :: k

        b = 0
        k = property_index(file)
        if (k == 0) return
        if (properties(k)%set(VERSION_CONTROL_PROPERTY_BACKEND)) b = properties(k)%backend
    end function version_control_file_get_backend

    !> Set per-file VERSION_CONTROL property `vc-state' for FILE to STATE (LOCKER for
    !> VERSION_CONTROL_LOCKED; VERSION_CONTROL_STATE_UNKNOWN for none).  Port of
    !> vc-hooks.el:vc-file-setprop.
    subroutine version_control_file_set_state(file, state, locker)
        character(len=*), intent(in) :: file
        integer, intent(in) :: state
        character(len=*), intent(in), optional :: locker
        integer :: k

        call touch(VERSION_CONTROL_PROPERTY_STATE)
        call make_property_entry(file, k)
        properties(k)%state = state
        properties(k)%set(VERSION_CONTROL_PROPERTY_STATE) = state /= VERSION_CONTROL_STATE_UNKNOWN

        if (present(locker)) then
            properties(k)%locker = locker
        else if (allocated(properties(k)%locker)) then
            deallocate (properties(k)%locker)
        end if
    end subroutine version_control_file_set_state

    !> Get per-file VERSION_CONTROL property `vc-state' for FILE (VERSION_CONTROL_STATE_UNKNOWN for
    !> none), and with LOCKER the user of VERSION_CONTROL_LOCKED.  Port of
    !> vc-hooks.el:vc-file-getprop.
    integer function version_control_file_get_state(file, locker) result(state)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out), optional :: locker
        integer :: k

        state = VERSION_CONTROL_STATE_UNKNOWN
        if (present(locker)) locker = ""
        k = property_index(file)
        if (k == 0) return
        if (properties(k)%set(VERSION_CONTROL_PROPERTY_STATE)) state = properties(k)%state
        if (present(locker) .and. allocated(properties(k)%locker)) locker = properties(k)%locker
    end function version_control_file_get_state

    !> Set per-file VERSION_CONTROL property `vc-working-revision' for FILE (unset
    !> with REVISION absent).  Port of vc-hooks.el:vc-file-setprop.
    subroutine version_control_file_set_working_revision(file, revision)
        character(len=*), intent(in) :: file
        character(len=*), intent(in), optional :: revision
        integer :: k

        call touch(VERSION_CONTROL_PROPERTY_WORKING_REVISION)
        call make_property_entry(file, k)
        properties(k)%set(VERSION_CONTROL_PROPERTY_WORKING_REVISION) = present(revision)
        if (present(revision)) properties(k)%working_revision = revision
    end subroutine version_control_file_set_working_revision

    !> Get per-file VERSION_CONTROL property `vc-working-revision' for FILE (FOUND
    !> .false. for none).  Port of vc-hooks.el:vc-file-getprop.
    subroutine version_control_file_get_working_revision(file, revision, found)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: revision
        logical, intent(out) :: found
        integer :: k

        revision = ""
        found = .false.
        k = property_index(file)
        if (k == 0) return
        found = properties(k)%set(VERSION_CONTROL_PROPERTY_WORKING_REVISION)
        if (found) revision = properties(k)%working_revision
    end subroutine version_control_file_get_working_revision

    !> Set per-file VERSION_CONTROL property `vc-checkout-time' for FILE (none with SECOND
    !> absent).  Port of vc-hooks.el:vc-file-setprop.
    subroutine version_control_file_set_checkout_time(file, second, nsec)
        character(len=*), intent(in) :: file
        integer(int64), intent(in), optional :: second
        integer(int32), intent(in), optional :: nsec
        integer :: k

        call touch(VERSION_CONTROL_PROPERTY_CHECKOUT_TIME)
        call make_property_entry(file, k)
        properties(k)%set(VERSION_CONTROL_PROPERTY_CHECKOUT_TIME) = present(second)
        if (present(second)) properties(k)%checkout_second = second
        properties(k)%checkout_nsec = 0
        if (present(nsec)) properties(k)%checkout_nsec = nsec
    end subroutine version_control_file_set_checkout_time

    !> Get per-file VERSION_CONTROL property `vc-checkout-time' for FILE (FOUND .false.
    !> for none).  Port of vc-hooks.el:vc-file-getprop.
    subroutine version_control_file_get_checkout_time(file, second, nsec, found)
        character(len=*), intent(in) :: file
        integer(int64), intent(out) :: second
        integer(int32), intent(out) :: nsec
        logical, intent(out) :: found
        integer :: k

        second = 0
        nsec = 0
        found = .false.
        k = property_index(file)
        if (k == 0) return
        found = properties(k)%set(VERSION_CONTROL_PROPERTY_CHECKOUT_TIME)
        second = properties(k)%checkout_second
        nsec = properties(k)%checkout_nsec
    end subroutine version_control_file_get_checkout_time

    !> Set per-file VERSION_CONTROL property `vc-git-symbolic-ref' for FILE (none with
    !> REFERENCE absent).  Port of vc-hooks.el:vc-file-setprop.
    subroutine version_control_file_set_git_symbolic_reference(file, reference)
        character(len=*), intent(in) :: file
        character(len=*), intent(in), optional :: reference
        integer :: k

        call touch(VERSION_CONTROL_PROPERTY_GIT_SYMBOLIC_REFERENCE)
        call make_property_entry(file, k)
        properties(k)%set(VERSION_CONTROL_PROPERTY_GIT_SYMBOLIC_REFERENCE) = present(reference)
        if (present(reference)) properties(k)%git_symbolic_reference = reference
    end subroutine version_control_file_set_git_symbolic_reference

    !> Get per-file VERSION_CONTROL property `vc-git-symbolic-ref' for FILE (FOUND
    !> .false. for none).  Port of vc-hooks.el:vc-file-getprop.
    subroutine version_control_file_get_git_symbolic_reference(file, reference, found)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: reference
        logical, intent(out) :: found
        integer :: k

        reference = ""
        found = .false.
        k = property_index(file)
        if (k == 0) return
        found = properties(k)%set(VERSION_CONTROL_PROPERTY_GIT_SYMBOLIC_REFERENCE)
        if (found) reference = properties(k)%git_symbolic_reference
    end subroutine version_control_file_get_git_symbolic_reference

    !> Set the entry for UPSTREAM_LOCATION ("" for the default) of the
    !> per-repository property `vc-incoming-revision' of ROOT to REVISION
    !> (absent: there is none).  Port of vc-hooks.el:vc--repo-setprop
    !> (with vc.el:vc--incoming-revision's update).
    subroutine version_control_file_set_incoming_revision(root, upstream_location, revision)
        character(len=*), intent(in) :: root, upstream_location
        character(len=*), intent(in), optional :: revision
        type(version_control_incoming_revision_type) :: entry
        integer :: k, i

        call make_property_entry(root, k)
        if (.not. allocated(properties(k)%incoming_revisions)) allocate (properties(k)%incoming_revisions(0))
        i = incoming_index(properties(k)%incoming_revisions, upstream_location)

        if (i == 0) then
            entry%upstream_location = upstream_location
            properties(k)%incoming_revisions = [properties(k)%incoming_revisions, entry]
            i = size(properties(k)%incoming_revisions)
        end if

        if (allocated(properties(k)%incoming_revisions(i)%revision)) deallocate (properties(k)%incoming_revisions(i)%revision)
        if (present(revision)) properties(k)%incoming_revisions(i)%revision = revision
    end subroutine version_control_file_set_incoming_revision

    !> Get the entry for UPSTREAM_LOCATION of the per-repository property
    !> `vc-incoming-revision' of ROOT: KNOWN is false when there is none,
    !> FOUND false when the entry says there is no incoming revision.
    !> Port of vc-hooks.el:vc--repo-getprop (with
    !> vc.el:vc--incoming-revision's lookup).
    subroutine version_control_file_get_incoming_revision(root, upstream_location, revision, known, found)
        character(len=*), intent(in) :: root, upstream_location
        character(len=:), allocatable, intent(out) :: revision
        logical, intent(out) :: known, found
        integer :: k, i

        revision = ""
        known = .false.
        found = .false.
        k = property_index(root)
        if (k == 0) return
        if (.not. allocated(properties(k)%incoming_revisions)) return
        i = incoming_index(properties(k)%incoming_revisions, upstream_location)
        if (i == 0) return
        known = .true.
        found = allocated(properties(k)%incoming_revisions(i)%revision)
        if (found) revision = properties(k)%incoming_revisions(i)%revision
    end subroutine version_control_file_get_incoming_revision

    !> Forget the per-repository property `vc-incoming-revision' of ROOT
    !> (setting it to none).  Port of vc-hooks.el:vc--repo-setprop.
    subroutine version_control_file_clear_incoming_revisions(root)
        character(len=*), intent(in) :: root
        integer :: k

        k = property_index(root)
        if (k == 0) return
        if (allocated(properties(k)%incoming_revisions)) deallocate (properties(k)%incoming_revisions)
    end subroutine version_control_file_clear_incoming_revisions

    !> The index of UPSTREAM_LOCATION's entry in ENTRIES (0 if none).
    !> Not a port.
    integer function incoming_index(entries, upstream_location) result(i)
        type(version_control_incoming_revision_type), intent(in) :: entries(:)
        character(len=*), intent(in) :: upstream_location

        do i = 1, size(entries)
            if (len(entries(i)%upstream_location) /= len(upstream_location)) cycle
            if (entries(i)%upstream_location == upstream_location) return
        end do

        i = 0
    end function incoming_index

    !> Clear all VERSION_CONTROL properties of FILE.  Port of
    !> vc-hooks.el:vc-file-clearprops (`version_control_parent_buffer' is version_control_dispatcher_mod's).
    subroutine version_control_file_clearprops(file)
        character(len=*), intent(in) :: file
        integer :: k
        type(version_control_properties_type) :: fresh

        k = property_index(file)
        if (k == 0) return
        call move_alloc(properties(k)%file, fresh%file)
        properties(k) = fresh
    end subroutine version_control_file_clearprops

    !> Clear all cached file properties.  Port of vc.el:vc-clear-context.
    subroutine version_control_clear_context()
        if (allocated(properties)) deallocate (properties)
        allocate (properties(0))
    end subroutine version_control_clear_context

    ! ------------------------------------------------------------------
    ! Finding backends, states and revisions
    ! ------------------------------------------------------------------

    !> Find the root of a checked out project.
    !> The function walks up the directory tree from FILE looking for WITNESS.
    !> If WITNESS if not found, FOUND is false, otherwise ROOT is the root.
    !> Port of vc-hooks.el:vc-find-root.
    subroutine version_control_find_root(file, witness, root, found)
        character(len=*), intent(in) :: file, witness
        character(len=:), allocatable, intent(out) :: root
        logical, intent(out) :: found

        if (allocated(version_control_ignore_directory_regular_expression)) then
            call locate_dominating_file(file, witness, root, found, version_control_ignore_directory_regular_expression)
        else
            call locate_dominating_file(file, witness, root, found, locate_dominating_stop_directory_regular_expression)
        end if
    end subroutine version_control_find_root

    !> Return true if FILE is registered in a version control system.
    !>
    !> This function performs the check each time it is called.  To rely
    !> on the result of a previous call, use `version_control_backend' instead.  If the
    !> file was previously registered under a certain backend, then that
    !> backend is tried first.  Port of vc-hooks.el:vc-registered.
    recursive logical function version_control_registered(file) result(r)
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: directory
        type(status_type) :: st
        integer :: b, k, c

        r = .false.
        directory = file_name_directory(file)

        if (len(directory) > 0 .and. allocated(version_control_ignore_directory_regular_expression)) then
            if (string_match(version_control_ignore_directory_regular_expression, directory, st) > 0) return
        end if

        ! Try vc-BACKEND-registered for each handled BACKEND.
        b = version_control_file_get_backend(file)

        if (b > 0) then
            if (execute_registered(b, file)) then
                call version_control_file_set_backend(file, b)
                r = .true.
                return
            end if
        end if

        if (allocated(version_control_handled_backends)) then
            do k = 1, size(version_control_handled_backends)
                c = version_control_backend_named(version_control_handled_backends(k))
                if (c == 0) cycle

                if (execute_registered(c, file)) then
                    call version_control_file_set_backend(file, c)
                    r = .true.
                    return
                end if
            end do
        end if

        ! File is not registered.
        call version_control_file_set_backend(file, VERSION_CONTROL_BACKEND_NONE)
    end function version_control_registered

    !> Return the version control type of FILE, 0 if it's not registered.
    !>
    !> This function returns cached information.  To query the VERSION_CONTROLS regarding
    !> whether FILE is registered or unregistered, use `version_control_registered'.
    !> Port of vc-hooks.el:vc-backend (a single file; "" for none).
    recursive integer function version_control_backend(file) result(b)
        character(len=*), intent(in) :: file

        b = 0
        if (len(file) == 0) return
        b = version_control_file_get_backend(file)

        ! Note that internally, Emacs remembers unregistered files by
        ! setting the property to `none'.
        if (b == VERSION_CONTROL_BACKEND_NONE) then
            b = 0
        else if (b == 0) then
            ! vc-registered sets the vc-backend property
            if (version_control_registered(file)) then
                b = version_control_file_get_backend(file)
            end if
        end if
    end function version_control_backend

    !> Indicate how FILES are checked out.
    !>
    !> If FILES are not registered, this function always returns
    !> VERSION_CONTROL_MODEL_UNKNOWN.  For registered files, the possible values are:
    !>
    !>   VERSION_CONTROL_MODEL_IMPLICIT   FILES are always writable, and checked out `implicitly'
    !>               when the user saves the first changes to the file.
    !>
    !>   VERSION_CONTROL_MODEL_LOCKING   FILES are read-only if up-to-date; user must type
    !>               C-x v v before editing.  Strict locking
    !>               is assumed.
    !>
    !>   VERSION_CONTROL_MODEL_ANNOUNCE   FILES are read-only if up-to-date; user must type
    !>               C-x v v before editing.  But other users
    !>               may be editing at the same time.  Port of
    !>               vc-hooks.el:vc-checkout-model.
    integer function version_control_checkout_model(b, files) result(model)
        integer, intent(in) :: b
        type(string_type), intent(in) :: files(:)

        model = VERSION_CONTROL_MODEL_UNKNOWN
        if (b <= 0) return
        if (associated(version_control_backends(b)%checkout_model)) model = version_control_backends(b)%checkout_model(files)
    end function version_control_checkout_model

    !> Return the version control state of FILE.
    !>
    !> A return of VERSION_CONTROL_STATE_UNKNOWN from this function means we have no information on the
    !> status of this file.  Otherwise, the value returned is one of:
    !>
    !>   VERSION_CONTROL_UP_TO_DATE       The working file is unmodified with respect to the
    !>                      latest version on the current branch, and not locked.
    !>
    !>   VERSION_CONTROL_EDITED           The working file has been edited by the user.  If
    !>                      locking is used for the file, this state means that
    !>                      the current version is locked by the calling user.
    !>                      This status should *not* be reported for files
    !>                      which have a changed mtime but the same content
    !>                      as the repo copy.
    !>
    !>   VERSION_CONTROL_LOCKED           The current version of the working file is locked by
    !>                      some other USER (a string, given as LOCKER).
    !>
    !>   VERSION_CONTROL_NEEDS_UPDATE     The file has not been edited by the user, but there is
    !>                      a more recent version on the current branch stored
    !>                      in the repository.
    !>
    !>   VERSION_CONTROL_NEEDS_MERGE      The file has been edited by the user, and there is also
    !>                      a more recent version on the current branch stored in
    !>                      the repository.  This state can only occur if locking
    !>                      is not used for the file.
    !>
    !>   VERSION_CONTROL_UNLOCKED_CHANGES The working version of the file is not locked,
    !>                      but the working file has been changed with respect
    !>                      to that version.  This state can only occur for files
    !>                      with locking; it represents an erroneous condition that
    !>                      should be resolved by the user (version_control_next_action will
    !>                      prompt the user to do it).
    !>
    !>   VERSION_CONTROL_ADDED            Scheduled to go into the repository on the next commit.
    !>                      Often represented by version_control_working_revision = "0" in VCSes
    !>                      with monotonic IDs like Subversion and Mercurial.
    !>
    !>   VERSION_CONTROL_REMOVED          Scheduled to be deleted from the repository on next commit.
    !>
    !>   VERSION_CONTROL_CONFLICT         The file contains conflicts as the result of a merge.
    !>                      For now the conflicts are text conflicts.  In the
    !>                      future this might be extended to deal with metadata
    !>                      conflicts too.
    !>
    !>   VERSION_CONTROL_MISSING          The file is not present in the file system, but the VERSION_CONTROL
    !>                      system still tracks it.
    !>
    !>   VERSION_CONTROL_IGNORED          The file showed up in a dir-status listing with a flag
    !>                      indicating the version-control system is ignoring it,
    !>                      Note: This property is not set reliably (some VCSes
    !>                      don't have useful directory-status commands) so assume
    !>                      that any file with version_control_state VERSION_CONTROL_STATE_UNKNOWN might be ignorable
    !>                      without VERSION_CONTROL knowing it.
    !>
    !>   VERSION_CONTROL_UNREGISTERED     The file is not under version control.
    !>
    !> Port of vc-hooks.el:vc-state.
    recursive integer function version_control_state(file, backend, locker) result(state)
        character(len=*), intent(in) :: file
        integer, intent(in), optional :: backend
        character(len=:), allocatable, intent(out), optional :: locker
        integer :: b

        if (present(locker)) then
            state = version_control_file_get_state(file, locker)
        else
            state = version_control_file_get_state(file)
        end if

        if (state /= VERSION_CONTROL_STATE_UNKNOWN) return
        if (len(file) == 0) return
        b = 0
        if (present(backend)) b = backend
        if (b == 0) b = version_control_backend(file)
        if (b == 0) return
        state = version_control_state_refresh(file, b)
        if (present(locker)) locker = ""
    end function version_control_state

    !> Quickly recompute the `state' of FILE.  Port of
    !> vc-hooks.el:vc-state-refresh (the state set is returned).
    recursive integer function version_control_state_refresh(file, b) result(state)
        character(len=*), intent(in) :: file
        integer, intent(in) :: b

        state = VERSION_CONTROL_STATE_UNKNOWN
        if (associated(version_control_backends(b)%state)) state = version_control_backends(b)%state(file)
        call version_control_file_set_state(file, state)
    end function version_control_state_refresh

    !> Convenience function to check whether `version_control_state' of FILE is
    !> VERSION_CONTROL_UP_TO_DATE.  Port of vc-hooks.el:vc-up-to-date-p.
    logical function is_version_control_up_to_date(file)
        character(len=*), intent(in) :: file

        is_version_control_up_to_date = version_control_state(file) == VERSION_CONTROL_UP_TO_DATE
    end function is_version_control_up_to_date

    !> The name of state STATE as upstream's symbol (`up-to-date'), or
    !> the user LOCKER of VERSION_CONTROL_LOCKED.  Not a port.
    function version_control_state_name(state, locker) result(name)
        integer, intent(in) :: state
        character(len=*), intent(in), optional :: locker
        character(len=:), allocatable :: name

        select case (state)
        case (VERSION_CONTROL_UP_TO_DATE)
            name = "up-to-date"
        case (VERSION_CONTROL_EDITED)
            name = "edited"
        case (VERSION_CONTROL_LOCKED)
            name = ""
            if (present(locker)) name = locker
        case (VERSION_CONTROL_NEEDS_UPDATE)
            name = "needs-update"
        case (VERSION_CONTROL_NEEDS_MERGE)
            name = "needs-merge"
        case (VERSION_CONTROL_UNLOCKED_CHANGES)
            name = "unlocked-changes"
        case (VERSION_CONTROL_ADDED)
            name = "added"
        case (VERSION_CONTROL_REMOVED)
            name = "removed"
        case (VERSION_CONTROL_CONFLICT)
            name = "conflict"
        case (VERSION_CONTROL_MISSING)
            name = "missing"
        case (VERSION_CONTROL_IGNORED)
            name = "ignored"
        case (VERSION_CONTROL_UNREGISTERED)
            name = "unregistered"
        case default
            name = "nil"
        end select
    end function version_control_state_name

    !> Return the repository version from which FILE was checked out
    !> (FOUND false if FILE is not registered).
    !>
    !> This function does not return no revision without first confirming with the
    !> underlying VERSION_CONTROLS that FILE is unregistered; this is in contrast to
    !> `version_control_symbolic_working_revision'.  Port of vc-hooks.el:vc-working-revision.
    subroutine version_control_working_revision(file, revision, found, backend)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: revision
        logical, intent(out) :: found
        integer, intent(in), optional :: backend
        character(len=:), allocatable :: abs, saved_directory
        type(status_type) :: st
        integer :: b

        call expand_file_name(file, abs, st)
        if (.not. st%ok()) abs = file
        call version_control_file_get_working_revision(abs, revision, found)
        if (found) return
        b = 0
        if (present(backend)) b = backend
        if (b == 0) b = version_control_backend(file)
        if (b == 0) return
        if (.not. associated(version_control_backends(b)%working_revision)) return
        ! (let ((default-directory (file-name-directory abs))) ...)
        saved_directory = buffer_directory()
        call set_buffer_directory(file_name_directory(abs))
        call version_control_backends(b)%working_revision(file, revision, found)
        call set_buffer_directory(saved_directory)

        if (found) then
            call version_control_file_set_working_revision(abs, revision)
        else
            call version_control_file_set_working_revision(abs)
        end if
    end subroutine version_control_working_revision

    !> Return BACKEND's symbolic name for FILE's working revision.
    !> If FILE names an existing directory, the BACKEND argument is mandatory.
    !> If FILE is not registered according to cached information or names an
    !> existing directory and BACKEND is absent, FOUND is false.
    !> If BACKEND does not have a symbolic name for the working revision or
    !> Fortmacs doesn't know what it is, call `version_control_working_revision' instead.
    !>
    !> Prefer this function to `version_control_working_revision' whenever a symbolic name
    !> will do, for it avoids a call out to the underlying VERSION_CONTROLS.  Port of
    !> vc-hooks.el:vc-symbolic-working-revision.
    subroutine version_control_symbolic_working_revision(file, revision, found, backend)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: revision
        logical, intent(out) :: found
        integer, intent(in), optional :: backend
        integer :: b, cached

        revision = ""
        found = .false.
        b = 0
        if (present(backend)) b = backend
        cached = 0

        if (.not. (is_file_directory(file) .and. b /= 0)) then
            cached = version_control_backend(file)
            if (cached == 0) return
        end if

        if (b == 0) b = cached

        if (associated(version_control_backends(b)%working_revision_symbol)) then
            call version_control_backends(b)%working_revision_symbol(revision)
            found = .true.
        else
            call version_control_working_revision(file, revision, found, b)
        end if
    end subroutine version_control_symbolic_working_revision

    !> Return the repository version for FILE in a shortened form (FOUND
    !> false if FILE is not registered).  Port of
    !> vc-hooks.el:vc-short-revision.
    subroutine version_control_short_revision(file, revision, found, backend)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: revision
        logical, intent(out) :: found
        integer, intent(in), optional :: backend
        logical :: saved
        integer :: b

        revision = ""
        found = .false.
        b = 0
        if (present(backend)) b = backend
        if (b == 0) b = version_control_backend(file)
        if (b == 0) return
        if (.not. associated(version_control_backends(b)%working_revision)) return
        saved = version_control_use_short_revision
        version_control_use_short_revision = .true.
        call version_control_backends(b)%working_revision(file, revision, found)
        version_control_use_short_revision = saved
    end subroutine version_control_short_revision

    !> The current buffer's `default_directory'.  Not a port.
    function buffer_directory() result(directory)
        character(len=:), allocatable :: directory

        directory = "./"
        if (current == 0) return
        if (allocated(buffers(current)%directory)) directory = buffers(current)%directory
    end function buffer_directory

    !> Set the current buffer's `default_directory' to DIRECTORY (a let-binding
    !> undone by setting it back).  Not a port.
    subroutine set_buffer_directory(directory)
        character(len=*), intent(in) :: directory

        if (current == 0) return
        if (len(directory) > 0) buffers(current)%directory = directory
    end subroutine set_buffer_directory

    ! ------------------------------------------------------------------
    ! Version backups
    ! ------------------------------------------------------------------

    !> Return a backup file name for REVISION or the current version of FILE.
    !> If MANUAL is true it means that a name for backups created by
    !> the user should be returned; if REGULAR_EXPRESSION is true that means to return
    !> a regexp for matching all such backup files, regardless of the version.
    !> Port of vc-hooks.el:vc-version-backup-file-name.
    function version_control_version_backup_file_name(file, revision, manual, regular_expression) result(name)
        use search_mod, only: regular_expression_quote
        character(len=*), intent(in) :: file
        character(len=*), intent(in), optional :: revision
        logical, intent(in), optional :: manual, regular_expression
        character(len=:), allocatable :: name
        character(len=:), allocatable :: r, dot
        type(status_type) :: st
        logical :: man, found
        integer :: k

        man = .false.
        if (present(manual)) man = manual
        dot = "."
        if (man) dot = ""

        if (present(regular_expression)) then
            if (regular_expression) then
                name = regular_expression_quote(file_name_nondirectory(file))//"\.~.+"
                if (.not. man) name = name//"\."
                name = name//"~"
                return
            end if
        end if

        if (present(revision)) then
            r = revision
        else
            call version_control_working_revision(file, r, found)
        end if

        do k = 1, len(r)
            if (r(k:k) == "/") r(k:k) = "_"
        end do

        call expand_file_name(file_name_nondirectory(file)//".~"//r//dot//"~", name, st, file_name_directory_or_default(file))
        if (.not. st%ok()) name = file_name_nondirectory(file)//".~"//r//dot//"~"
    end function version_control_version_backup_file_name

    !> The directory of FILE, or `default_directory' (expand-file-name's
    !> default for an absent DIRECTORY).  Not a port.
    function file_name_directory_or_default(file) result(directory)
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: directory

        directory = file_name_directory(file)
        if (len(directory) == 0) directory = buffer_directory()
    end function file_name_directory_or_default

    !> Delete all existing automatic version backups for FILE.  Port of
    !> vc-hooks.el:vc-delete-automatic-version-backups.
    subroutine version_control_delete_automatic_version_backups(file)
        use directory_editor_mod, only: directory_files
        use value_mod, only: value_type, list_elements, xstring
        character(len=*), intent(in) :: file
        type(value_type) :: names
        type(value_type), allocatable :: elements(:)
        type(status_type) :: st, mst
        character(len=:), allocatable :: pattern, directory
        integer :: k

        directory = file_name_directory_or_default(file)
        pattern = version_control_version_backup_file_name(file, regular_expression=.true.)
        ! Don't fail when the directory doesn't exist.
        call directory_files(directory, names, st, full=.true.)
        if (.not. st%ok()) return
        call list_elements(names, elements)

        do k = 1, size(elements)
            if (string_match(pattern, file_name_nondirectory(xstring(elements(k))), mst) > 0) then
                call delete_file(xstring(elements(k)), st)
            end if
        end do
    end subroutine version_control_delete_automatic_version_backups

    !> Make a backup copy of FILE, which is assumed in sync with the repository.
    !> Before doing that, check if there are any old backups and get rid of them.
    !> Port of vc-hooks.el:vc-make-version-backup.
    subroutine version_control_make_version_backup(file)
        character(len=*), intent(in) :: file
        type(status_type) :: st

        call version_control_delete_automatic_version_backups(file)
        call copy_file(file, version_control_version_backup_file_name(file), st, keep_time=.true.)
        ! It's ok if it doesn't work (e.g. directory not writable), since
        ! this is just for efficiency.
        if (.not. st%ok()) call message("Warning: Cannot make version backup; diff/revert therefore not local")
    end subroutine version_control_make_version_backup

    !> Function to be called by `basic_save_buffer' (in files_mod).  Port of
    !> vc-hooks.el:vc-before-save.
    subroutine version_control_before_save()
        character(len=:), allocatable :: file
        type(string_type) :: files(1)
        integer :: b

        ! If the file on disk is still in sync with the repository,
        ! and version backups should be made, copy the file to
        ! another name.  This enables local diffs and local reverting.
        if (current == 0) return
        if (.not. allocated(buffers(current)%filename)) return
        file = buffers(current)%filename
        if (.not. file_exists(file)) call version_control_file_clearprops(file)
        b = version_control_backend(file)
        if (b == 0) return
        if (.not. is_version_control_up_to_date(file)) return
        files(1)%text = file
        if (version_control_checkout_model(b, files) /= VERSION_CONTROL_MODEL_IMPLICIT) return
        if (.not. is_version_control_make_version_backups(b, file)) return
        call version_control_make_version_backup(file)
    end subroutine version_control_before_save

    !> Function to be called by `basic_save_buffer' (in files_mod).  Port of
    !> vc-hooks.el:vc-after-save (no *vc-dir* buffers).
    subroutine version_control_after_save()
        character(len=:), allocatable :: file
        type(string_type) :: files(1)
        type(file_status_type) :: attribute
        integer(int64) :: second
        integer(int32) :: nsec
        logical :: found, has_attribute
        integer :: b, state

        ! If the file in the current buffer is under version control,
        ! up-to-date, and locking is not used for the file, set
        ! the state to 'edited and redisplay the mode line.
        if (current == 0) return
        if (.not. allocated(buffers(current)%filename)) return
        file = buffers(current)%filename
        b = version_control_backend(file)
        files(1)%text = file

        if (b == 0) then
            continue
        else if (version_control_checkout_model(b, files) == VERSION_CONTROL_MODEL_IMPLICIT) then
            ! If the file was saved at the same time that it was
            ! checked out, clear the checkout-time to avoid confusion.
            call version_control_file_get_checkout_time(file, second, nsec, found)
            call file_attributes(file, attribute, has_attribute)

            if (found .and. has_attribute) then
                if (second == attribute%mtime_second .and. nsec == attribute%mtime_nsec) then
                    call version_control_file_set_checkout_time(file)
                end if
            end if

            state = version_control_state_refresh(file, b)
            if (state /= VERSION_CONTROL_STATE_UNKNOWN) call version_control_mode_line(file, b)
        else if (version_control_file_get_state(file) == VERSION_CONTROL_UP_TO_DATE) then
            ! If we saved an unlocked file on a locking based VERSION_CONTROLS, that
            ! file is not longer up-to-date.
            call version_control_file_set_state(file, VERSION_CONTROL_STATE_UNKNOWN)
        end if

        ! Resynch *vc-dir* buffers, if any are present.
        if (has_version_control_directory_buffers() .and. associated(version_control_directory_resynch_file)) then
            call version_control_directory_resynch_file(file)
        end if
    end subroutine version_control_after_save

    !> Update VC-Dir contents after reverting a buffer from disk.  Port of
    !> vc-hooks.el:vc-after-revert.
    recursive subroutine version_control_after_revert(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file

        status = status_type()
        if (.not. has_version_control_directory_buffers()) return
        if (current == 0) return
        if (.not. allocated(buffers(current)%filename)) return
        ! A copy: the backend can grow the buffer registry (see version_control_refresh_state).
        file = buffers(current)%filename
        if (version_control_backend(file) == 0) return
        if (associated(version_control_directory_resynch_file)) call version_control_directory_resynch_file(file)
    end subroutine version_control_after_revert

    !> Whether `version_control_directory_buffers' is not empty.  Not a port.
    logical function has_version_control_directory_buffers()
        has_version_control_directory_buffers = .false.
        if (allocated(version_control_directory_buffers)) has_version_control_directory_buffers = size( &
                                                                                              version_control_directory_buffers) > 0
    end function has_version_control_directory_buffers

    ! ------------------------------------------------------------------
    ! The mode line
    ! ------------------------------------------------------------------

    !> Set `version_control_mode' to display type of version control for FILE.
    !> The value is set in the current buffer, which should be the buffer
    !> visiting FILE.
    !> If BACKEND is passed use it as the VERSION_CONTROL backend when computing the
    !> result; USED is the backend (0 for none).  Port of
    !> vc-hooks.el:vc-mode-line.
    subroutine version_control_mode_line(file, backend, used)
        character(len=*), intent(in) :: file
        integer, intent(in), optional :: backend
        integer, intent(out), optional :: used
        character(len=:), allocatable :: ml_string
        integer :: b
        logical :: found
        integer(int32) :: modes

        b = 0
        if (present(backend)) b = backend
        if (b == 0) b = version_control_backend(file)

        if (b == 0) then
            if (allocated(buffers(current)%version_control_mode)) deallocate (buffers(current)%version_control_mode)
        else if (version_control_display_status%is_off()) then
            buffers(current)%version_control_mode = " "//version_control_backend_name(b)
        else
            if (associated(version_control_backends(b)%mode_line_string)) then
                call version_control_backends(b)%mode_line_string(file, ml_string, found)
            else
                ml_string = version_control_default_mode_line_string(b, file)
            end if

            buffers(current)%version_control_mode = " "//ml_string

            ! If the user is root, and the file is not owner-writable,
            ! then pretend that we can't write it
            ! even though we can (because root can write anything).
            ! This way, even root cannot modify a file that isn't locked.
            if (allocated(buffers(current)%filename)) then
                if (file == buffers(current)%filename .and. .not. buffers(current)%read_only .and. user_real_uid() == 0) then
                    modes = file_modes(buffers(current)%filename)
                    if (modes >= 0 .and. iand(modes, 128_int32) == 0) buffers(current)%read_only = .true.
                end if
            end if
        end if

        if (present(used)) used = b
    end subroutine version_control_mode_line

    !> The data to display on the mode line for STATE (LOCKER for
    !> VERSION_CONTROL_LOCKED), the version control state returned from `version_control_state':
    !> the echo string, the face name, and the indicator that usually is
    !> one character.  Port of vc-hooks.el:vc-mode-line-state.
    subroutine version_control_mode_line_state(state, state_echo, face, indicator, locker)
        integer, intent(in) :: state
        character(len=:), allocatable, intent(out) :: state_echo, face, indicator
        character(len=*), intent(in), optional :: locker

        select case (state)
        case (VERSION_CONTROL_UP_TO_DATE, VERSION_CONTROL_NEEDS_UPDATE)
            state_echo = "Up to date file"
            face = "version_control_up_to_date_state"
            indicator = "-"
        case (VERSION_CONTROL_LOCKED)
            if (present(locker)) then
                state_echo = "File locked by"//locker
                indicator = ":"//locker//":"
            else
                state_echo = "File locked by"
                indicator = "::"
            end if

            face = "version_control_locked_state"
        case (VERSION_CONTROL_ADDED)
            state_echo = "Locally added file"
            face = "version_control_locally_added_state"
            indicator = "@"
        case (VERSION_CONTROL_CONFLICT)
            state_echo = "File contains conflicts after the last merge"
            face = "version_control_conflict_state"
            indicator = "!"
        case (VERSION_CONTROL_REMOVED)
            state_echo = "File removed from the VC system"
            face = "version_control_removed_state"
            indicator = "!"
        case (VERSION_CONTROL_MISSING)
            state_echo = "File tracked by the VC system, but missing from the file system"
            face = "version_control_missing_state"
            indicator = "?"
        case (VERSION_CONTROL_IGNORED)
            state_echo = "File tracked by the VC system, but ignored"
            face = "version_control_ignored_state"
            indicator = "!"
        case default
            ! Not just for the 'edited state, but also a fallback
            ! for all other states.  Think about different symbols
            ! for 'needs-update and 'needs-merge.
            state_echo = "Locally modified file"
            face = "version_control_edited_state"
            indicator = ":"
        end select
    end subroutine version_control_mode_line_state

    !> Return a string for `version_control_mode_line' to put in the mode line for FILE.
    !> Format:
    !>
    !>   "BACKEND-REV"        if the file is up-to-date
    !>   "BACKEND:REVISION"        if the file is edited (or locked by the calling user)
    !>   "BACKEND:LOCKER:REVISION" if the file is locked by somebody else
    !>   "BACKEND@REVISION"        if the file was locally added
    !>   "BACKEND!REVISION"        if the file contains conflicts or was removed
    !>   "BACKEND?REVISION"        if the file is under VERSION_CONTROL, but is missing
    !>
    !> This function assumes that the file is registered.  Port of
    !> vc-hooks.el:vc-default-mode-line-string.
    function version_control_default_mode_line_string(b, file) result(s)
        integer, intent(in) :: b
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: s
        character(len=:), allocatable :: revision, echo, face, indicator, locker
        logical :: found
        integer :: state

        state = version_control_state(file, b, locker)
        call version_control_symbolic_working_revision(file, revision, found, b)
        call version_control_mode_line_state(state, echo, face, indicator, locker)
        s = ""
        if (version_control_display_status%choice /= VERSION_CONTROL_DISPLAY_STATUS_NO_BACKEND) s = version_control_backend_name(b)
        s = s//indicator//revision
    end function version_control_default_mode_line_string

    !> If current buffer visits a symbolic link, visit the real file.
    !> If the real file is already visited in another buffer, make that buffer
    !> current, and close the buffer that visits the link.  Port of
    !> vc-hooks.el:vc-follow-link.
    subroutine version_control_follow_link()
        character(len=:), allocatable :: truename
        type(handle_type) :: true_buffer, this_buffer, buffer
        type(status_type) :: st

        call file_truename(buffers(current)%filename, truename, st)
        if (.not. st%ok()) return
        true_buffer = get_file_buffer(truename)
        this_buffer = current_buffer()

        if (true_buffer%index == 0 .or. true_buffer%index == this_buffer%index) then
            call close_buffer(this_buffer, st)
            ! In principle, we could do something like set-visited-file-name.
            ! However, it can't be exactly the same as set-visited-file-name.
            ! I'm not going to work out the details right now. -- rms.
            call find_file_noselect(truename, buffer, st)
            if (st%ok()) call set_buffer(buffer, st)
        else
            call set_buffer(true_buffer, st)
            call close_buffer(this_buffer, st)
        end if
    end subroutine version_control_follow_link

    !> Refresh the VERSION_CONTROL state of the current buffer's file.
    !>
    !> This command is more thorough than `version_control_state_refresh', in that it
    !> also supports switching a back-end or removing the file from VERSION_CONTROL.
    !> In the latter case, VERSION_CONTROL mode is deactivated for this buffer.  Port of
    !> vc-hooks.el:vc-refresh-state (errors are shown as "VERSION_CONTROL refresh
    !> error: ..." messages, as with-demoted-errors does).
    recursive subroutine version_control_refresh_state(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: truename, saved_directory, filename
        type(status_type) :: st
        integer :: b, link_type
        logical :: yes

        status = status_type()
        ! Recompute whether file is version controlled,
        ! if user has killed the buffer and revisited.
        if (current == 0) return
        if (allocated(buffers(current)%version_control_mode)) deallocate (buffers(current)%version_control_mode)
        if (.not. allocated(buffers(current)%filename)) return
        ! A copy: the backends make temporary buffers, which can grow
        ! (reallocate) the buffer registry under a component argument.
        filename = buffers(current)%filename
        call version_control_file_clearprops(filename)
        b = version_control_backend(filename)

        if (b /= 0) then
            saved_directory = buffer_directory()
            ! Let the backend setup any buffer-local things it needs.
            if (associated(version_control_backends(b)%find_file_hook)) call version_control_backends(b)%find_file_hook()

            ! Compute the state and put it in the mode line.
            if (current /= 0) then
                if (allocated(buffers(current)%filename)) then
                    filename = buffers(current)%filename
                    call version_control_mode_line(filename, b)
                end if
            end if

            call set_buffer_directory(saved_directory)
            ! Use this variable, not make-backup-files, because this is
            ! for things that depend on the file name.
            if (.not. version_control_create_backup_files) buffers(current)%backup_inhibited = .true.
            return
        end if

        call file_truename(filename, truename, st)
        if (.not. st%ok()) return
        link_type = 0
        if (truename /= filename) link_type = version_control_backend(truename)
        if (link_type == 0) return

        if (version_control_follow_symlinks%is_off()) then
            call message("Warning: symbolic link to "//version_control_backend_name(link_type)//"-controlled source file")
        else if (version_control_follow_symlinks%choice /= VERSION_CONTROL_FOLLOW_SYMLINKS_ASK .or. already_followed()) then
            call version_control_follow_link()
            call message("Followed link to "//buffers(current)%filename)
            call version_control_refresh_state(status)
        else
            call read_yes_or_no("Symbolic link to "//version_control_backend_name(link_type)// &
                                "-controlled source file; follow link? ", yes, status)
            if (.not. status%ok()) return

            if (yes) then
                call version_control_follow_link()
                call message("Followed link to "//buffers(current)%filename)
                call version_control_refresh_state(status)
            else
                call message("Warning: editing through the link bypasses version control")
            end if
        end if

    contains

        !> If we already visited this file by following the link, don't
        !> ask again if we try to visit it again.
        logical function already_followed()
            type(handle_type) :: h1, h2

            h1 = get_file_buffer(abbreviate_file_name(truename))
            h2 = get_file_buffer(truename)
            already_followed = h1%index /= 0 .or. h2%index /= 0
        end function already_followed
    end subroutine version_control_refresh_state

    !> `version_control_refresh_state' as the find-file-hook function.  Not a port.
    recursive subroutine refresh_state_hook(status)
        type(status_type), intent(out) :: status

        call version_control_refresh_state(status)

        if (.not. status%ok()) then
            call message("VC refresh error: "//format_error(status))
            status = status_type()
        end if
    end subroutine refresh_state_hook

    !> Discard VERSION_CONTROL info about a file when we close its buffer.  Port of
    !> vc-hooks.el:vc-kill-buffer-hook.
    recursive subroutine version_control_close_buffer_hook(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (current == 0) return
        if (allocated(buffers(current)%filename)) call version_control_file_clearprops(buffers(current)%filename)
    end subroutine version_control_close_buffer_hook

    recursive subroutine execute_version_control_refresh_state(status)
        type(status_type), intent(out) :: status

        call version_control_refresh_state(status)
    end subroutine execute_version_control_refresh_state

    ! ------------------------------------------------------------------
    ! Setup
    ! ------------------------------------------------------------------

    ! Define the faces of VERSION_CONTROL's state indicator.  Not a port.
    subroutine define_faces()
        type(named_face_type), pointer :: f

        f => version_control_state_base
        call f%define("version_control_state_base", "Base face for VC state indicator.")
        call f%specification_entry(display_when(is_default=.true.), face_attributes())
        f => version_control_up_to_date_state
        call f%define("version_control_up_to_date_state", "Face for VC modeline state when the file is up to date.")
        call f%specification_entry(display_when(is_default=.true.), face_attributes(inherit="version_control_state_base"))
        f => version_control_needs_update_state
        call f%define("version_control_needs_update_state", "Face for VC modeline state when the file needs update.")
        call f%specification_entry(display_when(is_default=.true.), face_attributes(inherit="version_control_state_base"))
        f => version_control_locked_state
        call f%define("version_control_locked_state", "Face for VC modeline state when the file locked.")
        call f%specification_entry(display_when(is_default=.true.), face_attributes(inherit="version_control_state_base"))
        f => version_control_locally_added_state
        call f%define("version_control_locally_added_state", "Face for VC modeline state when the file is locally added.")
        call f%specification_entry(display_when(is_default=.true.), face_attributes(inherit="version_control_state_base"))
        f => version_control_conflict_state
        call f%define("version_control_conflict_state", "Face for VC modeline state when the file contains merge conflicts.")
        call f%specification_entry(display_when(is_default=.true.), face_attributes(inherit="version_control_state_base"))
        f => version_control_removed_state
        call f%define("version_control_removed_state", "Face for VC modeline state when the file was removed from the VC system.")
        call f%specification_entry(display_when(is_default=.true.), face_attributes(inherit="version_control_state_base"))
        f => version_control_missing_state
        call f%define("version_control_missing_state", &
                      "Face for VC modeline state when the file is missing from the file system.")
        call f%specification_entry(display_when(is_default=.true.), face_attributes(inherit="version_control_state_base"))
        f => version_control_edited_state
        call f%define("version_control_edited_state", "Face for VC modeline state when the file is edited.")
        call f%specification_entry(display_when(is_default=.true.), face_attributes(inherit="version_control_state_base"))
        f => version_control_ignored_state
        call f%define("version_control_ignored_state", "Face for VC modeline state when the file is registered, but ignored.")
        call f%specification_entry(display_when(is_default=.true.), face_attributes(inherit="version_control_state_base"))
    end subroutine define_faces

    !> Set up VERSION_CONTROL's resident part: the options, faces, hooks and
    !> `version_control_prefix_map' on C-x v in CONTROL_X_MAP.  Not a port.
    subroutine initialize_version_control_hooks(control_x_map)
        use value_mod, only: value_type
        type(handle_type), intent(in) :: control_x_map
        type(value_type) :: key(1)
        type(status_type) :: st

        if (.not. allocated(version_control_ignore_directory_regular_expression)) then
            version_control_ignore_directory_regular_expression = &
                locate_dominating_stop_directory_regular_expression
        end if

        if (.not. allocated(version_control_handled_backends)) then
            version_control_handled_backends = [character(len=8) :: "RCS", "CVS", "SVN", "SCCS", "SRC", "Bzr", "Git", "Hg"]
        end if

        if (.not. allocated(version_control_directory_exclusions)) then
            version_control_directory_exclusions = [character(len=8) :: "SCCS", "RCS", "CVS", "MCVS", ".src", ".svn", ".git", &
                                                    ".hg", ".bzr", "_MTN", "_darcs", "{arch}", ".repo", ".jj"]
        end if

        if (.not. allocated(properties)) allocate (properties(0))
        call define_faces()
        commands%version_control_refresh_state%execute => execute_version_control_refresh_state
        call find_file_hook%prepend(refresh_state_hook)
        call close_buffer_hook%prepend(version_control_close_buffer_hook)
        version_control_before_save_hook => version_control_before_save
        version_control_after_save_hook => version_control_after_save
        if (.not. after_revert_hook%member(version_control_after_revert)) call after_revert_hook%prepend( &
            version_control_after_revert)
        call make_sparse_keymap(version_control_prefix_map)
        call keymap_set(version_control_prefix_map, "G", "version_control_ignore", st)
        call keymap_set(version_control_prefix_map, "i", "version_control_register", st)
        call keymap_set(version_control_prefix_map, "u", "version_control_revert", st)
        call keymap_set(version_control_prefix_map, "@", "version_control_revert", st)
        call keymap_set(version_control_prefix_map, "v", "version_control_next_action", st)
        call keymap_set(version_control_prefix_map, "=", "version_control_diff", st)
        call keymap_set(version_control_prefix_map, "D", "version_control_root_diff", st)
        call keymap_set(version_control_prefix_map, "d", "version_control_directory", st)
        call keymap_set(version_control_prefix_map, "l", "version_control_print_log", st)
        call keymap_set(version_control_prefix_map, "L", "version_control_print_root_log", st)
        call keymap_set(version_control_prefix_map, "~", "version_control_revision_other_window", st)
        call keymap_set(version_control_prefix_map, "R", "version_control_rename_file", st)
        call keymap_set(version_control_prefix_map, "x", "version_control_delete_file", st)
        ! Tags, branches, pulling and pushing, incoming and outgoing.
        call keymap_set(version_control_prefix_map, "b c", "version_control_create_branch", st)
        call keymap_set(version_control_prefix_map, "b l", "version_control_print_fileset_branch_log", st)
        call keymap_set(version_control_prefix_map, "b L", "version_control_print_root_branch_log", st)
        call keymap_set(version_control_prefix_map, "b s", "version_control_switch_branch", st)
        call keymap_set(version_control_prefix_map, "I", "version_control_root_log_incoming", st)
        call keymap_set(version_control_prefix_map, "O", "version_control_root_log_outgoing", st)
        call keymap_set(version_control_prefix_map, "E L", "version_control_root_log_outgoing", st)
        call keymap_set(version_control_prefix_map, "r", "version_control_retrieve_tag", st)
        call keymap_set(version_control_prefix_map, "s", "version_control_create_tag", st)
        call keymap_set(version_control_prefix_map, "+", "version_control_update", st)
        call keymap_set(version_control_prefix_map, "P", "version_control_push", st)
        key(1) = value_type(int(iachar("v"), int64))
        call define_key(control_x_map, key, make_keymap_reference(version_control_prefix_map), st)
    end subroutine initialize_version_control_hooks
end module version_control_hooks_mod
