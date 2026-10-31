! vc_hooks_fm.f90 -- resident support for version-control.
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

!> The resident part of VC: the backends (a `vc_backend_type' each, with
!> its operations as procedure pointers, registered by the backend
!> modules), the per-file properties VC caches (backend, state, working
!> revision, checkout time), finding a file's backend, state and
!> revision, `vc_mode' in the mode line, `vc_refresh_state' on
!> `find_file_hook', the saving hooks of `basic_save_buffer', version
!> backups, and `vc_prefix_map' on C-x v.
!>
!> Deferred: the backends other than Git (`vc_handled_backends' still
!> names them; they are skipped as if none of their files were found),
!> file name handlers, the vc-*-state faces on the mode-line string (it
!> has no text properties: faces, help-echo and the mouse menu
!> `vc_mode_line_map'), the menus (`vc_menu_map'), `vc_auto_revert_mode'
!> (Auto-Revert mode), the *vc-dir* resynchronization after saving and
!> reverting (VC-Dir), `vc_use_incoming_outgoing_prefixes' and the
!> C-x v keys of the commands not ported yet.
module vc_hooks_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use commands_fm, only: commands
    use error_fm, only: status_type, set_error, format_error, FM_GENERIC_ERROR, FM_VC_NOT_SUPPORTED
    use handle_fm, only: handle_type
    use ewoc_fm, only: ewoc_part_type
    use lisp_fm, only: make_keymap_ref
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, kill_buffer, get_file_buffer, is_buffer_live
    use fileio_fm, only: expand_file_name, file_name_directory, file_name_nondirectory, file_name_as_directory, &
                         file_exists, is_file_directory, file_modes, copy_file, file_attributes, file_stat_type, &
                         user_real_uid
    use files_fm, only: locate_dominating_file, locate_dominating_stop_dir_regexp, find_file_hook, kill_buffer_hook, &
                        vc_before_save_proc, vc_after_save_proc, file_truename, find_file_noselect, &
                        abbreviate_file_name, delete_file, after_revert_hook
    use search_fm, only: string_match
    use keymap_fm, only: make_sparse_keymap, keymap_set, define_key
    use xdisp_fm, only: message
    use fns_fm, only: read_yes_or_no
    use faces_fm, only: named_face_type, display_when, face_attrs
    implicit none(type, external)
    private

    public :: syms_of_vc_hooks, register_vc_backend, vc_backend_named, vc_backend_name
    public :: vc_file_clearprops, vc_file_set_backend, vc_file_get_backend, vc_file_set_state, vc_file_get_state, &
              vc_file_set_working_revision, vc_file_get_working_revision, vc_file_set_checkout_time, &
              vc_file_get_checkout_time, vc_file_set_git_symbolic_ref, vc_file_get_git_symbolic_ref, &
              vc_clear_context, vc_touch_reset, vc_touched
    public :: vc_find_root, vc_registered, vc_backend, vc_checkout_model, vc_state, vc_state_refresh, &
              is_vc_up_to_date, vc_working_revision, vc_symbolic_working_revision, vc_short_revision, &
              vc_version_backup_file_name, vc_delete_automatic_version_backups, vc_make_version_backup, &
              vc_before_save, vc_after_save, vc_mode_line, vc_mode_line_state, vc_default_mode_line_string, &
              vc_refresh_state, vc_state_name, vc_call_root, vc_call_responsible_p, is_vc_call_make_version_backups, &
              vc_not_supported
    public :: vc_string_type, vc_backend_type, vc_backends, vc_dir_update_proc

    !> A string in an array.
    type :: vc_string_type
        character(len=:), allocatable :: s
    end type vc_string_type

    !> The kinds of a switches option (vc-diff-switches and the like):
    !> nil (unspecified: try the next option), t (no switches at all), or
    !> a list of strings (a string is a list of one).
    integer, parameter, public :: VC_SWITCHES_NIL = 0, VC_SWITCHES_T = 1, VC_SWITCHES_LIST = 2

    !> The value of a switches option: its kind and, for
    !> VC_SWITCHES_LIST, the switches.
    type, public :: vc_switches_type
        integer :: kind = VC_SWITCHES_NIL
        type(vc_string_type), allocatable :: list(:)
    end type vc_switches_type

    ! ------------------------------------------------------------------
    ! Customization variables (the rest is in vc_fm)
    ! ------------------------------------------------------------------

    !> Regexp matching directory names that are not under VC's control.
    !> The default regexp prevents fruitless and time-consuming attempts
    !> to determine the VC status in directories in which filenames are
    !> interpreted as hostnames.  Port of vc-hooks.el vc-ignore-dir-regexp.
    character(len=:), allocatable, public :: vc_ignore_dir_regexp

    !> List of version control backends for which VC will be used.
    !> Entries in this list will be tried in order to determine whether a
    !> file is under that sort of version control.
    !> Removing an entry from the list prevents VC from being activated
    !> when visiting a file managed by that backend.
    !> An empty list disables VC altogether.  Port of vc-hooks.el
    !> vc-handled-backends.
    character(len=8), allocatable, public :: vc_handled_backends(:)

    !> List of directory names to be ignored when walking directory trees.
    !> Port of vc-hooks.el vc-directory-exclusion-list.
    character(len=8), allocatable, public :: vc_directory_exclusion_list(:)

    !> If true, backups of registered files are made as with other files.
    !> If false (the default), files covered by version control don't get backups.
    !> Port of vc-hooks.el vc-make-backup-files.
    logical, public :: vc_make_backup_files = .false.

    !> Values of `vc_follow_symlinks' (nil, t, `ask').
    integer, parameter, public :: VC_FOLLOW_SYMLINKS_NIL = 0, VC_FOLLOW_SYMLINKS_T = 1, VC_FOLLOW_SYMLINKS_ASK = 2

    !> What to do if visiting a symbolic link to a file under version control.
    !> Editing such a file through the link bypasses the version control system,
    !> which is dangerous and probably not what you want.
    !>
    !> If this variable is VC_FOLLOW_SYMLINKS_T, VC follows the link and visits the real file,
    !> telling you about it in the echo area.  If it is VC_FOLLOW_SYMLINKS_ASK, VC asks for
    !> confirmation whether it should follow the link.  If VC_FOLLOW_SYMLINKS_NIL, the link is
    !> visited and a warning displayed.  Port of vc-hooks.el vc-follow-symlinks.
    integer, public :: vc_follow_symlinks = VC_FOLLOW_SYMLINKS_ASK

    !> Values of `vc_display_status' (nil, t, `no-backend').
    integer, parameter, public :: VC_DISPLAY_STATUS_NIL = 0, VC_DISPLAY_STATUS_T = 1, VC_DISPLAY_STATUS_NO_BACKEND = 2

    !> If true, display revision number and lock status in mode line.
    !> If false, only the backend name is displayed.  When the value
    !> is VC_DISPLAY_STATUS_NO_BACKEND, then no backend name is displayed before the
    !> revision number and lock status.  Port of vc-hooks.el vc-display-status.
    integer, public :: vc_display_status = VC_DISPLAY_STATUS_T

    !> If true, identify work files by searching for version headers.
    !> Port of vc-hooks.el vc-consult-headers.
    logical, public :: vc_consult_headers = .true.

    !> Whether to mark conflicted file as resolved upon saving.
    !>
    !> If this is true and there are no more conflict markers in the file,
    !> VC will mark the conflicts in the saved file as resolved.  This is
    !> only meaningful for VCS that handle conflicts by inserting conflict
    !> markers in a conflicted file.
    !>
    !> When saving a conflicted file, VC first tries to use the value
    !> of `vc_BACKEND_resolve_conflicts', for handling backend-specific
    !> settings.  It defaults to this option if that option has the special
    !> value `default'.  Port of vc-hooks.el vc-resolve-conflicts.
    logical, public :: vc_resolve_conflicts = .true.

    !> If true, VC backend functions should return short revisions if possible.
    !> This is set to true when calling `vc_short_revision', which will
    !> then call the `working_revision' backend function.  Port of
    !> vc-hooks.el vc-use-short-revision.
    logical, public :: vc_use_short_revision = .false.

    ! ------------------------------------------------------------------
    ! States and checkout models
    ! ------------------------------------------------------------------

    !> The version control states of `vc_state': none known (nil), and
    !> `up-to-date', `edited', locked by another user (USER, whose name
    !> `vc_state_locker' gives), `needs-update', `needs-merge',
    !> `unlocked-changes', `added', `removed', `conflict', `missing',
    !> `ignored' and `unregistered'.
    integer, parameter, public :: VC_STATE_NIL = 0, VC_UP_TO_DATE = 1, VC_EDITED = 2, VC_LOCKED = 3, &
                                  VC_NEEDS_UPDATE = 4, VC_NEEDS_MERGE = 5, VC_UNLOCKED_CHANGES = 6, VC_ADDED = 7, &
                                  VC_REMOVED = 8, VC_CONFLICT = 9, VC_MISSING = 10, VC_IGNORED = 11, &
                                  VC_UNREGISTERED = 12

    !> The checkout models of `vc_checkout_model': unknown (nil),
    !> `implicit', `locking' and `announce'.
    integer, parameter, public :: VC_MODEL_NIL = 0, VC_MODEL_IMPLICIT = 1, VC_MODEL_LOCKING = 2, VC_MODEL_ANNOUNCE = 3

    ! ------------------------------------------------------------------
    ! Backends
    ! ------------------------------------------------------------------

    !> Kinds of a log's LIMIT: none (nil, unlimited), a number of entries,
    !> or a revision before which to stop.
    integer, parameter, public :: VC_LIMIT_NONE = 0, VC_LIMIT_NUMBER = 1, VC_LIMIT_REVISION = 2

    !> The LIMIT of the log commands: nil, a number or a revision string.
    type, public :: vc_log_limit_type
        integer :: kind = VC_LIMIT_NONE
        integer(int64) :: n = 0
        character(len=:), allocatable :: revision
    end type vc_log_limit_type

    !> Rename states of a file's backend-specific information: none, a copy
    !> and a rename.
    integer, parameter, public :: VC_RENAME_NIL = 0, VC_RENAME_COPY = 1, VC_RENAME_RENAME = 2

    !> One (FILE STATE EXTRA) entry of a backend's `dir_status_files': the
    !> file's name relative to the directory, its state, and the
    !> backend-specific EXTRA information (Git's permissions and renames,
    !> vc-git-extra-fileinfo).
    type, public :: vc_dir_entry_type
        character(len=:), allocatable :: name
        integer :: state = 0
        logical :: has_extra = .false.
        integer :: old_perm = 0, new_perm = 0
        integer :: rename_state = VC_RENAME_NIL
        character(len=:), allocatable :: orig_name
    end type vc_dir_entry_type

    !> The information shown for a file or directory in a VC-Dir buffer
    !> (vc-dir-fileinfo): NAME, STATE, EXTRA (as in `vc_dir_entry_type'),
    !> MARKED, NEEDS_UPDATE (to keep track of not updated files during a
    !> global refresh), DIRECTORY (allocated for directories, to
    !> distinguish files and directories) and DISPLAY_STATE (a pseudo-state
    !> for display only, allocated when set).
    type, public :: vc_dir_fileinfo_type
        type(vc_dir_entry_type) :: entry
        logical :: marked = .false.
        logical :: needs_update = .false.
        character(len=:), allocatable :: directory
        character(len=:), allocatable :: display_state
    end type vc_dir_fileinfo_type

    abstract interface
        !> A backend's `registered': whether FILE is registered.
        !> Not a port.
        logical function vc_file_predicate(file)
            implicit none(type, external)
            character(len=*), intent(in) :: file
        end function vc_file_predicate
        !> A backend's `state'.  Not a port.
        integer function vc_file_state_function(file)
            implicit none(type, external)
            character(len=*), intent(in) :: file
        end function vc_file_state_function
        !> A backend function of FILE giving a string, or nil (FOUND false):
        !> `working_revision', `root', `responsible_p', `mode_line_string',
        !> `find_ignore_file'.  Not a port.
        subroutine vc_file_string_function(file, s, found)
            implicit none(type, external)
            character(len=*), intent(in) :: file
            character(len=:), allocatable, intent(out) :: s
            logical, intent(out) :: found
        end subroutine vc_file_string_function
        !> A backend's `checkout_model' of FILES.  Not a port.
        integer function vc_files_model_function(files)
            import :: vc_string_type
            implicit none(type, external)
            type(vc_string_type), intent(in) :: files(:)
        end function vc_files_model_function
        !> A backend's `find_file_hook', run in the visiting buffer.
        !> Not a port.
        subroutine vc_buffer_proc()
            implicit none(type, external)
        end subroutine vc_buffer_proc
        !> A backend's `working_revision_symbol', as S (a subroutine:
        !> gfortran frees a pointer component to a function with a
        !> deferred-length result).  Not a port.
        subroutine vc_symbol_function(s)
            implicit none(type, external)
            character(len=:), allocatable, intent(out) :: s
        end subroutine vc_symbol_function
        !> A backend operation on FILES: `revert_files', `delete_files',
        !> `mark_resolved'.  Not a port.
        recursive subroutine vc_files_proc(files, status)
            import :: vc_string_type, status_type
            implicit none(type, external)
            type(vc_string_type), intent(in) :: files(:)
            type(status_type), intent(out) :: status
        end subroutine vc_files_proc
        !> A backend's `register' of FILES (COMMENT ignored by most).
        !> Not a port.
        recursive subroutine vc_register_proc(files, status, comment)
            import :: vc_string_type, status_type
            implicit none(type, external)
            type(vc_string_type), intent(in) :: files(:)
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: comment
        end subroutine vc_register_proc
        !> A backend operation on FILE: `delete_file', `unregister'.
        !> Not a port.
        recursive subroutine vc_file_proc(file, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: file
            type(status_type), intent(out) :: status
        end subroutine vc_file_proc
        !> A backend's `revert' of FILE (CONTENTS_DONE, the backup file
        !> already copied back, or absent).  Not a port.
        recursive subroutine vc_revert_proc(file, status, contents_done)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: file
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: contents_done
        end subroutine vc_revert_proc
        !> A backend's `diff' of FILES between REV1 and REV2 (absent: the
        !> working revision and the work file) into BUFFER, asynchronously
        !> with ASYNC.  CHANGES is false when there are none (exit status 0
        !> of a synchronous diff).  Not a port.
        recursive subroutine vc_diff_proc(files, buffer, async, status, rev1, rev2)
            import :: vc_string_type, status_type, handle_type
            implicit none(type, external)
            type(vc_string_type), intent(in) :: files(:)
            type(handle_type), intent(in) :: buffer
            logical, intent(in) :: async
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: rev1, rev2
        end subroutine vc_diff_proc
        !> A backend's `find_revision': insert revision REV of FILE into
        !> BUFFER.  Not a port.
        recursive subroutine vc_find_revision_proc(file, rev, buffer, status)
            import :: status_type, handle_type
            implicit none(type, external)
            character(len=*), intent(in) :: file, rev
            type(handle_type), intent(in) :: buffer
            type(status_type), intent(out) :: status
        end subroutine vc_find_revision_proc
        !> A backend's `checkout' of FILE at REV (absent: the head).
        !> Not a port.
        recursive subroutine vc_checkout_proc(file, status, rev)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: file
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: rev
        end subroutine vc_checkout_proc
        !> A backend's `rename_file' from OLD to NEW.  Not a port.
        recursive subroutine vc_rename_proc(old, new, status)
            import :: status_type
            implicit none(type, external)
            character(len=*), intent(in) :: old, new
            type(status_type), intent(out) :: status
        end subroutine vc_rename_proc
        !> A backend's `previous_revision' of REV for FILE.  Not a port.
        subroutine vc_previous_revision_proc(file, rev, prev, found)
            implicit none(type, external)
            character(len=*), intent(in) :: file, rev
            character(len=:), allocatable, intent(out) :: prev
            logical, intent(out) :: found
        end subroutine vc_previous_revision_proc
        !> A backend's `revision_completion_table' for FILES.  Not a port.
        subroutine vc_revision_table_proc(files, table)
            import :: vc_string_type
            implicit none(type, external)
            type(vc_string_type), intent(in) :: files(:)
            type(vc_string_type), allocatable, intent(out) :: table(:)
        end subroutine vc_revision_table_proc
        !> A backend's `create_repo' in `default_directory'.  Not a port.
        recursive subroutine vc_status_proc(status)
            import :: status_type
            implicit none(type, external)
            type(status_type), intent(out) :: status
        end subroutine vc_status_proc
        !> A backend's `print_log': insert the log of FILES into BUFFER (a
        !> short log with SHORTLOG), starting at START_REVISION ("" for the
        !> current one), with LIMIT.  LIMIT_UNSUPPORTED is the return value
        !> `limit-unsupported'.  Not a port.
        recursive subroutine vc_print_log_proc(files, buffer, shortlog, start_revision, limit, status, &
                                               limit_unsupported)
            import :: vc_string_type, handle_type, vc_log_limit_type, status_type
            implicit none(type, external)
            type(vc_string_type), intent(in) :: files(:)
            type(handle_type), intent(in) :: buffer
            logical, intent(in) :: shortlog
            character(len=*), intent(in) :: start_revision
            type(vc_log_limit_type), intent(in) :: limit
            type(status_type), intent(out) :: status
            logical, intent(out) :: limit_unsupported
        end subroutine vc_print_log_proc
        !> The UPDATE function given to a backend's `dir_status_files': add
        !> ENTRIES to the VC-Dir buffer BUFFER; MORE_TO_COME is true when
        !> more updates will come from the asynchronous process.
        !> Not a port.
        recursive subroutine vc_dir_update_proc(entries, buffer, more_to_come)
            import :: vc_dir_entry_type, handle_type
            implicit none(type, external)
            type(vc_dir_entry_type), intent(in) :: entries(:)
            type(handle_type), intent(in) :: buffer
            logical, intent(in) :: more_to_come
        end subroutine vc_dir_update_proc
        !> A backend's `dir_status_files': compute the states of FILES (all
        !> files when empty) in DIR asynchronously and pass them to UPDATE
        !> for BUFFER.  Not a port.
        recursive subroutine vc_dir_status_files_proc(dir, files, update, buffer, status)
            import :: vc_string_type, handle_type, status_type, vc_dir_update_proc
            implicit none(type, external)
            character(len=*), intent(in) :: dir
            type(vc_string_type), intent(in) :: files(:)
            procedure(vc_dir_update_proc) :: update
            type(handle_type), intent(in) :: buffer
            type(status_type), intent(out) :: status
        end subroutine vc_dir_status_files_proc
        !> A backend's `dir_extra_headers' of DIR, as the parts of the text.
        !> Not a port.
        recursive subroutine vc_dir_extra_headers_proc(dir, parts)
            import :: ewoc_part_type
            implicit none(type, external)
            character(len=*), intent(in) :: dir
            type(ewoc_part_type), allocatable, intent(out) :: parts(:)
        end subroutine vc_dir_extra_headers_proc
        !> A backend's `dir_printer': insert INFO at point.  Not a port.
        recursive subroutine vc_dir_printer_proc(info)
            import :: vc_dir_fileinfo_type
            implicit none(type, external)
            type(vc_dir_fileinfo_type), intent(in) :: info
        end subroutine vc_dir_printer_proc
        !> A backend's `status_fileinfo_extra' of FILE, set in ENTRY.
        !> Not a port.
        recursive subroutine vc_status_fileinfo_extra_proc(file, entry)
            import :: vc_dir_entry_type
            implicit none(type, external)
            character(len=*), intent(in) :: file
            type(vc_dir_entry_type), intent(inout) :: entry
        end subroutine vc_status_fileinfo_extra_proc
        !> A backend's `show_log_entry': move to the log entry for REVISION.
        !> Not a port.
        recursive subroutine vc_show_log_entry_proc(revision)
            implicit none(type, external)
            character(len=*), intent(in) :: revision
        end subroutine vc_show_log_entry_proc
        !> A backend's `checkin' of FILES with COMMENT (REV, the new
        !> revision, for the backends that take one).  Not a port.
        recursive subroutine vc_checkin_proc(files, comment, status, rev)
            import :: vc_string_type, status_type
            implicit none(type, external)
            type(vc_string_type), intent(in) :: files(:)
            character(len=*), intent(in) :: comment
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: rev
        end subroutine vc_checkin_proc
    end interface

    !> A VC backend: its name (as in `vc_handled_backends') and the
    !> functions of the backend API it implements (vc-BACKEND-FUN;
    !> unassociated ones use vc-default-FUN or signal `vc_not_supported').
    type :: vc_backend_type
        character(len=:), allocatable :: name
        procedure(vc_file_predicate), pointer, nopass :: registered => null()
        procedure(vc_file_state_function), pointer, nopass :: state => null()
        procedure(vc_file_string_function), pointer, nopass :: working_revision => null()
        procedure(vc_symbol_function), pointer, nopass :: working_revision_symbol => null()
        procedure(vc_file_string_function), pointer, nopass :: mode_line_string => null()
        procedure(vc_file_string_function), pointer, nopass :: root => null()
        procedure(vc_file_string_function), pointer, nopass :: responsible_p => null()
        procedure(vc_file_string_function), pointer, nopass :: find_ignore_file => null()
        procedure(vc_files_model_function), pointer, nopass :: checkout_model => null()
        procedure(vc_buffer_proc), pointer, nopass :: find_file_hook => null()
        procedure(vc_file_predicate), pointer, nopass :: make_version_backups_p => null()
        procedure(vc_register_proc), pointer, nopass :: register => null()
        procedure(vc_file_proc), pointer, nopass :: unregister => null()
        procedure(vc_revert_proc), pointer, nopass :: revert => null()
        procedure(vc_files_proc), pointer, nopass :: revert_files => null()
        procedure(vc_diff_proc), pointer, nopass :: diff => null()
        procedure(vc_find_revision_proc), pointer, nopass :: find_revision => null()
        procedure(vc_checkout_proc), pointer, nopass :: checkout => null()
        procedure(vc_file_proc), pointer, nopass :: delete_file => null()
        procedure(vc_files_proc), pointer, nopass :: delete_files => null()
        procedure(vc_rename_proc), pointer, nopass :: rename_file => null()
        procedure(vc_previous_revision_proc), pointer, nopass :: previous_revision => null()
        procedure(vc_revision_table_proc), pointer, nopass :: revision_completion_table => null()
        procedure(vc_files_proc), pointer, nopass :: mark_resolved => null()
        procedure(vc_status_proc), pointer, nopass :: create_repo => null()
        procedure(vc_checkin_proc), pointer, nopass :: checkin => null()
        !> The backend's Log Edit mode (vc-default-log-edit-mode is
        !> `log_edit_mode').
        procedure(vc_status_proc), pointer, nopass :: log_edit_mode => null()
        procedure(vc_print_log_proc), pointer, nopass :: print_log => null()
        !> The backend's Log View mode (vc-default-log-view-mode is
        !> `log_view_mode').
        procedure(vc_status_proc), pointer, nopass :: log_view_mode => null()
        procedure(vc_show_log_entry_proc), pointer, nopass :: show_log_entry => null()
        procedure(vc_dir_status_files_proc), pointer, nopass :: dir_status_files => null()
        procedure(vc_dir_extra_headers_proc), pointer, nopass :: dir_extra_headers => null()
        procedure(vc_dir_printer_proc), pointer, nopass :: dir_printer => null()
        procedure(vc_status_fileinfo_extra_proc), pointer, nopass :: status_fileinfo_extra => null()
        !> vc-BACKEND-diff-switches, if the backend has it.
        type(vc_switches_type), pointer :: diff_switches => null()
    end type vc_backend_type

    !> The capacity of `vc_backends' (upstream's vc-handled-backends has 8).
    integer, parameter, public :: MAX_VC_BACKENDS = 16

    !> The backends ported, registered by their modules (vc_git_fm).
    !> (A fixed array: gfortran 13 frees an associated procedure pointer
    !> component whose function returns an allocatable string when an
    !> array of this type is deallocated, aborting.)
    type(vc_backend_type), target :: vc_backends(MAX_VC_BACKENDS)

    ! The number of entries of `vc_backends' in use.
    integer :: n_vc_backends = 0

    !> List of `vc_dir' buffers.  Port of vc-hooks.el vc-dir-buffers.
    type(handle_type), allocatable, public :: vc_dir_buffers(:)

    abstract interface
        !> `vc_dir_resynch_file': update the entries for FNAME in any
        !> directory buffers that list it.  Not a port.
        recursive subroutine vc_dir_resynch_file_proc(fname)
            implicit none(type, external)
            character(len=*), intent(in) :: fname
        end subroutine vc_dir_resynch_file_proc
    end interface

    !> vc-dir.el's `vc_dir_resynch_file', associated once VC-Dir is loaded
    !> (vc_dir_fm), for vc-hooks.el, vc-dispatcher.el and vc.el.
    procedure(vc_dir_resynch_file_proc), pointer, public :: vc_dir_resynch_file => null()

    ! ------------------------------------------------------------------
    ! Per-file properties
    ! ------------------------------------------------------------------

    !> The per-file properties (vc-file-prop-obarray), by property.
    integer, parameter, public :: VC_PROP_BACKEND = 1, VC_PROP_STATE = 2, VC_PROP_WORKING_REVISION = 3, &
                                  VC_PROP_CHECKOUT_TIME = 4, VC_PROP_GIT_SYMBOLIC_REF = 5
    integer, parameter :: NPROPS = 5

    !> The backend of an unregistered file (the property `none').
    integer, parameter, public :: VC_BACKEND_NONE = -1

    !> VC's properties of a file (an expanded name): which are set, the
    !> backend (an index of `vc_backends', or VC_BACKEND_NONE), the
    !> state (and the locker of VC_LOCKED), the working revision, the
    !> checkout time (seconds and nanoseconds; nil when unset) and Git's
    !> symbolic ref.
    type :: vc_props_type
        character(len=:), allocatable :: file
        logical :: set(NPROPS) = .false.
        integer :: backend = 0
        integer :: state = VC_STATE_NIL
        character(len=:), allocatable :: locker
        character(len=:), allocatable :: working_revision
        integer(int64) :: checkout_sec = 0
        integer(int32) :: checkout_nsec = 0
        character(len=:), allocatable :: git_symbolic_ref
    end type vc_props_type

    type(vc_props_type), allocatable :: props(:)

    !> The properties set since `vc_touch_reset' (vc-touched-properties),
    !> for vc_fm's with-vc-properties.
    logical :: touched(NPROPS) = .false.

    !> Version control prefix map (C-x v).  Port of vc-hooks.el
    !> vc-prefix-map.
    type(handle_type), public :: vc_prefix_map

    ! Faces.
    !> Base face for VC state indicator.  Port of vc-hooks.el vc-state-base.
    type(named_face_type), target, public :: vc_state_base
    !> Face for VC modeline state when the file is up to date.
    type(named_face_type), target, public :: vc_up_to_date_state
    !> Face for VC modeline state when the file needs update.
    type(named_face_type), target, public :: vc_needs_update_state
    !> Face for VC modeline state when the file locked.
    type(named_face_type), target, public :: vc_locked_state
    !> Face for VC modeline state when the file is locally added.
    type(named_face_type), target, public :: vc_locally_added_state
    !> Face for VC modeline state when the file contains merge conflicts.
    type(named_face_type), target, public :: vc_conflict_state
    !> Face for VC modeline state when the file was removed from the VC system.
    type(named_face_type), target, public :: vc_removed_state
    !> Face for VC modeline state when the file is missing from the file system.
    type(named_face_type), target, public :: vc_missing_state
    !> Face for VC modeline state when the file is edited.
    type(named_face_type), target, public :: vc_edited_state
    !> Face for VC modeline state when the file is registered, but ignored.
    type(named_face_type), target, public :: vc_ignored_state

contains

    ! ------------------------------------------------------------------
    ! Backends
    ! ------------------------------------------------------------------

    !> Make the backend named NAME known to VC (as loading vc-BACKEND.el
    !> does): B is its index in `vc_backends' (0 when it is full), whose operations the
    !> backend module then sets.  A backend of the same name keeps its
    !> entry, which the module sets again.  Not a port.
    subroutine register_vc_backend(name, b)
        character(len=*), intent(in) :: name
        integer, intent(out) :: b
        integer :: k

        do k = 1, n_vc_backends
            if (vc_backends(k)%name == name) then
                b = k
                return
            end if
        end do

        b = 0
        if (n_vc_backends == MAX_VC_BACKENDS) return
        n_vc_backends = n_vc_backends + 1
        vc_backends(n_vc_backends)%name = name
        b = n_vc_backends
    end subroutine register_vc_backend

    !> The index in `vc_backends' of the backend named NAME (0 if none
    !> is ported).  Not a port.
    integer function vc_backend_named(name) result(b)
        character(len=*), intent(in) :: name
        integer :: k

        b = 0

        do k = 1, n_vc_backends
            if (vc_backends(k)%name == trim(name)) then
                b = k
                return
            end if
        end do
    end function vc_backend_named

    !> The name of backend B ("" for none).  Not a port.
    function vc_backend_name(b) result(name)
        integer, intent(in) :: b
        character(len=:), allocatable :: name

        name = ""
        if (b > 0) name = vc_backends(b)%name
    end function vc_backend_name

    !> Signal `vc_not_supported' for FUNCTION of backend B.  Not a port.
    subroutine vc_not_supported(status, function, b)
        type(status_type), intent(inout) :: status
        character(len=*), intent(in) :: function
        integer, intent(in) :: b

        call set_error(status, FM_VC_NOT_SUPPORTED, function//", "//vc_backend_name(b))
    end subroutine vc_not_supported

    !> Backend B's `registered' of FILE (vc-default-registered checks
    !> master templates, which no ported backend has).  Not a port.
    logical function call_registered(b, file) result(r)
        integer, intent(in) :: b
        character(len=*), intent(in) :: file

        r = .false.
        if (associated(vc_backends(b)%registered)) r = vc_backends(b)%registered(file)
    end function call_registered

    !> Backend B's `root' of FILE: the root directory (FOUND false for
    !> nil); an error if it doesn't implement it.  Not a port.
    subroutine vc_call_root(b, file, root, found, status)
        integer, intent(in) :: b
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: root
        logical, intent(out) :: found
        type(status_type), intent(out) :: status

        root = ""
        found = .false.

        if (.not. associated(vc_backends(b)%root)) then
            call vc_not_supported(status, "root", b)
            return
        end if

        call vc_backends(b)%root(file, root, found)
    end subroutine vc_call_root

    !> Backend B's `responsible_p' of FILE: the directory it is
    !> responsible for FILE from (vc-default-responsible-p: nil).
    !> Not a port.
    subroutine vc_call_responsible_p(b, file, dir, found)
        integer, intent(in) :: b
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: dir
        logical, intent(out) :: found

        dir = ""
        found = .false.
        if (associated(vc_backends(b)%responsible_p)) call vc_backends(b)%responsible_p(file, dir, found)
    end subroutine vc_call_responsible_p

    !> Backend B's `make_version_backups_p' of FILE.  Port of
    !> vc-hooks.el:vc-default-make-version-backups-p as the default:
    !> return true if unmodified versions should be backed up locally.
    !> The default is to switch off this feature.
    logical function is_vc_call_make_version_backups(b, file) result(r)
        integer, intent(in) :: b
        character(len=*), intent(in) :: file

        r = .false.
        if (associated(vc_backends(b)%make_version_backups_p)) r = vc_backends(b)%make_version_backups_p(file)
    end function is_vc_call_make_version_backups

    ! ------------------------------------------------------------------
    ! Per-file properties
    ! ------------------------------------------------------------------

    !> The index of FILE's properties in `props', making them with CREATE
    !> (0 if FILE has none).  Not a port.
    integer function prop_index(file, create) result(k)
        character(len=*), intent(in) :: file
        logical, intent(in) :: create
        character(len=:), allocatable :: name
        type(vc_props_type), allocatable :: grown(:)
        type(status_type) :: st
        integer :: n

        call expand_file_name(file, name, st)
        if (.not. st%ok()) name = file
        if (.not. allocated(props)) allocate (props(0))

        do k = 1, size(props)
            if (props(k)%file == name .and. len(props(k)%file) == len(name)) return
        end do

        k = 0
        if (.not. create) return
        n = size(props)
        allocate (grown(n + 1))
        grown(1:n) = props
        grown(n + 1)%file = name
        call move_alloc(grown, props)
        k = n + 1
    end function prop_index

    !> Note that property PROP was set (cl-pushnew onto vc-touched-properties).
    !> Not a port.
    subroutine touch(prop)
        integer, intent(in) :: prop

        touched(prop) = .true.
    end subroutine touch

    !> Start recording which properties are set (let-binding
    !> `vc-touched-properties' to (t)).  Not a port.
    subroutine vc_touch_reset()
        touched = .false.
    end subroutine vc_touch_reset

    !> Whether property PROP was set since `vc_touch_reset'.  Not a port.
    logical function vc_touched(prop)
        integer, intent(in) :: prop

        vc_touched = touched(prop)
    end function vc_touched

    !> Set per-file VC property `vc-backend' for FILE to backend B (0 for
    !> nil, VC_BACKEND_NONE for `none').  Port of vc-hooks.el:vc-file-setprop.
    subroutine vc_file_set_backend(file, b)
        character(len=*), intent(in) :: file
        integer, intent(in) :: b
        integer :: k

        call touch(VC_PROP_BACKEND)
        k = prop_index(file, .true.)
        props(k)%backend = b
        props(k)%set(VC_PROP_BACKEND) = b /= 0
    end subroutine vc_file_set_backend

    !> Get per-file VC property `vc-backend' for FILE (0 for nil).  Port of
    !> vc-hooks.el:vc-file-getprop.
    integer function vc_file_get_backend(file) result(b)
        character(len=*), intent(in) :: file
        integer :: k

        b = 0
        k = prop_index(file, .false.)
        if (k == 0) return
        if (props(k)%set(VC_PROP_BACKEND)) b = props(k)%backend
    end function vc_file_get_backend

    !> Set per-file VC property `vc-state' for FILE to STATE (LOCKER for
    !> VC_LOCKED; VC_STATE_NIL for nil).  Port of
    !> vc-hooks.el:vc-file-setprop.
    subroutine vc_file_set_state(file, state, locker)
        character(len=*), intent(in) :: file
        integer, intent(in) :: state
        character(len=*), intent(in), optional :: locker
        integer :: k

        call touch(VC_PROP_STATE)
        k = prop_index(file, .true.)
        props(k)%state = state
        props(k)%set(VC_PROP_STATE) = state /= VC_STATE_NIL

        if (present(locker)) then
            props(k)%locker = locker
        else if (allocated(props(k)%locker)) then
            deallocate (props(k)%locker)
        end if
    end subroutine vc_file_set_state

    !> Get per-file VC property `vc-state' for FILE (VC_STATE_NIL for
    !> nil), and with LOCKER the user of VC_LOCKED.  Port of
    !> vc-hooks.el:vc-file-getprop.
    integer function vc_file_get_state(file, locker) result(state)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out), optional :: locker
        integer :: k

        state = VC_STATE_NIL
        if (present(locker)) locker = ""
        k = prop_index(file, .false.)
        if (k == 0) return
        if (props(k)%set(VC_PROP_STATE)) state = props(k)%state
        if (present(locker) .and. allocated(props(k)%locker)) locker = props(k)%locker
    end function vc_file_get_state

    !> Set per-file VC property `vc-working-revision' for FILE (unset
    !> with REV absent).  Port of vc-hooks.el:vc-file-setprop.
    subroutine vc_file_set_working_revision(file, rev)
        character(len=*), intent(in) :: file
        character(len=*), intent(in), optional :: rev
        integer :: k

        call touch(VC_PROP_WORKING_REVISION)
        k = prop_index(file, .true.)
        props(k)%set(VC_PROP_WORKING_REVISION) = present(rev)
        if (present(rev)) props(k)%working_revision = rev
    end subroutine vc_file_set_working_revision

    !> Get per-file VC property `vc-working-revision' for FILE (FOUND
    !> false for nil).  Port of vc-hooks.el:vc-file-getprop.
    subroutine vc_file_get_working_revision(file, rev, found)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: rev
        logical, intent(out) :: found
        integer :: k

        rev = ""
        found = .false.
        k = prop_index(file, .false.)
        if (k == 0) return
        found = props(k)%set(VC_PROP_WORKING_REVISION)
        if (found) rev = props(k)%working_revision
    end subroutine vc_file_get_working_revision

    !> Set per-file VC property `vc-checkout-time' for FILE (nil with SEC
    !> absent).  Port of vc-hooks.el:vc-file-setprop.
    subroutine vc_file_set_checkout_time(file, sec, nsec)
        character(len=*), intent(in) :: file
        integer(int64), intent(in), optional :: sec
        integer(int32), intent(in), optional :: nsec
        integer :: k

        call touch(VC_PROP_CHECKOUT_TIME)
        k = prop_index(file, .true.)
        props(k)%set(VC_PROP_CHECKOUT_TIME) = present(sec)
        if (present(sec)) props(k)%checkout_sec = sec
        props(k)%checkout_nsec = 0
        if (present(nsec)) props(k)%checkout_nsec = nsec
    end subroutine vc_file_set_checkout_time

    !> Get per-file VC property `vc-checkout-time' for FILE (FOUND false
    !> for nil).  Port of vc-hooks.el:vc-file-getprop.
    subroutine vc_file_get_checkout_time(file, sec, nsec, found)
        character(len=*), intent(in) :: file
        integer(int64), intent(out) :: sec
        integer(int32), intent(out) :: nsec
        logical, intent(out) :: found
        integer :: k

        sec = 0
        nsec = 0
        found = .false.
        k = prop_index(file, .false.)
        if (k == 0) return
        found = props(k)%set(VC_PROP_CHECKOUT_TIME)
        sec = props(k)%checkout_sec
        nsec = props(k)%checkout_nsec
    end subroutine vc_file_get_checkout_time

    !> Set per-file VC property `vc-git-symbolic-ref' for FILE (nil with
    !> REF absent).  Port of vc-hooks.el:vc-file-setprop.
    subroutine vc_file_set_git_symbolic_ref(file, ref)
        character(len=*), intent(in) :: file
        character(len=*), intent(in), optional :: ref
        integer :: k

        call touch(VC_PROP_GIT_SYMBOLIC_REF)
        k = prop_index(file, .true.)
        props(k)%set(VC_PROP_GIT_SYMBOLIC_REF) = present(ref)
        if (present(ref)) props(k)%git_symbolic_ref = ref
    end subroutine vc_file_set_git_symbolic_ref

    !> Get per-file VC property `vc-git-symbolic-ref' for FILE (FOUND
    !> false for nil).  Port of vc-hooks.el:vc-file-getprop.
    subroutine vc_file_get_git_symbolic_ref(file, ref, found)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: ref
        logical, intent(out) :: found
        integer :: k

        ref = ""
        found = .false.
        k = prop_index(file, .false.)
        if (k == 0) return
        found = props(k)%set(VC_PROP_GIT_SYMBOLIC_REF)
        if (found) ref = props(k)%git_symbolic_ref
    end subroutine vc_file_get_git_symbolic_ref

    !> Clear all VC properties of FILE.  Port of
    !> vc-hooks.el:vc-file-clearprops (`vc_parent_buffer' is vc_fm's).
    subroutine vc_file_clearprops(file)
        character(len=*), intent(in) :: file
        integer :: k
        type(vc_props_type) :: fresh

        k = prop_index(file, .false.)
        if (k == 0) return
        call move_alloc(props(k)%file, fresh%file)
        props(k) = fresh
    end subroutine vc_file_clearprops

    !> Clear all cached file properties.  Port of vc.el:vc-clear-context.
    subroutine vc_clear_context()
        if (allocated(props)) deallocate (props)
        allocate (props(0))
    end subroutine vc_clear_context

    ! ------------------------------------------------------------------
    ! Finding backends, states and revisions
    ! ------------------------------------------------------------------

    !> Find the root of a checked out project.
    !> The function walks up the directory tree from FILE looking for WITNESS.
    !> If WITNESS if not found, FOUND is false, otherwise ROOT is the root.
    !> Port of vc-hooks.el:vc-find-root.
    subroutine vc_find_root(file, witness, root, found)
        character(len=*), intent(in) :: file, witness
        character(len=:), allocatable, intent(out) :: root
        logical, intent(out) :: found

        if (allocated(vc_ignore_dir_regexp)) then
            call locate_dominating_file(file, witness, root, found, vc_ignore_dir_regexp)
        else
            call locate_dominating_file(file, witness, root, found, locate_dominating_stop_dir_regexp)
        end if
    end subroutine vc_find_root

    !> Return true if FILE is registered in a version control system.
    !>
    !> This function performs the check each time it is called.  To rely
    !> on the result of a previous call, use `vc_backend' instead.  If the
    !> file was previously registered under a certain backend, then that
    !> backend is tried first.  Port of vc-hooks.el:vc-registered.
    logical function vc_registered(file) result(r)
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: dir
        type(status_type) :: st
        integer :: b, k, c

        r = .false.
        dir = file_name_directory(file)

        if (len(dir) > 0 .and. allocated(vc_ignore_dir_regexp)) then
            if (string_match(vc_ignore_dir_regexp, dir, st) > 0) return
        end if

        ! Try vc-BACKEND-registered for each handled BACKEND.
        b = vc_file_get_backend(file)

        if (b > 0) then
            if (call_registered(b, file)) then
                call vc_file_set_backend(file, b)
                r = .true.
                return
            end if
        end if

        if (allocated(vc_handled_backends)) then
            do k = 1, size(vc_handled_backends)
                c = vc_backend_named(vc_handled_backends(k))
                if (c == 0) cycle

                if (call_registered(c, file)) then
                    call vc_file_set_backend(file, c)
                    r = .true.
                    return
                end if
            end do
        end if

        ! File is not registered.
        call vc_file_set_backend(file, VC_BACKEND_NONE)
    end function vc_registered

    !> Return the version control type of FILE, 0 if it's not registered.
    !>
    !> This function returns cached information.  To query the VCS regarding
    !> whether FILE is registered or unregistered, use `vc_registered'.
    !> Port of vc-hooks.el:vc-backend (a single file; "" is nil).
    integer function vc_backend(file) result(b)
        character(len=*), intent(in) :: file

        b = 0
        if (len(file) == 0) return
        b = vc_file_get_backend(file)

        ! Note that internally, Emacs remembers unregistered files by
        ! setting the property to `none'.
        if (b == VC_BACKEND_NONE) then
            b = 0
        else if (b == 0) then
            ! vc-registered sets the vc-backend property
            if (vc_registered(file)) then
                b = vc_file_get_backend(file)
            end if
        end if
    end function vc_backend

    !> Indicate how FILES are checked out.
    !>
    !> If FILES are not registered, this function always returns
    !> VC_MODEL_NIL.  For registered files, the possible values are:
    !>
    !>   VC_MODEL_IMPLICIT   FILES are always writable, and checked out `implicitly'
    !>               when the user saves the first changes to the file.
    !>
    !>   VC_MODEL_LOCKING   FILES are read-only if up-to-date; user must type
    !>               C-x v v before editing.  Strict locking
    !>               is assumed.
    !>
    !>   VC_MODEL_ANNOUNCE   FILES are read-only if up-to-date; user must type
    !>               C-x v v before editing.  But other users
    !>               may be editing at the same time.  Port of
    !>               vc-hooks.el:vc-checkout-model.
    integer function vc_checkout_model(b, files) result(model)
        integer, intent(in) :: b
        type(vc_string_type), intent(in) :: files(:)

        model = VC_MODEL_NIL
        if (b <= 0) return
        if (associated(vc_backends(b)%checkout_model)) model = vc_backends(b)%checkout_model(files)
    end function vc_checkout_model

    !> Return the version control state of FILE.
    !>
    !> A return of VC_STATE_NIL from this function means we have no information on the
    !> status of this file.  Otherwise, the value returned is one of:
    !>
    !>   VC_UP_TO_DATE       The working file is unmodified with respect to the
    !>                      latest version on the current branch, and not locked.
    !>
    !>   VC_EDITED           The working file has been edited by the user.  If
    !>                      locking is used for the file, this state means that
    !>                      the current version is locked by the calling user.
    !>                      This status should *not* be reported for files
    !>                      which have a changed mtime but the same content
    !>                      as the repo copy.
    !>
    !>   VC_LOCKED           The current version of the working file is locked by
    !>                      some other USER (a string, given as LOCKER).
    !>
    !>   VC_NEEDS_UPDATE     The file has not been edited by the user, but there is
    !>                      a more recent version on the current branch stored
    !>                      in the repository.
    !>
    !>   VC_NEEDS_MERGE      The file has been edited by the user, and there is also
    !>                      a more recent version on the current branch stored in
    !>                      the repository.  This state can only occur if locking
    !>                      is not used for the file.
    !>
    !>   VC_UNLOCKED_CHANGES The working version of the file is not locked,
    !>                      but the working file has been changed with respect
    !>                      to that version.  This state can only occur for files
    !>                      with locking; it represents an erroneous condition that
    !>                      should be resolved by the user (vc_next_action will
    !>                      prompt the user to do it).
    !>
    !>   VC_ADDED            Scheduled to go into the repository on the next commit.
    !>                      Often represented by vc_working_revision = "0" in VCSes
    !>                      with monotonic IDs like Subversion and Mercurial.
    !>
    !>   VC_REMOVED          Scheduled to be deleted from the repository on next commit.
    !>
    !>   VC_CONFLICT         The file contains conflicts as the result of a merge.
    !>                      For now the conflicts are text conflicts.  In the
    !>                      future this might be extended to deal with metadata
    !>                      conflicts too.
    !>
    !>   VC_MISSING          The file is not present in the file system, but the VC
    !>                      system still tracks it.
    !>
    !>   VC_IGNORED          The file showed up in a dir-status listing with a flag
    !>                      indicating the version-control system is ignoring it,
    !>                      Note: This property is not set reliably (some VCSes
    !>                      don't have useful directory-status commands) so assume
    !>                      that any file with vc_state VC_STATE_NIL might be ignorable
    !>                      without VC knowing it.
    !>
    !>   VC_UNREGISTERED     The file is not under version control.
    !>
    !> Port of vc-hooks.el:vc-state.
    integer function vc_state(file, backend, locker) result(state)
        character(len=*), intent(in) :: file
        integer, intent(in), optional :: backend
        character(len=:), allocatable, intent(out), optional :: locker
        integer :: b

        if (present(locker)) then
            state = vc_file_get_state(file, locker)
        else
            state = vc_file_get_state(file)
        end if

        if (state /= VC_STATE_NIL) return
        if (len(file) == 0) return
        b = 0
        if (present(backend)) b = backend
        if (b == 0) b = vc_backend(file)
        if (b == 0) return
        state = vc_state_refresh(file, b)
        if (present(locker)) locker = ""
    end function vc_state

    !> Quickly recompute the `state' of FILE.  Port of
    !> vc-hooks.el:vc-state-refresh (the state set is returned).
    integer function vc_state_refresh(file, b) result(state)
        character(len=*), intent(in) :: file
        integer, intent(in) :: b

        state = VC_STATE_NIL
        if (associated(vc_backends(b)%state)) state = vc_backends(b)%state(file)
        call vc_file_set_state(file, state)
    end function vc_state_refresh

    !> Convenience function to check whether `vc_state' of FILE is
    !> VC_UP_TO_DATE.  Port of vc-hooks.el:vc-up-to-date-p.
    logical function is_vc_up_to_date(file)
        character(len=*), intent(in) :: file

        is_vc_up_to_date = vc_state(file) == VC_UP_TO_DATE
    end function is_vc_up_to_date

    !> The name of state STATE as upstream's symbol (`up-to-date'), or
    !> the user LOCKER of VC_LOCKED.  Not a port.
    function vc_state_name(state, locker) result(name)
        integer, intent(in) :: state
        character(len=*), intent(in), optional :: locker
        character(len=:), allocatable :: name

        select case (state)
        case (VC_UP_TO_DATE)
            name = "up-to-date"
        case (VC_EDITED)
            name = "edited"
        case (VC_LOCKED)
            name = ""
            if (present(locker)) name = locker
        case (VC_NEEDS_UPDATE)
            name = "needs-update"
        case (VC_NEEDS_MERGE)
            name = "needs-merge"
        case (VC_UNLOCKED_CHANGES)
            name = "unlocked-changes"
        case (VC_ADDED)
            name = "added"
        case (VC_REMOVED)
            name = "removed"
        case (VC_CONFLICT)
            name = "conflict"
        case (VC_MISSING)
            name = "missing"
        case (VC_IGNORED)
            name = "ignored"
        case (VC_UNREGISTERED)
            name = "unregistered"
        case default
            name = "nil"
        end select
    end function vc_state_name

    !> Return the repository version from which FILE was checked out
    !> (FOUND false if FILE is not registered).
    !>
    !> This function does not return nil without first confirming with the
    !> underlying VCS that FILE is unregistered; this is in contrast to
    !> `vc_symbolic_working_revision'.  Port of vc-hooks.el:vc-working-revision.
    subroutine vc_working_revision(file, rev, found, backend)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: rev
        logical, intent(out) :: found
        integer, intent(in), optional :: backend
        character(len=:), allocatable :: abs, saved_dir
        type(status_type) :: st
        integer :: b

        call expand_file_name(file, abs, st)
        if (.not. st%ok()) abs = file
        call vc_file_get_working_revision(abs, rev, found)
        if (found) return
        b = 0
        if (present(backend)) b = backend
        if (b == 0) b = vc_backend(file)
        if (b == 0) return
        if (.not. associated(vc_backends(b)%working_revision)) return
        ! (let ((default-directory (file-name-directory abs))) ...)
        saved_dir = buffer_directory()
        call set_buffer_directory(file_name_directory(abs))
        call vc_backends(b)%working_revision(file, rev, found)
        call set_buffer_directory(saved_dir)

        if (found) then
            call vc_file_set_working_revision(abs, rev)
        else
            call vc_file_set_working_revision(abs)
        end if
    end subroutine vc_working_revision

    !> Return BACKEND's symbolic name for FILE's working revision.
    !> If FILE names an existing directory, the BACKEND argument is mandatory.
    !> If FILE is not registered according to cached information or names an
    !> existing directory and BACKEND is absent, FOUND is false.
    !> If BACKEND does not have a symbolic name for the working revision or
    !> Fortmacs doesn't know what it is, call `vc_working_revision' instead.
    !>
    !> Prefer this function to `vc_working_revision' whenever a symbolic name
    !> will do, for it avoids a call out to the underlying VCS.  Port of
    !> vc-hooks.el:vc-symbolic-working-revision.
    subroutine vc_symbolic_working_revision(file, rev, found, backend)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: rev
        logical, intent(out) :: found
        integer, intent(in), optional :: backend
        integer :: b, cached

        rev = ""
        found = .false.
        b = 0
        if (present(backend)) b = backend
        cached = 0

        if (.not. (is_file_directory(file) .and. b /= 0)) then
            cached = vc_backend(file)
            if (cached == 0) return
        end if

        if (b == 0) b = cached

        if (associated(vc_backends(b)%working_revision_symbol)) then
            call vc_backends(b)%working_revision_symbol(rev)
            found = .true.
        else
            call vc_working_revision(file, rev, found, b)
        end if
    end subroutine vc_symbolic_working_revision

    !> Return the repository version for FILE in a shortened form (FOUND
    !> false if FILE is not registered).  Port of
    !> vc-hooks.el:vc-short-revision.
    subroutine vc_short_revision(file, rev, found, backend)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: rev
        logical, intent(out) :: found
        integer, intent(in), optional :: backend
        logical :: saved
        integer :: b

        rev = ""
        found = .false.
        b = 0
        if (present(backend)) b = backend
        if (b == 0) b = vc_backend(file)
        if (b == 0) return
        if (.not. associated(vc_backends(b)%working_revision)) return
        saved = vc_use_short_revision
        vc_use_short_revision = .true.
        call vc_backends(b)%working_revision(file, rev, found)
        vc_use_short_revision = saved
    end subroutine vc_short_revision

    !> The current buffer's `default_directory'.  Not a port.
    function buffer_directory() result(dir)
        character(len=:), allocatable :: dir

        dir = "./"
        if (cur == 0) return
        if (allocated(buffers(cur)%directory)) dir = buffers(cur)%directory
    end function buffer_directory

    !> Set the current buffer's `default_directory' to DIR (a let-binding
    !> undone by setting it back).  Not a port.
    subroutine set_buffer_directory(dir)
        character(len=*), intent(in) :: dir

        if (cur == 0) return
        if (len(dir) > 0) buffers(cur)%directory = dir
    end subroutine set_buffer_directory

    ! ------------------------------------------------------------------
    ! Version backups
    ! ------------------------------------------------------------------

    !> Return a backup file name for REV or the current version of FILE.
    !> If MANUAL is true it means that a name for backups created by
    !> the user should be returned; if REGEXP is true that means to return
    !> a regexp for matching all such backup files, regardless of the version.
    !> Port of vc-hooks.el:vc-version-backup-file-name.
    function vc_version_backup_file_name(file, rev, manual, regexp) result(name)
        use search_fm, only: regexp_quote
        character(len=*), intent(in) :: file
        character(len=*), intent(in), optional :: rev
        logical, intent(in), optional :: manual, regexp
        character(len=:), allocatable :: name
        character(len=:), allocatable :: r, dot
        type(status_type) :: st
        logical :: man, found
        integer :: k

        man = .false.
        if (present(manual)) man = manual
        dot = "."
        if (man) dot = ""

        if (present(regexp)) then
            if (regexp) then
                name = regexp_quote(file_name_nondirectory(file))//"\.~.+"
                if (.not. man) name = name//"\."
                name = name//"~"
                return
            end if
        end if

        if (present(rev)) then
            r = rev
        else
            call vc_working_revision(file, r, found)
        end if

        do k = 1, len(r)
            if (r(k:k) == "/") r(k:k) = "_"
        end do

        call expand_file_name(file_name_nondirectory(file)//".~"//r//dot//"~", name, st, file_name_directory_or_default(file))
        if (.not. st%ok()) name = file_name_nondirectory(file)//".~"//r//dot//"~"
    end function vc_version_backup_file_name

    !> The directory of FILE, or `default_directory' (expand-file-name's
    !> default for a nil DIRECTORY).  Not a port.
    function file_name_directory_or_default(file) result(dir)
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: dir

        dir = file_name_directory(file)
        if (len(dir) == 0) dir = buffer_directory()
    end function file_name_directory_or_default

    !> Delete all existing automatic version backups for FILE.  Port of
    !> vc-hooks.el:vc-delete-automatic-version-backups.
    subroutine vc_delete_automatic_version_backups(file)
        use dired_fm, only: directory_files
        use lisp_fm, only: value_type, list_elements, xstring
        character(len=*), intent(in) :: file
        type(value_type) :: names
        type(value_type), allocatable :: elts(:)
        type(status_type) :: st, mst
        character(len=:), allocatable :: re, dir
        integer :: k

        dir = file_name_directory_or_default(file)
        re = vc_version_backup_file_name(file, regexp=.true.)
        ! Don't fail when the directory doesn't exist.
        call directory_files(dir, names, st, full=.true.)
        if (.not. st%ok()) return
        call list_elements(names, elts)

        do k = 1, size(elts)
            if (string_match(re, file_name_nondirectory(xstring(elts(k))), mst) > 0) then
                call delete_file(xstring(elts(k)), st)
            end if
        end do
    end subroutine vc_delete_automatic_version_backups

    !> Make a backup copy of FILE, which is assumed in sync with the repository.
    !> Before doing that, check if there are any old backups and get rid of them.
    !> Port of vc-hooks.el:vc-make-version-backup.
    subroutine vc_make_version_backup(file)
        character(len=*), intent(in) :: file
        type(status_type) :: st

        call vc_delete_automatic_version_backups(file)
        call copy_file(file, vc_version_backup_file_name(file), st, keep_time=.true.)
        ! It's ok if it doesn't work (e.g. directory not writable), since
        ! this is just for efficiency.
        if (.not. st%ok()) call message("Warning: Cannot make version backup; diff/revert therefore not local")
    end subroutine vc_make_version_backup

    !> Function to be called by `basic_save_buffer' (in files_fm).  Port of
    !> vc-hooks.el:vc-before-save.
    subroutine vc_before_save()
        character(len=:), allocatable :: file
        type(vc_string_type) :: files(1)
        integer :: b

        ! If the file on disk is still in sync with the repository,
        ! and version backups should be made, copy the file to
        ! another name.  This enables local diffs and local reverting.
        if (cur == 0) return
        if (.not. allocated(buffers(cur)%filename)) return
        file = buffers(cur)%filename
        if (.not. file_exists(file)) call vc_file_clearprops(file)
        b = vc_backend(file)
        if (b == 0) return
        if (.not. is_vc_up_to_date(file)) return
        files(1)%s = file
        if (vc_checkout_model(b, files) /= VC_MODEL_IMPLICIT) return
        if (.not. is_vc_call_make_version_backups(b, file)) return
        call vc_make_version_backup(file)
    end subroutine vc_before_save

    !> Function to be called by `basic_save_buffer' (in files_fm).  Port of
    !> vc-hooks.el:vc-after-save (no *vc-dir* buffers).
    subroutine vc_after_save()
        character(len=:), allocatable :: file
        type(vc_string_type) :: files(1)
        type(file_stat_type) :: attr
        integer(int64) :: sec
        integer(int32) :: nsec
        logical :: found, have_attr
        integer :: b, state

        ! If the file in the current buffer is under version control,
        ! up-to-date, and locking is not used for the file, set
        ! the state to 'edited and redisplay the mode line.
        if (cur == 0) return
        if (.not. allocated(buffers(cur)%filename)) return
        file = buffers(cur)%filename
        b = vc_backend(file)
        files(1)%s = file

        if (b == 0) then
            continue
        else if (vc_checkout_model(b, files) == VC_MODEL_IMPLICIT) then
            ! If the file was saved at the same time that it was
            ! checked out, clear the checkout-time to avoid confusion.
            call vc_file_get_checkout_time(file, sec, nsec, found)
            call file_attributes(file, attr, have_attr)

            if (found .and. have_attr) then
                if (sec == attr%mtime_sec .and. nsec == attr%mtime_nsec) call vc_file_set_checkout_time(file)
            end if

            state = vc_state_refresh(file, b)
            if (state /= VC_STATE_NIL) call vc_mode_line(file, b)
        else if (vc_file_get_state(file) == VC_UP_TO_DATE) then
            ! If we saved an unlocked file on a locking based VCS, that
            ! file is not longer up-to-date.
            call vc_file_set_state(file, VC_STATE_NIL)
        end if

        ! Resynch *vc-dir* buffers, if any are present.
        if (has_vc_dir_buffers() .and. associated(vc_dir_resynch_file)) call vc_dir_resynch_file(file)
    end subroutine vc_after_save

    !> Update VC-Dir contents after reverting a buffer from disk.  Port of
    !> vc-hooks.el:vc-after-revert.
    recursive subroutine vc_after_revert(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (.not. has_vc_dir_buffers()) return
        if (cur == 0) return
        if (.not. allocated(buffers(cur)%filename)) return
        if (vc_backend(buffers(cur)%filename) == 0) return
        if (associated(vc_dir_resynch_file)) call vc_dir_resynch_file(buffers(cur)%filename)
    end subroutine vc_after_revert

    !> Whether `vc_dir_buffers' is not empty.  Not a port.
    logical function has_vc_dir_buffers()
        has_vc_dir_buffers = .false.
        if (allocated(vc_dir_buffers)) has_vc_dir_buffers = size(vc_dir_buffers) > 0
    end function has_vc_dir_buffers

    ! ------------------------------------------------------------------
    ! The mode line
    ! ------------------------------------------------------------------

    !> Set `vc_mode' to display type of version control for FILE.
    !> The value is set in the current buffer, which should be the buffer
    !> visiting FILE.
    !> If BACKEND is passed use it as the VC backend when computing the
    !> result; USED is the backend (0 for none).  Port of
    !> vc-hooks.el:vc-mode-line.
    subroutine vc_mode_line(file, backend, used)
        character(len=*), intent(in) :: file
        integer, intent(in), optional :: backend
        integer, intent(out), optional :: used
        character(len=:), allocatable :: ml_string
        integer :: b
        logical :: found
        integer(int32) :: modes

        b = 0
        if (present(backend)) b = backend
        if (b == 0) b = vc_backend(file)

        if (b == 0) then
            if (allocated(buffers(cur)%vc_mode)) deallocate (buffers(cur)%vc_mode)
        else if (vc_display_status == VC_DISPLAY_STATUS_NIL) then
            buffers(cur)%vc_mode = " "//vc_backend_name(b)
        else
            if (associated(vc_backends(b)%mode_line_string)) then
                call vc_backends(b)%mode_line_string(file, ml_string, found)
            else
                ml_string = vc_default_mode_line_string(b, file)
            end if

            buffers(cur)%vc_mode = " "//ml_string

            ! If the user is root, and the file is not owner-writable,
            ! then pretend that we can't write it
            ! even though we can (because root can write anything).
            ! This way, even root cannot modify a file that isn't locked.
            if (allocated(buffers(cur)%filename)) then
                if (file == buffers(cur)%filename .and. .not. buffers(cur)%read_only .and. user_real_uid() == 0) then
                    modes = file_modes(buffers(cur)%filename)
                    if (modes >= 0 .and. iand(modes, 128_int32) == 0) buffers(cur)%read_only = .true.
                end if
            end if
        end if

        if (present(used)) used = b
    end subroutine vc_mode_line

    !> The data to display on the mode line for STATE (LOCKER for
    !> VC_LOCKED), the version control state returned from `vc_state':
    !> the echo string, the face name, and the indicator that usually is
    !> one character.  Port of vc-hooks.el:vc-mode-line-state.
    subroutine vc_mode_line_state(state, state_echo, face, indicator, locker)
        integer, intent(in) :: state
        character(len=:), allocatable, intent(out) :: state_echo, face, indicator
        character(len=*), intent(in), optional :: locker

        select case (state)
        case (VC_UP_TO_DATE, VC_NEEDS_UPDATE)
            state_echo = "Up to date file"
            face = "vc_up_to_date_state"
            indicator = "-"
        case (VC_LOCKED)
            if (present(locker)) then
                state_echo = "File locked by"//locker
                indicator = ":"//locker//":"
            else
                state_echo = "File locked by"
                indicator = "::"
            end if

            face = "vc_locked_state"
        case (VC_ADDED)
            state_echo = "Locally added file"
            face = "vc_locally_added_state"
            indicator = "@"
        case (VC_CONFLICT)
            state_echo = "File contains conflicts after the last merge"
            face = "vc_conflict_state"
            indicator = "!"
        case (VC_REMOVED)
            state_echo = "File removed from the VC system"
            face = "vc_removed_state"
            indicator = "!"
        case (VC_MISSING)
            state_echo = "File tracked by the VC system, but missing from the file system"
            face = "vc_missing_state"
            indicator = "?"
        case (VC_IGNORED)
            state_echo = "File tracked by the VC system, but ignored"
            face = "vc_ignored_state"
            indicator = "!"
        case default
            ! Not just for the 'edited state, but also a fallback
            ! for all other states.  Think about different symbols
            ! for 'needs-update and 'needs-merge.
            state_echo = "Locally modified file"
            face = "vc_edited_state"
            indicator = ":"
        end select
    end subroutine vc_mode_line_state

    !> Return a string for `vc_mode_line' to put in the mode line for FILE.
    !> Format:
    !>
    !>   "BACKEND-REV"        if the file is up-to-date
    !>   "BACKEND:REV"        if the file is edited (or locked by the calling user)
    !>   "BACKEND:LOCKER:REV" if the file is locked by somebody else
    !>   "BACKEND@REV"        if the file was locally added
    !>   "BACKEND!REV"        if the file contains conflicts or was removed
    !>   "BACKEND?REV"        if the file is under VC, but is missing
    !>
    !> This function assumes that the file is registered.  Port of
    !> vc-hooks.el:vc-default-mode-line-string.
    function vc_default_mode_line_string(b, file) result(s)
        integer, intent(in) :: b
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: s
        character(len=:), allocatable :: rev, echo, face, indicator, locker
        logical :: found
        integer :: state

        state = vc_state(file, b, locker)
        call vc_symbolic_working_revision(file, rev, found, b)
        call vc_mode_line_state(state, echo, face, indicator, locker)
        s = ""
        if (vc_display_status /= VC_DISPLAY_STATUS_NO_BACKEND) s = vc_backend_name(b)
        s = s//indicator//rev
    end function vc_default_mode_line_string

    !> If current buffer visits a symbolic link, visit the real file.
    !> If the real file is already visited in another buffer, make that buffer
    !> current, and kill the buffer that visits the link.  Port of
    !> vc-hooks.el:vc-follow-link.
    subroutine vc_follow_link()
        character(len=:), allocatable :: truename
        type(handle_type) :: true_buffer, this_buffer, buf
        type(status_type) :: st

        call file_truename(buffers(cur)%filename, truename, st)
        if (.not. st%ok()) return
        true_buffer = get_file_buffer(truename)
        this_buffer = current_buffer()

        if (true_buffer%index == 0 .or. true_buffer%index == this_buffer%index) then
            call kill_buffer(this_buffer, st)
            ! In principle, we could do something like set-visited-file-name.
            ! However, it can't be exactly the same as set-visited-file-name.
            ! I'm not going to work out the details right now. -- rms.
            call find_file_noselect(truename, buf, st)
            if (st%ok()) call set_buffer(buf, st)
        else
            call set_buffer(true_buffer, st)
            call kill_buffer(this_buffer, st)
        end if
    end subroutine vc_follow_link

    !> Refresh the VC state of the current buffer's file.
    !>
    !> This command is more thorough than `vc_state_refresh', in that it
    !> also supports switching a back-end or removing the file from VC.
    !> In the latter case, VC mode is deactivated for this buffer.  Port of
    !> vc-hooks.el:vc-refresh-state (errors are shown as "VC refresh
    !> error: ..." messages, as with-demoted-errors does).
    recursive subroutine vc_refresh_state(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: truename, saved_dir
        type(status_type) :: st
        integer :: b, link_type
        logical :: yes

        status = status_type()
        ! Recompute whether file is version controlled,
        ! if user has killed the buffer and revisited.
        if (cur == 0) return
        if (allocated(buffers(cur)%vc_mode)) deallocate (buffers(cur)%vc_mode)
        if (.not. allocated(buffers(cur)%filename)) return
        call vc_file_clearprops(buffers(cur)%filename)
        b = vc_backend(buffers(cur)%filename)

        if (b /= 0) then
            saved_dir = buffer_directory()
            ! Let the backend setup any buffer-local things it needs.
            if (associated(vc_backends(b)%find_file_hook)) call vc_backends(b)%find_file_hook()

            ! Compute the state and put it in the mode line.
            if (cur /= 0) then
                if (allocated(buffers(cur)%filename)) call vc_mode_line(buffers(cur)%filename, b)
            end if

            call set_buffer_directory(saved_dir)
            ! Use this variable, not make-backup-files, because this is
            ! for things that depend on the file name.
            if (.not. vc_make_backup_files) buffers(cur)%backup_inhibited = .true.
            return
        end if

        call file_truename(buffers(cur)%filename, truename, st)
        if (.not. st%ok()) return
        link_type = 0
        if (truename /= buffers(cur)%filename) link_type = vc_backend(truename)
        if (link_type == 0) return

        if (vc_follow_symlinks == VC_FOLLOW_SYMLINKS_NIL) then
            call message("Warning: symbolic link to "//vc_backend_name(link_type)//"-controlled source file")
        else if (vc_follow_symlinks /= VC_FOLLOW_SYMLINKS_ASK .or. already_followed()) then
            call vc_follow_link()
            call message("Followed link to "//buffers(cur)%filename)
            call vc_refresh_state(status)
        else
            call read_yes_or_no("Symbolic link to "//vc_backend_name(link_type)// &
                                "-controlled source file; follow link? ", yes, status)
            if (.not. status%ok()) return

            if (yes) then
                call vc_follow_link()
                call message("Followed link to "//buffers(cur)%filename)
                call vc_refresh_state(status)
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
    end subroutine vc_refresh_state

    !> `vc_refresh_state' as the find-file-hook function.  Not a port.
    recursive subroutine refresh_state_hook(status)
        type(status_type), intent(out) :: status

        call vc_refresh_state(status)

        if (.not. status%ok()) then
            call message("VC refresh error: "//format_error(status))
            status = status_type()
        end if
    end subroutine refresh_state_hook

    !> Discard VC info about a file when we kill its buffer.  Port of
    !> vc-hooks.el:vc-kill-buffer-hook.
    recursive subroutine vc_kill_buffer_hook(status)
        type(status_type), intent(out) :: status

        status = status_type()
        if (cur == 0) return
        if (allocated(buffers(cur)%filename)) call vc_file_clearprops(buffers(cur)%filename)
    end subroutine vc_kill_buffer_hook

    recursive subroutine cmd_vc_refresh_state(status)
        type(status_type), intent(out) :: status

        call vc_refresh_state(status)
    end subroutine cmd_vc_refresh_state

    ! ------------------------------------------------------------------
    ! Setup
    ! ------------------------------------------------------------------

    ! Define the faces of VC's state indicator.  Not a port.
    subroutine define_faces()
        type(named_face_type), pointer :: f

        f => vc_state_base
        call f%define("vc_state_base", "Base face for VC state indicator.")
        call f%spec_entry(display_when(is_default=.true.), face_attrs())
        f => vc_up_to_date_state
        call f%define("vc_up_to_date_state", "Face for VC modeline state when the file is up to date.")
        call f%spec_entry(display_when(is_default=.true.), face_attrs(inherit="vc_state_base"))
        f => vc_needs_update_state
        call f%define("vc_needs_update_state", "Face for VC modeline state when the file needs update.")
        call f%spec_entry(display_when(is_default=.true.), face_attrs(inherit="vc_state_base"))
        f => vc_locked_state
        call f%define("vc_locked_state", "Face for VC modeline state when the file locked.")
        call f%spec_entry(display_when(is_default=.true.), face_attrs(inherit="vc_state_base"))
        f => vc_locally_added_state
        call f%define("vc_locally_added_state", "Face for VC modeline state when the file is locally added.")
        call f%spec_entry(display_when(is_default=.true.), face_attrs(inherit="vc_state_base"))
        f => vc_conflict_state
        call f%define("vc_conflict_state", "Face for VC modeline state when the file contains merge conflicts.")
        call f%spec_entry(display_when(is_default=.true.), face_attrs(inherit="vc_state_base"))
        f => vc_removed_state
        call f%define("vc_removed_state", "Face for VC modeline state when the file was removed from the VC system.")
        call f%spec_entry(display_when(is_default=.true.), face_attrs(inherit="vc_state_base"))
        f => vc_missing_state
        call f%define("vc_missing_state", &
                      "Face for VC modeline state when the file is missing from the file system.")
        call f%spec_entry(display_when(is_default=.true.), face_attrs(inherit="vc_state_base"))
        f => vc_edited_state
        call f%define("vc_edited_state", "Face for VC modeline state when the file is edited.")
        call f%spec_entry(display_when(is_default=.true.), face_attrs(inherit="vc_state_base"))
        f => vc_ignored_state
        call f%define("vc_ignored_state", "Face for VC modeline state when the file is registered, but ignored.")
        call f%spec_entry(display_when(is_default=.true.), face_attrs(inherit="vc_state_base"))
    end subroutine define_faces

    !> Set up VC's resident part: the options, faces, hooks and
    !> `vc_prefix_map' on C-x v in CTL_X_MAP.  Not a port.
    subroutine syms_of_vc_hooks(ctl_x_map)
        use lisp_fm, only: value_type, make_fixnum
        type(handle_type), intent(in) :: ctl_x_map
        type(value_type) :: key(1)
        type(status_type) :: st

        if (.not. allocated(vc_ignore_dir_regexp)) vc_ignore_dir_regexp = locate_dominating_stop_dir_regexp

        if (.not. allocated(vc_handled_backends)) then
            vc_handled_backends = [character(len=8) :: "RCS", "CVS", "SVN", "SCCS", "SRC", "Bzr", "Git", "Hg"]
        end if

        if (.not. allocated(vc_directory_exclusion_list)) then
            vc_directory_exclusion_list = [character(len=8) :: "SCCS", "RCS", "CVS", "MCVS", ".src", ".svn", ".git", &
                                           ".hg", ".bzr", "_MTN", "_darcs", "{arch}", ".repo", ".jj"]
        end if

        if (.not. allocated(props)) allocate (props(0))
        call define_faces()
        commands%vc_refresh_state%run => cmd_vc_refresh_state
        call find_file_hook%add(refresh_state_hook)
        call kill_buffer_hook%add(vc_kill_buffer_hook)
        vc_before_save_proc => vc_before_save
        vc_after_save_proc => vc_after_save
        if (.not. after_revert_hook%member(vc_after_revert)) call after_revert_hook%add(vc_after_revert)
        call make_sparse_keymap(vc_prefix_map)
        call keymap_set(vc_prefix_map, "G", "vc_ignore", st)
        call keymap_set(vc_prefix_map, "i", "vc_register", st)
        call keymap_set(vc_prefix_map, "u", "vc_revert", st)
        call keymap_set(vc_prefix_map, "@", "vc_revert", st)
        call keymap_set(vc_prefix_map, "v", "vc_next_action", st)
        call keymap_set(vc_prefix_map, "=", "vc_diff", st)
        call keymap_set(vc_prefix_map, "D", "vc_root_diff", st)
        call keymap_set(vc_prefix_map, "d", "vc_dir", st)
        call keymap_set(vc_prefix_map, "l", "vc_print_log", st)
        call keymap_set(vc_prefix_map, "L", "vc_print_root_log", st)
        call keymap_set(vc_prefix_map, "~", "vc_revision_other_window", st)
        call keymap_set(vc_prefix_map, "R", "vc_rename_file", st)
        call keymap_set(vc_prefix_map, "x", "vc_delete_file", st)
        key(1) = make_fixnum(int(iachar("v"), int64))
        call define_key(ctl_x_map, key, make_keymap_ref(vc_prefix_map), st)
    end subroutine syms_of_vc_hooks
end module vc_hooks_fm
