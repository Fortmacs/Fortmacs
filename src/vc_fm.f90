! vc_fm.f90 -- drive a version-control system from within Fortmacs.
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

!> VC's commands: deducing the fileset and backend of the current buffer
!> (a file under version control, a Dired buffer, a Diff buffer or a
!> directory), `vc_next_action' (C-x v v), registering (C-x v i),
!> ignoring (C-x v G), diffs (C-x v =, C-u C-x v =, C-x v D), reverting
!> (C-x v u, C-x v @), visiting another revision (C-x v ~), deleting
!> (C-x v x) and renaming (C-x v R) files, and the switches options.
!> dired-aux.el's `dired_vc_deduce_fileset' and diff-mode.el's
!> `diff_vc_deduce_fileset' are here, with the fileset type.
!>
!> Deferred: committing (`vc_checkin', and so C-x v v on changed files
!> and on a patch: Log Edit mode), the log commands (Log View mode), VC-Dir,
!> annotating, tags and branches, merging, pulling and pushing,
!> incoming/outgoing and unintegrated changes, working trees, cherry-picks
!> and the history-rewriting commands, `vc_find_revision_no_save' (the
!> revision is saved as FILE.~REV~), `vc_coding_system_for_diff' (output
!> is decoded as UTF-8), restoring point in a reverted *vc-diff* buffer
!> (vc-diff-restore-buffer), `vc_diff_finish_functions' other than
!> shrinking the window, the locking backends' operations (stealing locks,
!> checking out, unlocked changes), transferring files between backends
!> (C-x v v with a backend name), `vc_user_edit_command', and C-x v v from
!> Log View and VC-Dir buffers.
module vc_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use commands_fm, only: commands
    use error_fm, only: status_type, set_error, format_error, condition_matches, FM_GENERIC_ERROR, FM_USER_ERROR, &
                        FM_VC_NOT_SUPPORTED, FM_FILE_ALREADY_EXISTS
    use handle_fm, only: handle_type, null_handle
    use lisp_fm, only: value_type, nil, make_string, make_list, nilp
    use lread_fm, only: sym
    use data_fm, only: symbol_value, set, add_local_variables_killer
    use eval_fm, only: hook_type
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, get_buffer_create, get_buffer, buffer_live_p, &
                         get_file_buffer, buffer_list, kill_buffer, generate_new_buffer_name, restore_buffer_modified_p
    use editfns_fm, only: point, goto_char, point_min, point_max, bolp, eobp, buffer_string, buffer_narrowed_p
    use insdel_fm, only: insert
    use search_fm, only: re_search_forward, replace_match, regexp_quote, SEARCH_NOERROR
    use fileio_fm, only: expand_file_name, file_name_directory, file_name_nondirectory, file_name_as_directory, &
                         directory_file_name, file_exists_p, file_directory_p, file_readable_p, file_name_absolute_p, &
                         file_symlink_p, file_attributes, file_stat_type, rename_file, copy_file, write_region, &
                         directory_name_p
    use files_fm, only: file_relative_name, find_file_noselect, save_some_buffers, set_visited_file_name, &
                        revert_buffer_function, revert_buffer_in_progress, make_backup_files, backup_file_name_p, &
                        delete_file, delete_directory, save_buffer, file_in_directory_p, backup_buffer
    use callint_fm, only: current_prefix_arg
    use minibuf_fm, only: read_string, completion_list_table
    use minibuffer_fm, only: read_file_name, completing_read, REQUIRE_MATCH_T, REQUIRE_MATCH_NIL
    use subr_fm, only: y_or_n_p
    use fns_fm, only: yes_or_no_p
    use xdisp_fm, only: message
    use window_fm, only: pop_to_buffer, switch_to_buffer_other_window, quit_windows_on, &
                         shrink_window_if_larger_than_buffer, get_buffer_window
    use diff_fm, only: diff_switches
    use diff_mode_fm, only: diff_mode, diff_setup_buffer_type, set_diff_vc_backend, diff_vc_backend, diff_file_next, &
                            diff_find_file_name
    use dired_fm, only: directory_files
    use dired_mode_fm, only: dired_get_marked_files, dired_string_type
    use vc_hooks_fm, only: vc_string_type, vc_switches_type, vc_backends, vc_backend_named, vc_backend_name, &
                           vc_handled_backends, vc_directory_exclusion_list, vc_make_backup_files, vc_backend, &
                           vc_registered, vc_state, vc_checkout_model, vc_up_to_date_p, vc_working_revision, &
                           vc_symbolic_working_revision, vc_file_set_backend, vc_file_set_state, vc_file_get_state, &
                           vc_file_set_checkout_time, vc_file_get_backend, vc_file_clearprops, vc_mode_line, &
                           vc_version_backup_file_name, vc_delete_automatic_version_backups, vc_make_version_backup, &
                           vc_call_root, vc_call_responsible_p, vc_call_make_version_backups_p, vc_not_supported, &
                           vc_clear_context, vc_touch_reset, vc_touched, vc_state_name, &
                           VC_SWITCHES_NIL, VC_SWITCHES_T, VC_SWITCHES_LIST, VC_STATE_NIL, VC_UP_TO_DATE, VC_EDITED, &
                           VC_LOCKED, VC_NEEDS_UPDATE, VC_NEEDS_MERGE, VC_UNLOCKED_CHANGES, VC_ADDED, VC_REMOVED, &
                           VC_CONFLICT, VC_MISSING, VC_IGNORED, VC_UNREGISTERED, VC_MODEL_NIL, VC_MODEL_IMPLICIT, &
                           VC_BACKEND_NONE, VC_PROP_BACKEND, VC_PROP_STATE, VC_PROP_CHECKOUT_TIME
    use vc_dispatcher_fm, only: vc_action_type, vc_setup_buffer, vc_exec_after, vc_delistify, vc_resynch_buffer, &
                                vc_buffer_sync, vc_parent_buffer, set_vc_parent_buffer, vc_mode_line_hook, &
                                vc_do_command, syms_of_vc_dispatcher, VC_OKSTATUS_ASYNC, VC_DEST_CURRENT, VC_NOQUERY_T
    use process_fm, only: get_buffer_process
    implicit none(type, external)
    private

    public :: syms_of_vc, vc_switches, vc_root_dir, vc_responsible_backend, vc_backend_for_registration, &
              vc_deduce_backend, vc_deduce_fileset, vc_ensure_vc_buffer, vc_next_action, vc_register, vc_ignore, &
              vc_checkout, vc_mark_resolved, vc_diff_internal, vc_read_revision, vc_version_diff, &
              vc_root_version_diff, vc_diff, vc_root_diff, vc_buffer_sync_fileset, vc_revision_other_window, &
              vc_find_revision, vc_revert, vc_revert_file, vc_revert_files, vc_delete_file, vc_rename_file, &
              vc_file_tree_walk, vc_create_repo, vc_fileset_type, vc_buffer_revision, vc_version_backup_file

    !> What `vc_deduce_fileset' returns: (BACKEND FILESET
    !> FILESET-ONLY-FILES STATE CHECKOUT-MODEL), the last three only with
    !> STATE-MODEL-ONLY-FILES (HAS_STATE); a PATCH (the model `patch' of a
    !> Diff buffer).
    type :: vc_fileset_type
        integer :: backend = 0
        type(vc_string_type), allocatable :: files(:), only_files(:)
        integer :: state = VC_STATE_NIL
        integer :: model = VC_MODEL_NIL
        logical :: has_state = .false.
        logical :: patch = .false.
    end type vc_fileset_type

    ! ------------------------------------------------------------------
    ! Customization variables
    ! ------------------------------------------------------------------

    !> A string or list of strings specifying extra switches for checkin.
    !> These are passed to the checkin program by `vc_checkin'.  Port of
    !> vc.el vc-checkin-switches.
    type(vc_switches_type), public :: vc_checkin_switches

    !> A string or list of strings specifying extra switches for checkout.
    !> These are passed to the checkout program by `vc_checkout'.  Port
    !> of vc.el vc-checkout-switches.
    type(vc_switches_type), public :: vc_checkout_switches

    !> A string or list of strings; extra switches for registering a file.
    !> These are passed to the checkin program by `vc_register'.  Port of
    !> vc.el vc-register-switches.
    type(vc_switches_type), public :: vc_register_switches

    !> A string or list of strings specifying switches for diff under VC.
    !> When running diff under a given BACKEND, VC uses the first
    !> non-nil value of `vc_BACKEND_diff_switches', `vc_diff_switches',
    !> and `diff_switches', in that order.  Since nil means to check the
    !> next variable in the sequence, either of the first two may use
    !> the value t to mean no switches at all.  `vc_diff_switches'
    !> should contain switches that are specific to version control, but
    !> not specific to any particular backend.  Port of vc.el
    !> vc-diff-switches.
    type(vc_switches_type), public :: vc_diff_switches

    !> A string or list of strings specifying switches for annotate under VC.
    !> When running annotate under a given BACKEND, VC uses the first
    !> non-nil value of `vc_BACKEND_annotate_switches' and
    !> `vc_annotate_switches', in that order.  Since nil means to check
    !> the next variable in the sequence, setting the first to the value
    !> t means no switches at all.  `vc_annotate_switches' should
    !> contain switches that are specific to version control, but not
    !> specific to any particular backend.
    !>
    !> As very few switches (if any) are used across different VC tools,
    !> please consider using the specific `vc_BACKEND_annotate_switches'
    !> for the backend you use.  Port of vc.el vc-annotate-switches.
    type(vc_switches_type), public :: vc_annotate_switches

    !> Limit the number of items shown by the VC log commands.
    !> Zero means unlimited.
    !> Not all VC backends are able to support this feature.  Port of
    !> vc.el vc-log-show-limit.
    integer, public :: vc_log_show_limit = 500

    !> Specifies whether the diff during C-x v u may be asynchronous.
    !> Enabling this option means that you can confirm a revert operation even
    !> if the local changes in the file have not been found and displayed yet.
    !> Port of vc.el vc-allow-async-revert.
    logical, public :: vc_allow_async_revert = .false.

    !> True to allow asynchronous diff process.
    !> Enabling this means the buffer will be displayed before the diff is
    !> generated, and so might only say "No changes ...".  Port of vc.el
    !> vc-allow-async-diff.
    logical, public :: vc_allow_async_diff = .false.

    !> Normal hook (list of functions) run after checking out a file.
    !> See `run_hooks'.  Port of vc.el vc-checkout-hook.
    type(hook_type), target, public :: vc_checkout_hook = hook_type(name="vc_checkout_hook")

    !> Normal hook (list of functions) run after commit or file checkin.
    !> See also `log_edit_done_hook'.  Port of vc.el vc-checkin-hook.
    type(hook_type), target, public :: vc_checkin_hook = hook_type(name="vc_checkin_hook")

    !> Normal hook (list of functions) run before a commit or a file checkin.
    !> See `run_hooks'.  Port of vc.el vc-before-checkin-hook.
    type(hook_type), target, public :: vc_before_checkin_hook = hook_type(name="vc_before_checkin_hook")

    !> Values of `vc_revert_show_diff' (nil, t, `kill').
    integer, parameter, public :: VC_REVERT_SHOW_DIFF_NIL = 0, VC_REVERT_SHOW_DIFF_T = 1, VC_REVERT_SHOW_DIFF_KILL = 2

    !> If true, `vc_revert' shows a `vc_diff' buffer before querying.
    !> VC_REVERT_SHOW_DIFF_T shows it and buries it afterwards,
    !> VC_REVERT_SHOW_DIFF_KILL shows it and kills it afterwards, and
    !> VC_REVERT_SHOW_DIFF_NIL doesn't show it.  Port of vc.el
    !> vc-revert-show-diff.
    integer, public :: vc_revert_show_diff = VC_REVERT_SHOW_DIFF_T

    !> If true, `vc_find_revision' doesn't write the created buffer to file.
    !> Port of vc.el vc-find-revision-no-save.
    logical, public :: vc_find_revision_no_save = .false.

    !> List of modes not supported by VC where backend should be deduced.
    !> In these modes the backend is deduced based on `default_directory'.
    !> If `vc_deduce_backend_all_modes' is true, the backend is deduced in
    !> all modes.  Port of vc.el vc-deduce-backend-nonvc-modes (its t is
    !> `vc_deduce_backend_all_modes').
    character(len=32), allocatable, public :: vc_deduce_backend_nonvc_modes(:)

    !> The value t of `vc_deduce_backend_nonvc_modes': deduce the backend
    !> in all modes.
    logical, public :: vc_deduce_backend_all_modes = .false.

    !> If true, diff added files by comparing them to /dev/null.  Port of
    !> vc.el vc-diff-added-files.
    logical, public :: vc_diff_added_files = .false.

    ! ------------------------------------------------------------------
    ! Buffer-local state
    ! ------------------------------------------------------------------

    !> A buffer's VC variables, by serial number: `vc_buffer_overriding_fileset'
    !> (a backend and files), `vc_buffer_revision', and the arguments its
    !> `revert_buffer_function' runs `vc_diff_internal' with.
    type :: vc_locals_type
        integer(int64) :: serial = 0
        logical :: has_override = .false.
        type(vc_fileset_type) :: override
        character(len=:), allocatable :: revision
        logical :: has_diff = .false.
        logical :: diff_async = .false., diff_verbose = .false.
        type(vc_fileset_type) :: diff_fileset
        character(len=:), allocatable :: diff_rev1, diff_rev2
    end type vc_locals_type

    type(vc_locals_type), allocatable :: locals(:)

contains

    ! ------------------------------------------------------------------
    ! Helpers
    ! ------------------------------------------------------------------

    !> Append S to LIST.
    subroutine push(list, s)
        type(vc_string_type), allocatable, intent(inout) :: list(:)
        character(len=*), intent(in) :: s
        type(vc_string_type), allocatable :: grown(:)
        integer :: n
        if (.not. allocated(list)) allocate (list(0))
        n = size(list)
        allocate (grown(n + 1))
        grown(1:n) = list
        grown(n + 1)%s = s
        call move_alloc(grown, list)
    end subroutine push

    !> A list of the one string S.
    subroutine one(list, s)
        type(vc_string_type), allocatable, intent(out) :: list(:)
        character(len=*), intent(in) :: s
        allocate (list(1))
        list(1)%s = s
    end subroutine one

    !> Whether S is one of LIST.
    logical function member(s, list)
        character(len=*), intent(in) :: s
        type(vc_string_type), intent(in) :: list(:)
        integer :: k
        member = .false.
        do k = 1, size(list)
            if (list(k)%s == s .and. len(list(k)%s) == len(s)) then
                member = .true.
                return
            end if
        end do
    end function member

    !> FILE expanded.
    function expand(file, dir) result(full)
        character(len=*), intent(in) :: file
        character(len=*), intent(in), optional :: dir
        character(len=:), allocatable :: full
        type(status_type) :: st
        call expand_file_name(file, full, st, dir)
        if (.not. st%ok()) full = file
    end function expand

    !> The current buffer's `default_directory'.
    function default_directory() result(dir)
        character(len=:), allocatable :: dir
        dir = "./"
        if (cur == 0) return
        if (allocated(buffers(cur)%directory)) dir = buffers(cur)%directory
    end function default_directory

    !> The current buffer's visited file ("" for none).
    function buffer_file_name() result(file)
        character(len=:), allocatable :: file
        file = ""
        if (cur == 0) return
        if (allocated(buffers(cur)%filename)) file = buffers(cur)%filename
    end function buffer_file_name

    !> Whether the current buffer is modified.
    logical function buffer_modified_p(h)
        type(handle_type), intent(in) :: h
        buffer_modified_p = .false.
        if (.not. buffer_live_p(h)) return
        buffer_modified_p = buffers(h%index)%text%save_modiff < buffers(h%index)%text%modiff
    end function buffer_modified_p

    !> Whether the current buffer's major mode is MODE or derives from it.
    logical function derived_mode_p(mode)
        use derived_fm, only: provided_mode_derived_p
        character(len=*), intent(in) :: mode
        derived_mode_p = .false.
        if (cur == 0) return
        if (.not. allocated(buffers(cur)%major_mode)) return
        derived_mode_p = provided_mode_derived_p(buffers(cur)%major_mode, mode)
    end function derived_mode_p

    !> N as a decimal string.
    function itoa(n) result(s)
        integer, intent(in) :: n
        character(len=:), allocatable :: s
        character(len=16) :: buf
        write (buf, "(i0)") n
        s = trim(buf)
    end function itoa

    !> The index of the current buffer's VC variables in `locals' (made
    !> with CREATE; 0 if none).
    integer function local_index(create) result(k)
        logical, intent(in) :: create
        type(vc_locals_type), allocatable :: grown(:)
        integer :: n
        k = 0
        if (cur == 0) return
        if (.not. allocated(locals)) allocate (locals(0))
        do k = 1, size(locals)
            if (locals(k)%serial == buffers(cur)%serial) return
        end do
        k = 0
        if (.not. create) return
        n = size(locals)
        allocate (grown(n + 1))
        grown(1:n) = locals
        grown(n + 1)%serial = buffers(cur)%serial
        call move_alloc(grown, locals)
        k = n + 1
    end function local_index

    !> Forget the current buffer's VC variables (kill-all-local-variables).
    subroutine kill_vc_locals()
        logical, allocatable :: keep(:)
        integer :: k
        if (.not. allocated(locals) .or. cur == 0) return
        allocate (keep(size(locals)))
        do k = 1, size(locals)
            keep(k) = locals(k)%serial /= buffers(cur)%serial
        end do
        locals = pack(locals, keep)
    end subroutine kill_vc_locals

    !> The current buffer's `vc_buffer_revision' ("" for nil): the VCS
    !> revision to which this buffer's contents corresponds.  Port of vc.el
    !> vc-buffer-revision.
    function vc_buffer_revision() result(rev)
        character(len=:), allocatable :: rev
        integer :: k
        rev = ""
        k = local_index(.false.)
        if (k == 0) return
        if (allocated(locals(k)%revision)) rev = locals(k)%revision
    end function vc_buffer_revision

    ! ------------------------------------------------------------------
    ! Switches
    ! ------------------------------------------------------------------

    !> Return a list of vc-BACKEND switches for operation OP.
    !> B is the backend.  OP is an operation such as "diff".
    !>
    !> In decreasing order of preference, return the value of:
    !> vc-BACKEND-OP-switches (e.g. `vc_git_diff_switches');
    !> vc-OP-switches (e.g. `vc_diff_switches'); or, in the case of
    !> diff only, `diff_switches'.
    !>
    !> If the chosen value is t, return no switches.
    !> This is so that you may set, e.g. `vc_git_diff_switches' to t in order
    !> to override the value of `vc_diff_switches' and `diff_switches'.
    !> Port of vc.el:vc-switches (vc-BACKEND-OP-switches only for diff).
    subroutine vc_switches(b, op, switches)
        integer, intent(in) :: b
        character(len=*), intent(in) :: op
        type(vc_string_type), allocatable, intent(out) :: switches(:)
        type(vc_switches_type) :: sw
        allocate (switches(0))
        sw = vc_switches_type()
        if (b > 0 .and. op == "diff") then
            if (associated(vc_backends(b)%diff_switches)) sw = vc_backends(b)%diff_switches
        end if
        if (sw%kind == VC_SWITCHES_NIL) then
            select case (op)
            case ("diff")
                sw = vc_diff_switches
            case ("checkin")
                sw = vc_checkin_switches
            case ("checkout")
                sw = vc_checkout_switches
            case ("register")
                sw = vc_register_switches
            case ("annotate")
                sw = vc_annotate_switches
            case default
                ! Not an operation with switches options.
            end select
        end if
        if (sw%kind == VC_SWITCHES_NIL .and. op == "diff") then
            if (allocated(diff_switches)) call one(switches, diff_switches)
            return
        end if
        if (sw%kind == VC_SWITCHES_LIST) then
            if (allocated(sw%list)) switches = sw%list
        end if
    end subroutine vc_switches

    ! ------------------------------------------------------------------
    ! Deducing the fileset and backend
    ! ------------------------------------------------------------------

    !> Return the root directory for the current VC tree (FOUND false if
    !> the root directory cannot be identified).
    !> B is the VC backend.  Port of vc.el:vc-root-dir.
    subroutine vc_root_dir(root, found, status, backend)
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
        if (b == 0) b = vc_deduce_backend()
        if (b == 0) return
        call vc_call_root(b, default_directory(), root, found, status)
        if (status%code == FM_VC_NOT_SUPPORTED) status = status_type()
    end subroutine vc_root_dir

    !> Return a backend that can be used for registering FILE.
    !>
    !> If no backend declares itself responsible for FILE, then FILE
    !> must not be in a version controlled directory, so try to create a
    !> repository, prompting for the directory and the VC backend to
    !> use.  Port of vc.el:vc-backend-for-registration.
    recursive integer function vc_backend_for_registration(file, status) result(bk)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: names(:)
        character(len=:), allocatable :: dir, answer, def_dir, repo_dir, saved
        logical :: found
        integer :: k, b, maxlen, n
        status = status_type()
        bk = 0
        ! First try: find a responsible backend, it must be a backend
        ! under which FILE is not yet registered and with the most
        ! specific path to FILE.
        maxlen = 0
        do k = 1, size(vc_handled_backends)
            b = vc_backend_named(vc_handled_backends(k))
            if (b == 0) cycle
            if (associated(vc_backends(b)%registered)) then
                if (vc_backends(b)%registered(file)) cycle
            end if
            call vc_call_responsible_p(b, file, dir, found)
            if (.not. found) cycle
            n = count_components(expand(dir))
            if (n > maxlen) then
                maxlen = n
                bk = b
            end if
        end do
        if (bk /= 0) return
        ! no responsible backend
        allocate (names(0))
        do k = size(vc_handled_backends), 1, -1
            b = vc_backend_named(vc_handled_backends(k))
            if (b == 0) cycle
            if (associated(vc_backends(b)%create_repo)) names = [names, make_string(vc_backends(b)%name)]
        end do
        ! Read the VC backend from the user, only complete with the
        ! backends that have the 'create-repo method.
        call completing_read(file//" is not in a version controlled directory."//achar(10)//"Use VC backend: ", &
                             completion_list_table(make_list(names)), answer, status, require_match=REQUIRE_MATCH_T)
        if (.not. status%ok()) return
        bk = vc_backend_named(answer)
        if (bk == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Unknown backend "//answer)
            return
        end if
        ! read the directory where to create the repository, make sure
        ! it's a parent of file.
        def_dir = file_name_directory(file)
        call read_file_name("create "//answer//" repository in: ", repo_dir, status, dir=default_directory(), &
                            default_filename=def_dir, mustmatch=REQUIRE_MATCH_T)
        if (.not. status%ok()) return
        saved = default_directory()
        buffers(cur)%directory = file_name_as_directory(expand(repo_dir))
        call vc_backends(bk)%create_repo(status)
        buffers(cur)%directory = saved
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
    end function vc_backend_for_registration

    !> Return the backend that is responsible for FILE.
    !>
    !> If FILE is already registered, return the
    !> backend of FILE.  If FILE is not registered, then the
    !> first backend in `vc_handled_backends' that declares itself
    !> responsible for FILE is returned.
    !>
    !> Note that if FILE is a symbolic link, it will not be resolved --
    !> the responsible backend system for the symbolic link itself will
    !> be reported.
    !>
    !> If NO_ERROR is false or absent, signal an error that no VC backend is
    !> responsible for the given file.  Port of vc.el:vc-responsible-backend.
    integer function vc_responsible_backend(file, status, no_error) result(bk)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: no_error
        character(len=:), allocatable :: full, dir, best
        logical :: found, quiet
        integer :: k, b
        status = status_type()
        bk = 0
        if (.not. file_directory_p(file)) bk = vc_backend(file)
        if (bk /= 0) return
        ! Several roots; we seem to have one vc inside another's directory.
        ! Choose the most specific.
        full = expand(file)
        best = ""
        do k = 1, size(vc_handled_backends)
            b = vc_backend_named(vc_handled_backends(k))
            if (b == 0) cycle
            call vc_call_responsible_p(b, full, dir, found)
            if (.not. found) cycle
            ! We run DIR through `expand-file-name' so that abbreviated
            ! directories, such as "~/", wouldn't look "less specific" due
            ! to their artificially shorter length.
            dir = expand(dir)
            if (bk == 0 .or. len(dir) > len(best)) then
                bk = b
                best = dir
            end if
        end do
        if (bk /= 0) return
        quiet = .false.
        if (present(no_error)) quiet = no_error
        if (.not. quiet) call set_error(status, FM_GENERIC_ERROR, "No VC backend is responsible for "//full)
    end function vc_responsible_backend

    !> The backend of the current buffer: of its overriding fileset, of a
    !> Diff buffer made by VC, of `default_directory' in the modes of
    !> `vc_deduce_backend_nonvc_modes', or of the visited file.  Port of
    !> vc.el:vc-deduce-backend (VC-Dir and Log buffers are deferred).
    integer function vc_deduce_backend() result(b)
        type(status_type) :: st
        integer :: k
        logical :: nonvc
        b = 0
        k = local_index(.false.)
        if (k > 0) then
            if (locals(k)%has_override) then
                b = locals(k)%override%backend
                return
            end if
        end if
        if (derived_mode_p("diff_mode")) then
            b = vc_backend_named(diff_vc_backend())
            return
        end if
        nonvc = vc_deduce_backend_all_modes
        if (.not. nonvc .and. allocated(vc_deduce_backend_nonvc_modes)) then
            do k = 1, size(vc_deduce_backend_nonvc_modes)
                if (derived_mode_p(trim(vc_deduce_backend_nonvc_modes(k)))) nonvc = .true.
            end do
        end if
        if (nonvc) then
            b = vc_responsible_backend(default_directory(), st)
            if (.not. st%ok()) b = 0
            return
        end if
        if (allocated(buffers(cur)%vc_mode)) b = vc_backend(buffer_file_name())
    end function vc_deduce_backend

    !> Deduce a set of files and a backend to which to apply an operation.
    !> Gives FS, of the form:
    !>
    !>   (BACKEND FILESET FILESET-ONLY-FILES STATE CHECKOUT-MODEL)
    !>
    !> where the last 3 members are optional, and must be present only if
    !> STATE_MODEL_ONLY_FILES is true.
    !>
    !> NOT_STATE_CHANGING, if true, means that the operation
    !> requesting the fileset doesn't intend to change the VC state,
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
    !> Otherwise, throw an error.
    !>
    !> STATE_MODEL_ONLY_FILES, if true, means that the caller needs
    !> the FILESET-ONLY-FILES, STATE, and CHECKOUT-MODEL info, where
    !> FILESET-ONLY-FILES means only files in similar VC states,
    !> possible values of STATE are explained in `vc_state', and MODEL in
    !> `vc_checkout_model'.  Otherwise, these 3 members may be omitted from
    !> the returned list.
    !>
    !> BEWARE: this function may change the current buffer.  Port of
    !> vc.el:vc-deduce-fileset (VC-Dir and Log View buffers are deferred).
    recursive subroutine vc_deduce_fileset(fs, status, not_state_changing, allow_unregistered, state_model_only_files)
        type(vc_fileset_type), intent(out) :: fs
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: not_state_changing, allow_unregistered, state_model_only_files
        type(handle_type) :: parent
        type(status_type) :: st
        character(len=:), allocatable :: file
        logical :: nsc, au, smo, ok
        integer :: k, b
        status = status_type()
        nsc = .false.
        au = .false.
        smo = .false.
        if (present(not_state_changing)) nsc = not_state_changing
        if (present(allow_unregistered)) au = allow_unregistered
        if (present(state_model_only_files)) smo = state_model_only_files
        k = local_index(.false.)
        if (k > 0) then
            if (locals(k)%has_override .and. .not. smo) then
                fs = locals(k)%override
                return
            end if
        end if
        file = buffer_file_name()
        if (derived_mode_p("dired_mode")) then
            call dired_vc_deduce_fileset(fs, status, smo, nsc)
            return
        end if
        if (derived_mode_p("diff_mode") .and. len(file) == 0) then
            call diff_vc_deduce_fileset(fs, status)
            return
        end if
        b = vc_backend(file)
        if (b /= 0) then
            fs%backend = b
            call one(fs%files, file)
            if (smo) then
                call one(fs%only_files, file)
                fs%state = vc_state(file)
                fs%model = vc_checkout_model(b, fs%files)
                fs%has_state = .true.
            end if
            return
        end if
        parent = vc_parent_buffer()
        if (buffer_live_p(parent)) then
            ok = allocated(buffers(parent%index)%filename)
            if (.not. ok) then
                call set_buffer(parent, st)
                ok = derived_mode_p("dired_mode") .or. derived_mode_p("diff_mode")
            end if
            if (ok) then
                call set_buffer(parent, st)
                call vc_deduce_fileset(fs, status, not_state_changing, allow_unregistered, state_model_only_files)
                return
            end if
        end if
        if (len(file) == 0) then
            b = vc_responsible_backend(default_directory(), st)
            if (st%ok() .and. b /= 0) then
                fs%backend = b
                call one(fs%files, default_directory())
                return
            end if
        end if
        if (au .and. len(file) > 0) then
            if (.not. vc_registered(file)) then
                fs%backend = vc_backend_for_registration(file, status)
                if (.not. status%ok()) return
                call one(fs%files, file)
                if (smo) then
                    call one(fs%only_files, file)
                    fs%state = VC_UNREGISTERED
                    fs%model = VC_MODEL_NIL
                    fs%has_state = .true.
                end if
                return
            end if
        end if
        call set_error(status, FM_GENERIC_ERROR, "File is not under version control")
    end subroutine vc_deduce_fileset

    !> The fileset of a Dired buffer: the marked files (or the file at
    !> point) and the backend responsible for the directory (or one to
    !> register them with).  Port of dired-aux.el:dired-vc-deduce-fileset.
    recursive subroutine dired_vc_deduce_fileset(fs, status, state_model_only_files, not_state_changing)
        type(vc_fileset_type), intent(out) :: fs
        type(status_type), intent(out) :: status
        logical, intent(in) :: state_model_only_files, not_state_changing
        type(dired_string_type), allocatable :: files(:)
        type(vc_string_type), allocatable :: names(:)
        integer, allocatable :: states(:)
        type(status_type) :: st
        integer :: k
        call dired_get_marked_files(files, status, error="")
        if (.not. status%ok()) return
        allocate (names(size(files)))
        do k = 1, size(files)
            names(k)%s = files(k)%s
        end do
        fs%backend = vc_responsible_backend(default_directory(), st, no_error=.true.)
        if (fs%backend == 0 .and. size(names) > 0) then
            fs%backend = vc_backend_for_registration(names(1)%s, status)
            if (.not. status%ok()) return
        end if
        if (.not. not_state_changing) then
            do k = 1, size(names)
                if (file_directory_p(names(k)%s)) then
                    call set_error(status, FM_USER_ERROR, &
                                   "State-changing VC operations on directories supported only from VC-Dir")
                    return
                end if
            end do
        end if
        fs%files = names
        if (state_model_only_files) then
            allocate (states(size(names)))
            do k = 1, size(names)
                states(k) = vc_state(names(k)%s)
            end do
            call vc_only_files_state_and_model(names, states, fs%backend, fs, status)
        end if
    end subroutine dired_vc_deduce_fileset

    !> The fileset of a Diff buffer not visiting a file: the files the
    !> patch changes, and the model `patch' (C-x v v applies it).  Port of
    !> diff-mode.el:diff-vc-deduce-fileset.
    recursive subroutine diff_vc_deduce_fileset(fs, status)
        type(vc_fileset_type), intent(out) :: fs
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: name
        integer(int64) :: saved
        logical :: found
        if (buffer_narrowed_p()) then
            ! If user used `diff-restrict-view' then we may not have the
            ! file header, and the commit will not succeed (bug#73387).
            call set_error(status, FM_USER_ERROR, &
                           "Cannot commit patch when narrowed; consider C-x n w C-c RET n C-x v v")
            return
        end if
        fs%backend = vc_responsible_backend(default_directory(), status)
        if (.not. status%ok()) return
        allocate (fs%files(0))
        saved = point()
        call goto_char(point_min())
        do
            call diff_file_next(status)
            if (.not. status%ok()) exit
            if (eobp()) exit
            call diff_find_file_name(name, found, status, noprompt=.true.)
            if (.not. status%ok()) exit
            if (found) call push(fs%files, name)
        end do
        call goto_char(saved)
        if (.not. status%ok()) return
        fs%patch = .true.
        fs%has_state = .true.
        allocate (fs%only_files(0))
    end subroutine diff_vc_deduce_fileset

    !> Make sure that the current buffer visits a version-controlled file.
    !> Port of vc.el:vc-ensure-vc-buffer.
    recursive subroutine vc_ensure_vc_buffer(status)
        use dired_mode_fm, only: dired_get_filename
        type(status_type), intent(out) :: status
        type(handle_type) :: parent, buf
        type(status_type) :: st
        character(len=:), allocatable :: file
        logical :: found
        status = status_type()
        if (derived_mode_p("dired_mode")) then
            call dired_get_filename(file, found, status)
            if (.not. status%ok()) return
            call find_file_noselect(file, buf, status)
            if (.not. status%ok()) return
            call set_buffer(buf, st)
        else
            do
                parent = vc_parent_buffer()
                if (.not. buffer_live_p(parent)) exit
                ! Avoid infinite looping when vc-parent-buffer and
                ! current buffer are the same buffer.
                if (parent%index == cur) exit
                call set_buffer(parent, st)
            end do
        end if
        if (len(buffer_file_name()) == 0) then
            call set_error(status, FM_GENERIC_ERROR, "Buffer "//buffers(cur)%name//" is not associated with a file")
        else if (vc_backend(buffer_file_name()) == 0) then
            call set_error(status, FM_GENERIC_ERROR, "File "//buffer_file_name()//" is not under version control")
        end if
    end subroutine vc_ensure_vc_buffer

    !> Compute the last three `vc_deduce_fileset' return value elements
    !> for FILES in STATES into FS.  B is the VC backend responsible for
    !> FILES.  Port of vc.el:vc-only-files-state-and-model.
    recursive subroutine vc_only_files_state_and_model(files, states, b, fs, status)
        type(vc_string_type), intent(in) :: files(:)
        integer, intent(in) :: states(:)
        integer, intent(in) :: b
        type(vc_fileset_type), intent(inout) :: fs
        type(status_type), intent(out) :: status
        integer, allocatable :: distinct(:)
        integer :: k, j, first, second, s
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
        else if (all(distinct == VC_ADDED .or. distinct == VC_MISSING .or. distinct == VC_REMOVED .or. &
                     distinct == VC_EDITED)) then
            s = VC_EDITED
        else if (all(distinct == VC_ADDED .or. distinct == VC_MISSING .or. distinct == VC_REMOVED .or. &
                     distinct == VC_EDITED .or. distinct == VC_UNREGISTERED)) then
            ! Special, but common case: checking in both changes and new
            ! files at once.  The actual registration is delayed until
            ! `vc-checkin' so that if the user changes their mind while
            ! entering the log message, we leave things as we found them.
            call y_or_n_p("Some files are unregistered; register them before checking in?", yes, status)
            if (.not. status%ok()) return
            if (.not. yes) then
                call clash()
                return
            end if
            s = VC_EDITED
        else
            call clash()
            return
        end if
        fs%state = s
        fs%model = VC_MODEL_NIL
        if (s /= VC_STATE_NIL .and. s /= VC_UNREGISTERED) fs%model = vc_checkout_model(b, files)
    contains

        logical function compatible(state)
            integer, intent(in) :: state
            compatible = state == VC_ADDED .or. state == VC_MISSING .or. state == VC_REMOVED .or. state == VC_EDITED
        end function compatible

        !> The error naming two files in clashing states.
        subroutine clash()
            use files_fm, only: abbreviate_file_name
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
                do k = 1, size(distinct)
                    if (distinct(k) /= first) then
                        second = distinct(k)
                        exit
                    end if
                end do
            end if
            a = 1
            c = 1
            do j = 1, size(states)
                if (states(j) == first) then
                    a = j
                    exit
                end if
            end do
            do j = 1, size(states)
                if (states(j) == second) then
                    c = j
                    exit
                end if
            end do
            call set_error(status, FM_GENERIC_ERROR, &
                           "To apply VC operations to multiple files, the files must be in similar VC states."// &
                           achar(10)//abbreviate_file_name(files(a)%s)//" in state `"//vc_state_name(first)// &
                           "' clashes with"//achar(10)//abbreviate_file_name(files(c)%s)//" in state `"// &
                           vc_state_name(second)//"'")
        end subroutine clash
    end subroutine vc_only_files_state_and_model

    !> Read a backend name with PROMPT (completing over
    !> `vc_handled_backends').  Port of vc.el:vc-read-backend.
    recursive integer function vc_read_backend(prompt, status) result(b)
        character(len=*), intent(in) :: prompt
        type(status_type), intent(out) :: status
        type(value_type), allocatable :: names(:)
        character(len=:), allocatable :: answer
        type(value_type) :: saved
        type(status_type) :: st
        integer :: k
        b = 0
        allocate (names(size(vc_handled_backends)))
        do k = 1, size(vc_handled_backends)
            names(k) = make_string(trim(vc_handled_backends(k)))
        end do
        call symbol_value(sym("completion_ignore_case"), saved, st)
        call set(sym("completion_ignore_case"), nil(), st)
        call completing_read(prompt, completion_list_table(make_list(names)), answer, status, &
                             require_match=REQUIRE_MATCH_T)
        call set(sym("completion_ignore_case"), saved, st)
        if (.not. status%ok()) return
        b = vc_backend_named(answer)
        if (b == 0) call set_error(status, FM_GENERIC_ERROR, "VC backend "//answer//" is not ported yet")
    end function vc_read_backend

    ! ------------------------------------------------------------------
    ! C-x v v
    ! ------------------------------------------------------------------

    !> Do the next logical version control operation on the current fileset.
    !> This requires that all files in the current VC fileset be in the
    !> sufficiently similar states.  If they are not, signal an error.
    !> Also signal an error if files in the fileset are ignored by the VCS.
    !>
    !> For modern merging-based version control systems:
    !>   If every file in the fileset is not registered for version
    !>    control, register the fileset (but don't commit).  If VERBOSE is
    !>    true (interactively, the prefix argument), ask for the VC
    !>    backend with which to register the fileset.
    !>   If every work file in the VC fileset is either added or modified,
    !>    pop up a *vc-log* buffer to commit the fileset changes.
    !>   (If some are added or modified and some are unregistered, offer to
    !>    register the unregistered ones, first.)
    !>   For a centralized version control system, if any work file in
    !>    the VC fileset is out of date, offer to update the fileset.
    !>
    !> For old-style locking-based version control systems, like RCS:
    !>   If every file is not registered, register the file(s); with a prefix
    !>    argument, allow to specify the VC backend for registration.
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
    !> will automatically deduce which VC repository to register it
    !> with, using the most specific one.
    !>
    !> If VERBOSE is true (interactively, the prefix argument),
    !> you can specify another VC backend for the file(s),
    !> or (for centralized VCS only) the revision ID or branch ID
    !> from which to check out the file(s).  Port of vc.el:vc-next-action
    !> (committing is deferred: Log Edit mode).
    recursive subroutine vc_next_action(verbose, status)
        logical, intent(in) :: verbose
        type(status_type), intent(out) :: status
        type(vc_fileset_type) :: fs, reg
        character(len=:), allocatable :: revision
        integer :: state, model, b, k
        status = status_type()
        call vc_deduce_fileset(fs, status, .false., .true., .true.)
        if (.not. status%ok()) return
        state = fs%state
        model = fs%model
        ! If a buffer has unsaved changes, a checkout would discard those
        ! changes, so treat the buffer as having unlocked changes.
        if (model /= VC_MODEL_IMPLICIT .and. state == VC_UP_TO_DATE .and. .not. fs%patch) then
            do k = 1, size(fs%files)
                if (buffer_modified_p(get_file_buffer(fs%files(k)%s))) state = VC_UNLOCKED_CHANGES
            end do
        end if
        ! Do the right thing.
        if (state == VC_IGNORED) then
            call set_error(status, FM_GENERIC_ERROR, "Fileset files are ignored by the version-control system")
        else if (fs%patch) then
            ! Fileset comes from a diff-mode buffer, see
            ! 'diff-vc-deduce-fileset', and the buffer is the patch to apply.
            call checkin_deferred()
        else if (state == VC_STATE_NIL .or. state == VC_UNREGISTERED) then
            if (verbose) then
                b = vc_read_backend("Backend to register to: ", status)
                if (.not. status%ok()) return
                reg = fs
                reg%backend = b
                call vc_register(status, reg)
            else
                call vc_register(status, fs)
            end if
        else if (state == VC_MISSING) then
            call vc_delete_file(fs%only_files, status)
        else if (state == VC_UP_TO_DATE .or. (verbose .and. state == VC_NEEDS_UPDATE)) then
            ! Files are up-to-date, or need a merge and user specified a
            ! revision
            if (verbose) then
                ! Go to a different revision.
                call read_string("Branch, revision, or backend to move to: ", revision, status)
                if (.not. status%ok()) return
                if (is_backend_name(revision)) then
                    call set_error(status, FM_GENERIC_ERROR, "Transferring files between backends is not ported yet")
                    return
                end if
                do k = 1, size(fs%files)
                    call vc_checkout(fs%files(k)%s, status, revision)
                    if (.not. status%ok()) return
                end do
            else if (model /= VC_MODEL_IMPLICIT) then
                ! check the files out
                do k = 1, size(fs%files)
                    call vc_checkout(fs%files(k)%s, status)
                    if (.not. status%ok()) return
                end do
            else
                ! do nothing
                call message("Fileset is up-to-date")
            end if
        else if (state == VC_ADDED .or. state == VC_MISSING .or. state == VC_REMOVED .or. state == VC_EDITED) then
            ! Files have local changes
            call checkin_deferred()
        else if (state == VC_LOCKED) then
            call set_error(status, FM_GENERIC_ERROR, "Stealing locks (vc_steal_lock) is not ported yet")
        else if (state == VC_CONFLICT) then
            call vc_mark_resolved(fs%backend, fs%files, status)
        else if (state == VC_NEEDS_UPDATE .or. state == VC_NEEDS_MERGE .or. state == VC_UNLOCKED_CHANGES) then
            call set_error(status, FM_GENERIC_ERROR, "VC operations of centralized and locking backends are not ported yet")
        else
            call set_error(status, FM_GENERIC_ERROR, "Fileset is in an unknown state "//vc_state_name(state))
        end if
    contains

        subroutine checkin_deferred()
            call set_error(status, FM_GENERIC_ERROR, "Committing (vc_checkin) needs Log Edit mode, which is not ported yet")
        end subroutine checkin_deferred

        !> Whether S names a handled backend, ignoring case.
        logical function is_backend_name(s)
            use casefiddle_fm, only: downcase_string
            character(len=*), intent(in) :: s
            integer :: i
            is_backend_name = .false.
            do i = 1, size(vc_handled_backends)
                if (downcase_string(trim(vc_handled_backends(i))) == downcase_string(s)) is_backend_name = .true.
            end do
        end function is_backend_name
    end subroutine vc_next_action

    !> Create an empty repository in the current directory, with backend B.
    !> Port of vc.el:vc-create-repo.
    recursive subroutine vc_create_repo(b, status)
        integer, intent(in) :: b
        type(status_type), intent(out) :: status
        status = status_type()
        if (.not. associated(vc_backends(b)%create_repo)) then
            call vc_not_supported(status, "create-repo", b)
            return
        end if
        call vc_backends(b)%create_repo(status)
    end subroutine vc_create_repo

    !> Register into a version control system.
    !> If FILESET is given, register the files in that fileset.
    !> Otherwise register the current file.
    !> If COMMENT is present, use that as an initial comment.
    !>
    !> The version control system to use is found by cycling through the list
    !> `vc_handled_backends'.  The first backend in that list which declares
    !> itself responsible for the file (usually because other files in that
    !> directory are already registered under that backend) will be used to
    !> register the file.  If no backend declares itself responsible, the
    !> first backend that could register the file is used.  Port of
    !> vc.el:vc-register.
    recursive subroutine vc_register(status, fileset, comment)
        type(status_type), intent(out) :: status
        type(vc_fileset_type), intent(in), optional :: fileset
        character(len=*), intent(in), optional :: comment
        type(vc_fileset_type) :: fs
        type(handle_type) :: bname, old
        type(status_type) :: st
        character(len=:), allocatable :: root
        logical :: found
        integer :: b, k
        status = status_type()
        if (present(fileset)) then
            fs = fileset
        else
            call vc_deduce_fileset(fs, status, .false., .true.)
            if (.not. status%ok()) return
        end if
        b = fs%backend
        ! We used to operate on `only-files', but VC wants to provide the
        ! possibility to register directories rather than files only, since
        ! many VCS allow that as well.
        do k = 1, size(fs%files)
            if (associated(vc_backends(b)%registered)) then
                if (vc_backends(b)%registered(fs%files(k)%s)) then
                    call set_error(status, FM_GENERIC_ERROR, "This file is already registered: "//fs%files(k)%s)
                    return
                end if
            end if
            ! Watch out for new buffers of size 0: the corresponding file
            ! does not exist yet, even though buffer-modified-p is nil.
            bname = get_file_buffer(fs%files(k)%s)
            if (bname%index /= 0) then
                old = current_buffer()
                call set_buffer(bname, st)
                if (.not. buffer_modified_p(bname) .and. point_max() == point_min() .and. &
                    .not. file_exists_p(buffers(cur)%filename)) call restore_buffer_modified_p(.true.)
                call vc_buffer_sync(status)
                call set_buffer(old, st)
                if (.not. status%ok()) return
            end if
        end do
        call message("Registering ("//vc_delistify(fs%files)//")... ")
        do k = 1, size(fs%files)
            call vc_file_clearprops(fs%files(k)%s)
        end do
        if (.not. associated(vc_backends(b)%register)) then
            call vc_not_supported(status, "register", b)
            return
        end if
        call vc_backends(b)%register(fs%files, status, comment)
        if (.not. status%ok()) return
        call vc_root_dir(root, found, st, b)
        do k = 1, size(fs%files)
            call vc_file_set_backend(fs%files(k)%s, b)
            bname = get_file_buffer(fs%files(k)%s)
            if (bname%index /= 0) then
                if (.not. vc_make_backup_files) buffers(bname%index)%backup_inhibited = .true.
            end if
            call vc_resynch_buffer(fs%files(k)%s, root, st, keep=.true., noquery=VC_NOQUERY_T)
        end do
        call message("Registering ("//vc_delistify(fs%files)//")... done")
    end subroutine vc_register

    ! ------------------------------------------------------------------
    ! Ignoring
    ! ------------------------------------------------------------------

    !> Ignore FILE under the VCS of DIRECTORY.
    !>
    !> Normally, FILE is a wildcard specification that matches the files
    !> to be ignored.  When REMOVE is true, remove FILE from the list
    !> of ignored files.
    !>
    !> DIRECTORY defaults to `default_directory' and is used to
    !> determine the responsible VC backend.
    !>
    !> When called interactively, prompt for a FILE to ignore, unless a
    !> prefix argument is given, in which case prompt for a file FILE to
    !> remove from the list of ignored files.  Port of vc.el:vc-ignore
    !> (with vc-default-ignore; no backend has its own).
    recursive subroutine vc_ignore(file, status, directory, remove)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: directory
        logical, intent(in), optional :: remove
        character(len=:), allocatable :: dir, ignore
        logical :: found, rm
        integer :: b
        status = status_type()
        dir = default_directory()
        if (present(directory)) dir = directory
        b = vc_responsible_backend(default_directory(), status)
        if (.not. status%ok()) then
            call set_error(status, FM_GENERIC_ERROR, "Unknown backend")
            return
        end if
        rm = .false.
        if (present(remove)) rm = remove
        ! vc-default-ignore: FILE is a wildcard specification relative to
        ! DIRECTORY.
        if (.not. associated(vc_backends(b)%find_ignore_file)) then
            call vc_not_supported(status, "find-ignore-file", b)
            return
        end if
        call vc_backends(b)%find_ignore_file(dir, ignore, found)
        if (rm) then
            call vc_remove_regexp("^"//regexp_quote(file)//"\("//achar(10)//"\|$\)", ignore, status)
        else
            call vc_add_line(file, ignore, status)
        end if
    end subroutine vc_ignore

    !> The directory `vc_ignore' reads file names relative to: that of the
    !> ignore file of `default_directory''s backend.  Port of
    !> vc.el:vc--ignore-base-dir.
    function vc_ignore_base_dir(status) result(dir)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: dir
        character(len=:), allocatable :: ignore
        logical :: found
        integer :: b
        dir = default_directory()
        b = vc_responsible_backend(default_directory(), status)
        if (.not. status%ok()) return
        if (.not. associated(vc_backends(b)%find_ignore_file)) return
        call vc_backends(b)%find_ignore_file(default_directory(), ignore, found)
        if (found) dir = file_name_directory(ignore)
    end function vc_ignore_base_dir

    !> Add STRING as a line to FILE.  Port of vc.el:vc--add-line.
    recursive subroutine vc_add_line(string, file, status)
        character(len=*), intent(in) :: string, file
        type(status_type), intent(out) :: status
        type(handle_type) :: buf, old
        type(status_type) :: st
        integer(int64) :: found
        call find_file_noselect(file, buf, status)
        if (.not. status%ok()) return
        old = current_buffer()
        call set_buffer(buf, st)
        call goto_char(point_min())
        call re_search_forward("^"//regexp_quote(string)//"$", found, st, noerror=SEARCH_NOERROR)
        if (found == 0) then
            call goto_char(point_max())
            if (.not. bolp()) call insert(achar(10), status)
            call insert(string//achar(10), status)
            if (status%ok()) call save_buffer(1_int64, .false., status)
        end if
        call set_buffer(old, st)
    end subroutine vc_add_line

    !> Remove all matching for REGEXP in FILE.  Port of
    !> vc.el:vc--remove-regexp.
    recursive subroutine vc_remove_regexp(regexp, file, status)
        character(len=*), intent(in) :: regexp, file
        type(status_type), intent(out) :: status
        type(handle_type) :: buf, old
        type(status_type) :: st
        integer(int64) :: found
        status = status_type()
        if (.not. file_exists_p(file)) return
        call find_file_noselect(file, buf, status)
        if (.not. status%ok()) return
        old = current_buffer()
        call set_buffer(buf, st)
        call goto_char(point_min())
        do
            call re_search_forward(regexp, found, st, noerror=SEARCH_NOERROR)
            if (found == 0) exit
            call replace_match("", status)
            if (.not. status%ok()) exit
        end do
        if (status%ok()) call save_buffer(1_int64, .false., status)
        call set_buffer(old, st)
    end subroutine vc_remove_regexp

    ! ------------------------------------------------------------------
    ! Checking out, resolving
    ! ------------------------------------------------------------------

    !> Retrieve a copy of the revision REV of FILE.
    !> REV defaults to the latest revision.
    !>
    !> After check-out, runs the normal hook `vc_checkout_hook'.  Port of
    !> vc.el:vc-checkout.
    recursive subroutine vc_checkout(file, status, rev)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: rev
        type(vc_string_type), allocatable :: flist(:)
        type(file_stat_type) :: attr
        type(status_type) :: st
        character(len=:), allocatable :: root
        logical :: found
        integer :: b
        status = status_type()
        if (.not. present(rev)) then
            b = vc_backend(file)
            if (b /= 0) then
                if (vc_call_make_version_backups_p(b, file) .and. vc_up_to_date_p(file)) call vc_make_version_backup(file)
            end if
        end if
        b = vc_backend(file)
        if (b == 0) then
            call set_error(status, FM_GENERIC_ERROR, "File "//file//" is not under version control")
            return
        end if
        if (.not. associated(vc_backends(b)%checkout)) then
            call vc_not_supported(status, "checkout", b)
            return
        end if
        call with_properties_begin([string_of(file)], flist)
        call vc_backends(b)%checkout(file, status, rev)
        if (.not. status%ok()) return
        call file_attributes(file, attr, found)
        do b = 1, size(flist)
            if (.not. vc_touched(VC_PROP_STATE)) then
                if (vc_checkout_model(vc_backend(file), flist) == VC_MODEL_IMPLICIT) then
                    call vc_file_set_state(flist(b)%s, VC_UP_TO_DATE)
                else
                    call vc_file_set_state(flist(b)%s, VC_EDITED)
                end if
            end if
            if (.not. vc_touched(VC_PROP_CHECKOUT_TIME)) then
                if (found) then
                    call vc_file_set_checkout_time(flist(b)%s, attr%mtime_sec, attr%mtime_nsec)
                else
                    call vc_file_set_checkout_time(flist(b)%s)
                end if
            end if
        end do
        call vc_root_dir(root, found, st)
        call vc_resynch_buffer(file, root, status, keep=.true., noquery=VC_NOQUERY_T)
        if (.not. status%ok()) return
        call vc_checkout_hook%run(status)
    end subroutine vc_checkout

    !> A one-string entry.
    function string_of(s) result(e)
        character(len=*), intent(in) :: s
        type(vc_string_type) :: e
        e%s = s
    end function string_of

    !> The files of with-vc-properties for FILES: each file, and for a
    !> directory the visited files of buffers in it; start recording the
    !> properties set.
    subroutine with_properties_begin(files, flist)
        type(vc_string_type), intent(in) :: files(:)
        type(vc_string_type), allocatable, intent(out) :: flist(:)
        type(handle_type), allocatable :: all(:)
        integer :: k, j
        allocate (flist(0))
        call vc_touch_reset()
        do k = 1, size(files)
            if (file_directory_p(files(k)%s)) then
                all = buffer_list()
                do j = 1, size(all)
                    if (.not. allocated(buffers(all(j)%index)%filename)) cycle
                    if (index(buffers(all(j)%index)%filename, files(k)%s) == 1) then
                        call push(flist, buffers(all(j)%index)%filename)
                    end if
                end do
            else
                call push(flist, files(k)%s)
            end if
        end do
    end subroutine with_properties_begin

    !> Mark the conflicts in FILES of backend B as resolved.  Port of
    !> vc.el:vc-mark-resolved.
    recursive subroutine vc_mark_resolved(b, files, status)
        integer, intent(in) :: b
        type(vc_string_type), intent(in) :: files(:)
        type(status_type), intent(out) :: status
        type(vc_string_type), allocatable :: flist(:)
        integer :: k
        status = status_type()
        if (.not. associated(vc_backends(b)%mark_resolved)) then
            call vc_not_supported(status, "mark-resolved", b)
            return
        end if
        call with_properties_begin(files, flist)
        call vc_backends(b)%mark_resolved(files, status)
        if (.not. status%ok()) return
        ! FIXME: Is this TRTD?  Might not be.
        if (.not. vc_touched(VC_PROP_STATE)) then
            do k = 1, size(flist)
                call vc_file_set_state(flist(k)%s, VC_EDITED)
            end do
        end if
        ! Recompute mode lines.
        do k = 1, size(files)
            call vc_mode_line(files(k)%s, b)
        end do
        if (size(files) > 1) then
            call message("Conflicts have been resolved in "//itoa(size(files))//" files.  "// &
                         "Type C-x v v to check in changes.")
        else
            call message("Conflicts have been resolved in this file.  Type C-x v v to check in changes.")
        end if
    end subroutine vc_mark_resolved

    ! ------------------------------------------------------------------
    ! Diffs
    ! ------------------------------------------------------------------

    !> The end of a diff: say there were no changes (in a buffer the
    !> asynchronous process left empty), set up the Diff buffer type, go
    !> to the start and run `vc_diff_finish_functions'.  Port of
    !> vc.el:vc-diff-finish (as the action BUFFER, with S1 and S2 the
    !> messages and FLAG for having them).
    recursive subroutine vc_diff_finish(action, status)
        type(vc_action_type), intent(in) :: action
        type(status_type), intent(out) :: status
        type(handle_type) :: old, window
        type(status_type) :: st
        logical :: emptyp
        status = status_type()
        if (.not. buffer_live_p(action%buffer)) return
        old = current_buffer()
        call set_buffer(action%buffer, st)
        emptyp = point_max() == point_min()
        if (action%flag .and. emptyp) then
            call bind_read_only_insert(action%s2//"."//achar(10))
            call message(action%s2)
        end if
        call diff_setup_buffer_type()
        call goto_char(point_min())
        ! vc-diff-finish-functions: vc-shrink-buffer-window.
        window = get_buffer_window(action%buffer)
        if (window%index /= 0) call shrink_window_if_larger_than_buffer(window)
        call set_buffer(old, st)
        if (action%flag .and. .not. emptyp) call message(action%s1//"done")
    end subroutine vc_diff_finish

    !> Insert TEXT ignoring read-only (let ((inhibit-read-only t)) ...).
    subroutine bind_read_only_insert(text)
        use lisp_fm, only: t_value
        character(len=*), intent(in) :: text
        type(value_type) :: saved
        type(status_type) :: st
        call symbol_value(sym("inhibit_read_only"), saved, st)
        call set(sym("inhibit_read_only"), t_value(), st)
        call insert(text, st)
        call set(sym("inhibit_read_only"), saved, st)
    end subroutine bind_read_only_insert

    !> Report diffs between revisions REV1 and REV2 of a fileset in FS.
    !> ASYNC true means run the backend's commands asynchronously if possible.
    !> FS should have the format described in `vc_deduce_fileset'.
    !> Output goes to the buffer BUFFER, which defaults to *vc-diff*.
    !> CHANGES is true if the buffer had changes, false otherwise.  Port of
    !> vc.el:vc-diff-internal.
    recursive subroutine vc_diff_internal(async, fs, changes, status, rev1, rev2, verbose, buffer)
        logical, intent(in) :: async
        type(vc_fileset_type), intent(in) :: fs
        logical, intent(out) :: changes
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: rev1, rev2
        logical, intent(in), optional :: verbose
        type(handle_type), intent(in), optional :: buffer
        type(handle_type) :: buf, old, proc
        type(vc_action_type) :: finish
        type(vc_string_type), allocatable :: files(:), filtered(:), sw(:)
        type(status_type) :: st
        character(len=:), allocatable :: msg1, msg2, wrev
        logical :: verb, found
        integer :: k
        status = status_type()
        changes = .false.
        verb = .false.
        if (present(verbose)) verb = verbose
        if (present(buffer)) then
            buf = buffer
        else
            call get_buffer_create("*vc-diff*", buf, status)
            if (.not. status%ok()) return
        end if
        files = fs%files
        msg1 = "Finding changes in "//vc_delistify(files)//"..."
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
        ! (vc-setup-buffer leaves BUF current, so the backend's commands
        ! insert there without setting it up again.)
        call vc_setup_buffer(buf, status)
        if (.not. status%ok()) return
        call message(msg1)
        ! Many backends don't handle well the case of a file that has been
        ! added but not yet committed to the repo (notably CVS and Subversion).
        if (vc_diff_added_files) then
            allocate (filtered(0))
            do k = 1, size(files)
                call vc_working_revision(files(k)%s, wrev, found)
                if (file_directory_p(files(k)%s) .or. .not. (found .and. wrev == "0")) then
                    call push(filtered, files(k)%s)
                else if (present(rev1) .or. present(rev2)) then
                    ! This file is added but not yet committed; there is
                    ! no repository version to diff against.
                    call set_error(status, FM_GENERIC_ERROR, "No revisions of "//files(k)%s//" exist")
                    return
                else
                    ! We regard this as "changed".  Diff it against
                    ! /dev/null.
                    call vc_switches(0, "diff", sw)
                    call push(sw, "/dev/null")
                    if (async) then
                        call vc_do_command(VC_DEST_CURRENT, VC_OKSTATUS_ASYNC, "diff", [string_of(files(k)%s)], &
                                           sw, status, buffer=buf)
                    else
                        call vc_do_command(VC_DEST_CURRENT, 1, "diff", [string_of(files(k)%s)], sw, status, &
                                           buffer=buf)
                    end if
                    if (.not. status%ok()) return
                end if
            end do
            files = filtered
        end if
        call set_buffer(buf, st)
        ! Make the *vc-diff* buffer read only, the diff-mode key
        ! bindings are nicer for read only buffers. pcl-cvs does the
        ! same thing.
        buffers(cur)%read_only = .true.
        ! Set the major mode and some local variables before calling into
        ! the backend.
        call diff_mode(st)
        if (present(rev1) .and. present(rev2)) then
            call set_diff_vc_backend(vc_backend_name(fs%backend), rev1, rev2)
        else if (present(rev1)) then
            call set_diff_vc_backend(vc_backend_name(fs%backend), rev1=rev1)
        else if (present(rev2)) then
            call set_diff_vc_backend(vc_backend_name(fs%backend), rev2=rev2)
        else
            call set_diff_vc_backend(vc_backend_name(fs%backend))
        end if
        call remember_diff_args(async, fs, verb, rev1, rev2)
        call revert_buffer_function%set_local(vc_diff_revert)
        if (.not. associated(vc_backends(fs%backend)%diff)) then
            call vc_not_supported(status, "diff", fs%backend)
            return
        end if
        call vc_backends(fs%backend)%diff(files, buf, async, status, rev1, rev2)
        if (.not. status%ok()) return
        call set_buffer(buf, st)
        proc = get_buffer_process(buf)
        if (point_max() == point_min() .and. proc%index == 0) then
            ! Treat this case specially so as not to pop the buffer.
            call message(msg2)
            return
        end if
        ! Display the buffer, but at the end because it can change point.
        call pop_to_buffer(buf, status)
        if (.not. status%ok()) return
        ! The diff process may finish early, so call `vc-diff-finish'
        ! after `pop-to-buffer'; the former assumes the diff buffer is
        ! shown in some window.
        finish%run => vc_diff_finish
        finish%buffer = buf
        finish%flag = verb
        finish%s1 = msg1
        finish%s2 = msg2
        call vc_exec_after(finish, status)
        ! In the async case, we return t even if there are no differences
        ! because we don't know that yet.
        changes = .true.
    end subroutine vc_diff_internal

    !> Remember the arguments `vc_diff_revert' runs `vc_diff_internal' with
    !> in the current buffer.
    subroutine remember_diff_args(async, fs, verbose, rev1, rev2)
        logical, intent(in) :: async, verbose
        type(vc_fileset_type), intent(in) :: fs
        character(len=*), intent(in), optional :: rev1, rev2
        integer :: k
        k = local_index(.true.)
        locals(k)%has_diff = .true.
        locals(k)%diff_async = async
        locals(k)%diff_verbose = verbose
        locals(k)%diff_fileset = fs
        if (allocated(locals(k)%diff_rev1)) deallocate (locals(k)%diff_rev1)
        if (allocated(locals(k)%diff_rev2)) deallocate (locals(k)%diff_rev2)
        if (present(rev1)) locals(k)%diff_rev1 = rev1
        if (present(rev2)) locals(k)%diff_rev2 = rev2
    end subroutine remember_diff_args

    !> The `revert_buffer_function' of *vc-diff* buffers: make the diff
    !> again.
    recursive subroutine vc_diff_revert(ignore_auto, noconfirm, status)
        logical, intent(in) :: ignore_auto, noconfirm
        type(status_type), intent(out) :: status
        type(vc_locals_type) :: l
        type(handle_type) :: buf
        logical :: changes
        integer :: k
        status = status_type()
        if (ignore_auto .and. noconfirm .and. .false.) return
        k = local_index(.false.)
        if (k == 0) return
        l = locals(k)
        buf = current_buffer()
        if (allocated(l%diff_rev1) .and. allocated(l%diff_rev2)) then
            call vc_diff_internal(l%diff_async, l%diff_fileset, changes, status, l%diff_rev1, l%diff_rev2, &
                                  l%diff_verbose, buf)
        else if (allocated(l%diff_rev1)) then
            call vc_diff_internal(l%diff_async, l%diff_fileset, changes, status, rev1=l%diff_rev1, &
                                  verbose=l%diff_verbose, buffer=buf)
        else if (allocated(l%diff_rev2)) then
            call vc_diff_internal(l%diff_async, l%diff_fileset, changes, status, rev2=l%diff_rev2, &
                                  verbose=l%diff_verbose, buffer=buf)
        else
            call vc_diff_internal(l%diff_async, l%diff_fileset, changes, status, verbose=l%diff_verbose, buffer=buf)
        end if
    end subroutine vc_diff_revert

    !> Query the user for a revision using PROMPT.
    !> FILES may specify a file set to restrict the revisions to.  B is a VC
    !> backend.  DEFAULT and INITIAL are handled as defined by
    !> `completing_read'.  Port of vc.el:vc-read-revision (one revision).
    recursive subroutine vc_read_revision(prompt, rev, status, files, b, default, initial)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: rev
        type(status_type), intent(out) :: status
        type(vc_string_type), intent(in), optional :: files(:)
        integer, intent(in), optional :: b
        character(len=*), intent(in), optional :: default, initial
        type(vc_fileset_type) :: fs
        type(vc_string_type), allocatable :: fl(:), table(:)
        type(value_type), allocatable :: names(:)
        integer :: backend, k
        status = status_type()
        backend = 0
        if (present(b)) backend = b
        if (present(files)) then
            fl = files
            if (backend == 0 .and. size(fl) > 0) backend = vc_backend(fl(1)%s)
        else
            call vc_deduce_fileset(fs, status, .true.)
            if (.not. status%ok()) return
            fl = fs%files
            backend = fs%backend
        end if
        if (backend /= 0) then
            if (associated(vc_backends(backend)%revision_completion_table)) then
                call vc_backends(backend)%revision_completion_table(fl, table)
                allocate (names(size(table)))
                do k = 1, size(table)
                    names(k) = make_string(table(k)%s)
                end do
                call completing_read(prompt, completion_list_table(make_list(names)), rev, status, &
                                     initial=initial, history="vc_revision_history", default=default)
                return
            end if
        end if
        call read_string(prompt, rev, status, initial=initial, default=default)
    end subroutine vc_read_revision

    !> Read the revisions to compare for `vc_version_diff' from the
    !> minibuffer: REV1 (default: the previous revision of a single file)
    !> and REV2 (default: the current source), each "" for nil.  Port of
    !> vc.el:vc-diff-build-argument-list-internal.
    recursive subroutine vc_diff_build_argument_list_internal(fs, rev1, rev2, status)
        type(vc_fileset_type), intent(in) :: fs
        character(len=:), allocatable, intent(out) :: rev1, rev2
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: rev1_default, sym_rev, rev1_prompt
        logical :: found, have_default
        have_default = .false.
        rev1_default = ""
        ! someday we may be able to do revision completion on non-singleton
        ! filesets, but not yet.
        if (size(fs%files) == 1) then
            ! if the file is not locked, use previous revision and current
            ! source as defaults
            if (associated(vc_backends(fs%backend)%previous_revision)) then
                call vc_symbolic_working_revision(fs%files(1)%s, sym_rev, found, fs%backend)
                if (found) then
                    call vc_backends(fs%backend)%previous_revision(fs%files(1)%s, sym_rev, rev1_default, have_default)
                    if (have_default .and. len(rev1_default) == 0) have_default = .false.
                end if
            end if
        end if
        ! construct argument list
        if (have_default) then
            rev1_prompt = "Older revision (default "//rev1_default//"): "
            call vc_read_revision(rev1_prompt, rev1, status, fs%files, fs%backend, default=rev1_default)
        else
            call vc_read_revision("Older revision: ", rev1, status, fs%files, fs%backend)
        end if
        if (.not. status%ok()) return
        call vc_read_revision("Newer revision (default current source): ", rev2, status, fs%files, fs%backend)
    end subroutine vc_diff_build_argument_list_internal

    !> Report diffs between revisions REV1 and REV2 in the repository history.
    !> This compares two revisions of the current fileset.
    !> If REV1 is "", it defaults to the previous revision, i.e. revision
    !> before the last commit.
    !> If REV2 is "", it defaults to the work tree, i.e. the current
    !> state of each file in the fileset.  Port of vc.el:vc-version-diff.
    recursive subroutine vc_version_diff(rev1, rev2, status, interactive)
        character(len=*), intent(in) :: rev1, rev2
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: interactive
        type(vc_fileset_type) :: fs
        logical :: changes
        status = status_type()
        if (len(rev1) == 0 .and. len(rev2) > 0) then
            call set_error(status, FM_GENERIC_ERROR, "Not a valid revision range")
            return
        end if
        call vc_deduce_fileset(fs, status, .true.)
        if (.not. status%ok()) return
        call diff_revs(vc_allow_async_diff, fs, rev1, rev2, opt(interactive), changes, status)
    end subroutine vc_version_diff

    !> `vc_diff_internal' with REV1 and REV2 ("" for nil).
    recursive subroutine diff_revs(async, fs, rev1, rev2, verbose, changes, status, buffer)
        logical, intent(in) :: async, verbose
        type(vc_fileset_type), intent(in) :: fs
        character(len=*), intent(in) :: rev1, rev2
        logical, intent(out) :: changes
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: buffer
        if (len(rev1) > 0 .and. len(rev2) > 0) then
            call vc_diff_internal(async, fs, changes, status, rev1, rev2, verbose, buffer)
        else if (len(rev1) > 0) then
            call vc_diff_internal(async, fs, changes, status, rev1=rev1, verbose=verbose, buffer=buffer)
        else if (len(rev2) > 0) then
            call vc_diff_internal(async, fs, changes, status, rev2=rev2, verbose=verbose, buffer=buffer)
        else
            call vc_diff_internal(async, fs, changes, status, verbose=verbose, buffer=buffer)
        end if
    end subroutine diff_revs

    logical function opt(flag)
        logical, intent(in), optional :: flag
        opt = .false.
        if (present(flag)) opt = flag
    end function opt

    !> The fileset of the root directory of the current VC tree: the
    !> backend deduced, or the one responsible for a directory read with
    !> DESC (vc--with-backend-in-rootdir), and that directory.  Sets
    !> `default_directory' to the root, as the macro binds it (the caller
    !> restores SAVED_DIR).
    recursive subroutine with_backend_in_rootdir(desc, fs, saved_dir, status)
        character(len=*), intent(in) :: desc
        type(vc_fileset_type), intent(out) :: fs
        character(len=:), allocatable, intent(out) :: saved_dir
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: rootdir
        logical :: found
        saved_dir = default_directory()
        fs%backend = vc_deduce_backend()
        if (fs%backend /= 0) then
            call vc_call_root(fs%backend, default_directory(), rootdir, found, status)
            if (.not. status%ok()) return
        else
            call read_file_name("Directory for "//desc//": ", rootdir, status, default_filename=default_directory(), &
                                mustmatch=REQUIRE_MATCH_NIL)
            if (.not. status%ok()) return
            rootdir = file_name_as_directory(expand(rootdir))
            fs%backend = vc_responsible_backend(rootdir, status)
            if (.not. status%ok() .or. fs%backend == 0) then
                call set_error(status, FM_GENERIC_ERROR, "Directory is not version controlled")
                return
            end if
        end if
        buffers(cur)%directory = rootdir
        call one(fs%files, rootdir)
    end subroutine with_backend_in_rootdir

    !> Report diffs between REV1 and REV2 revisions of the whole tree.
    !> Port of vc.el:vc-root-version-diff.
    recursive subroutine vc_root_version_diff(rev1, rev2, status, interactive)
        character(len=*), intent(in) :: rev1, rev2
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: interactive
        type(vc_fileset_type) :: fs
        type(handle_type) :: old
        type(status_type) :: st
        character(len=:), allocatable :: saved
        logical :: changes
        status = status_type()
        ! This is a mix of `vc-root-diff' and `vc-version-diff'
        if (len(rev1) == 0 .and. len(rev2) > 0) then
            call set_error(status, FM_GENERIC_ERROR, "Not a valid revision range")
            return
        end if
        old = current_buffer()
        call with_backend_in_rootdir("VC root-diff", fs, saved, status)
        if (status%ok()) call diff_revs(vc_allow_async_diff, fs, rev1, rev2, opt(interactive), changes, status)
        if (buffer_live_p(old)) then
            call set_buffer(old, st)
            buffers(cur)%directory = saved
        end if
    end subroutine vc_root_version_diff

    !> Display diffs between file revisions.
    !> Normally this compares the currently selected fileset with their
    !> working revisions.  With a prefix argument HISTORIC, it reads two revision
    !> designators specifying which revisions to compare.
    !>
    !> Optional argument NOT_ESSENTIAL true means it is okay to say no to
    !> saving the buffer.
    !> Optional argument FILESET, if present, overrides the fileset.  Port
    !> of vc.el:vc-diff.
    recursive subroutine vc_diff(status, historic, not_essential, fileset, interactive)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: historic, not_essential, interactive
        type(vc_fileset_type), intent(in), optional :: fileset
        type(vc_fileset_type) :: fs
        character(len=:), allocatable :: rev1, rev2
        logical :: changes
        status = status_type()
        if (opt(historic)) then
            call vc_deduce_fileset(fs, status, .true.)
            if (.not. status%ok()) return
            call vc_diff_build_argument_list_internal(fs, rev1, rev2, status)
            if (.not. status%ok()) return
            call vc_version_diff(rev1, rev2, status, .true.)
            return
        end if
        if (present(fileset)) then
            fs = fileset
        else
            call vc_deduce_fileset(fs, status, .true.)
            if (.not. status%ok()) return
        end if
        call vc_buffer_sync_fileset(fs, status, not_essential)
        if (.not. status%ok()) return
        call vc_diff_internal(vc_allow_async_diff, fs, changes, status, verbose=opt(interactive))
    end subroutine vc_diff

    !> Call `vc_buffer_sync' for most buffers visiting files in FILESET.
    !> NOT_ESSENTIAL means it is okay to continue if the user says not to save.
    !>
    !> For files named explicitly in FILESET, this function always syncs their
    !> buffers.  By contrast, for directories named in FILESET, its behavior
    !> depends on MISSING_IN_DIRS.  For each directory named in FILESET, it
    !> considers buffers visiting any file contained within that directory or
    !> its subdirectories.  If MISSING_IN_DIRS is false, it syncs only those
    !> buffers whose files exist on disk.  Otherwise it syncs all of them.
    !> Port of vc.el:vc-buffer-sync-fileset.
    recursive subroutine vc_buffer_sync_fileset(fs, status, not_essential, missing_in_dirs)
        type(vc_fileset_type), intent(in) :: fs
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: not_essential, missing_in_dirs
        type(vc_string_type), allocatable :: dirs(:)
        type(handle_type), allocatable :: bufs(:), all(:)
        type(handle_type) :: buf, old
        type(status_type) :: st
        character(len=:), allocatable :: file
        integer :: k, j
        logical :: in_dir
        status = status_type()
        allocate (dirs(0), bufs(0))
        do k = 1, size(fs%files)
            if (file_directory_p(fs%files(k)%s)) then
                call push(dirs, file_name_as_directory(fs%files(k)%s))
            else
                buf = get_file_buffer(fs%files(k)%s)
                if (buf%index /= 0) bufs = [bufs, buf]
            end if
        end do
        if (size(dirs) > 0) then
            all = buffer_list()
            do k = 1, size(all)
                if (.not. allocated(buffers(all(k)%index)%filename)) cycle
                file = buffers(all(k)%index)%filename
                in_dir = .false.
                do j = 1, size(dirs)
                    if (opt(not_essential)) then
                        ! For speed (bug#79137).
                        if (index(file, dirs(j)%s) == 1) in_dir = .true.
                    else
                        if (file_in_directory_p(file, dirs(j)%s)) in_dir = .true.
                    end if
                end do
                if (.not. in_dir) cycle
                if (.not. (opt(missing_in_dirs) .or. file_exists_p(file))) cycle
                if (any(bufs%index == all(k)%index)) cycle
                bufs = [bufs, all(k)]
            end do
        end if
        old = current_buffer()
        do k = 1, size(bufs)
            call set_buffer(bufs(k), st)
            call vc_buffer_sync(status, not_essential)
            if (.not. status%ok()) exit
        end do
        if (buffer_live_p(old)) call set_buffer(old, st)
    end subroutine vc_buffer_sync_fileset

    !> Display diffs between VC-controlled whole tree revisions.
    !> Normally, this compares the tree corresponding to the current
    !> fileset with the working revision.
    !> With a prefix argument HISTORIC, prompt for two revision
    !> designators specifying which revisions to compare.
    !>
    !> Optional argument NOT_ESSENTIAL true means it is okay to say no to
    !> saving the buffer.  Port of vc.el:vc-root-diff.
    recursive subroutine vc_root_diff(historic, status, not_essential, interactive)
        logical, intent(in) :: historic
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: not_essential, interactive
        type(vc_fileset_type) :: fs, args
        type(handle_type) :: old
        type(status_type) :: st
        character(len=:), allocatable :: saved, rev1, rev2, root
        logical :: changes, found
        status = status_type()
        if (historic) then
            ! We want the diff for the VC root dir.
            args%backend = vc_deduce_backend()
            if (args%backend == 0) then
                args%backend = vc_responsible_backend(default_directory(), status)
                if (.not. status%ok()) return
            end if
            call vc_call_root(args%backend, default_directory(), root, found, status)
            if (.not. status%ok()) return
            call one(args%files, root)
            call vc_diff_build_argument_list_internal(args, rev1, rev2, status)
            if (.not. status%ok()) return
            call vc_root_version_diff(rev1, rev2, status, .true.)
            return
        end if
        old = current_buffer()
        ! VC diff for the root directory produces output that is relative
        ! to it.  Bind default-directory to the root directory here, this
        ! way the *vc-diff* buffer is setup correctly, so relative file
        ! names work.
        call with_backend_in_rootdir("VC root-diff", fs, saved, status)
        if (status%ok()) then
            call vc_buffer_sync_fileset(fs, status, not_essential)
            if (status%ok()) call vc_diff_internal(vc_allow_async_diff, fs, changes, status, verbose=opt(interactive))
        end if
        if (buffer_live_p(old)) then
            call set_buffer(old, st)
            buffers(cur)%directory = saved
        end if
    end subroutine vc_root_diff

    ! ------------------------------------------------------------------
    ! Other revisions
    ! ------------------------------------------------------------------

    !> Visit revision REV of the current file in another window.
    !> If the current file is named `F', the revision is named `F.~REV~'.
    !> If `F.~REV~' already exists, use it instead of checking it out again.
    !>
    !> If this command needs to split the current window, it by default obeys
    !> the user options `split_height_threshold' and `split_width_threshold',
    !> when it decides whether to split the window horizontally or vertically.
    !> Port of vc.el:vc-revision-other-window.
    recursive subroutine vc_revision_other_window(rev, status)
        character(len=*), intent(in) :: rev
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file, revision
        type(handle_type) :: buf
        logical :: found
        call vc_ensure_vc_buffer(status)
        if (.not. status%ok()) return
        file = buffers(cur)%filename
        if (len(rev) == 0) then
            call vc_symbolic_working_revision(file, revision, found)
        else
            revision = rev
        end if
        call vc_find_revision(file, revision, buf, status)
        if (.not. status%ok()) return
        call switch_to_buffer_other_window(buf, status)
    end subroutine vc_revision_other_window

    !> Read REVISION of FILE into a buffer and give the buffer as RESULT.
    !> Use B as the VC backend if specified.  Port of vc.el:vc-find-revision
    !> with vc-find-revision-save (saves the buffer to the file).
    recursive subroutine vc_find_revision(file, revision, result, status, b)
        character(len=*), intent(in) :: file, revision
        type(handle_type), intent(out) :: result
        type(status_type), intent(out) :: status
        integer, intent(in), optional :: b
        character(len=:), allocatable :: automatic_backup, filename, msg
        type(handle_type) :: filebuf, old, outbuf
        type(status_type) :: st
        integer :: backend, k
        result = null_handle()
        status = status_type()
        if (vc_find_revision_no_save) then
            call set_error(status, FM_GENERIC_ERROR, "vc_find_revision_no_save is not ported yet")
            return
        end if
        automatic_backup = vc_version_backup_file_name(file, revision)
        filebuf = get_file_buffer(file)
        if (filebuf%index == 0) filebuf = current_buffer()
        filename = vc_version_backup_file_name(file, revision, manual=.true.)
        backend = 0
        if (present(b)) backend = b
        if (backend == 0) backend = vc_backend(file)
        if (.not. file_exists_p(filename)) then
            if (file_exists_p(automatic_backup)) then
                call rename_file(automatic_backup, filename, status)
                if (.not. status%ok()) return
            else
                call message("Checking out "//filename//"...")
                old = current_buffer()
                call set_buffer(filebuf, st)
                ! with-temp-file FILENAME
                call get_buffer_create(generate_new_buffer_name(" *temp file*"), outbuf, status)
                if (.not. status%ok()) return
                if (.not. associated(vc_backends(backend)%find_revision)) then
                    call vc_not_supported(status, "find-revision", backend)
                else
                    ! Change buffer to get local value of
                    ! vc-checkout-switches.
                    call vc_backends(backend)%find_revision(file, revision, outbuf, status)
                end if
                if (status%ok()) then
                    call set_buffer(outbuf, st)
                    call write_region(filename, .false., status, point_min(), point_max(), msg)
                end if
                call kill_buffer(outbuf, st)
                if (.not. status%ok()) then
                    if (file_exists_p(filename)) call delete_file(filename, st)
                    call set_buffer(old, st)
                    return
                end if
                call set_buffer(filebuf, st)
                call vc_mode_line(file)
                call set_buffer(old, st)
                call message("Checking out "//filename//"...done")
            end if
        end if
        call find_file_noselect(filename, result, status)
        if (.not. status%ok()) return
        old = current_buffer()
        call set_buffer(result, st)
        call set_vc_parent_buffer(filebuf, "")
        k = local_index(.true.)
        locals(k)%has_override = .true.
        locals(k)%override%backend = backend
        call one(locals(k)%override%files, expand(file))
        locals(k)%revision = revision
        call set_buffer(old, st)
    end subroutine vc_find_revision

    ! ------------------------------------------------------------------
    ! Reverting
    ! ------------------------------------------------------------------

    !> Revert working copies of the selected fileset to their repository contents.
    !> This asks for confirmation if the buffer contents are not identical
    !> to the working revision (except for keyword expansion).  Port of
    !> vc.el:vc-revert.
    recursive subroutine vc_revert(status)
        type(status_type), intent(out) :: status
        type(vc_fileset_type) :: fs
        type(handle_type) :: diff_buffer, buf
        type(status_type) :: st
        character(len=:), allocatable :: names, str
        logical :: queried, yes, changes, show
        integer :: k
        status = status_type()
        call vc_deduce_fileset(fs, status)
        if (.not. status%ok()) return
        queried = .false.
        diff_buffer = null_handle()
        ! If any of the files is visited by the current buffer, make sure
        ! buffer is saved.  If the user says `no', abort since we cannot
        ! show the changes and ask for confirmation to discard them.
        if (len(buffer_file_name()) > 0) then
            if (size(fs%files) == 0 .or. member(buffer_file_name(), fs%files)) then
                call vc_buffer_sync(status)
                if (.not. status%ok()) return
            end if
        end if
        call save_some_buffers(.false., .false., status, in_fileset)
        if (.not. status%ok()) return
        names = ""
        do k = 1, size(fs%files)
            buf = get_file_buffer(fs%files(k)%s)
            if (buffer_modified_p(buf)) then
                if (len(names) > 0) then
                    names = buffers(buf%index)%name//", "//names
                else
                    names = buffers(buf%index)%name
                end if
            end if
            if (vc_up_to_date_p(fs%files(k)%s)) then
                call yes_or_no_p(fs%files(k)%s//" seems up-to-date.  Revert anyway? ", yes, status)
                if (.not. status%ok()) return
                if (.not. yes) then
                    call set_error(status, FM_GENERIC_ERROR, "Revert canceled")
                    return
                end if
                queried = .true.
            end if
        end do
        if (len(names) > 0) then
            call set_error(status, FM_GENERIC_ERROR, "Cannot revert with these buffers unsaved: "//names)
            return
        end if
        if (vc_revert_show_diff /= VC_REVERT_SHOW_DIFF_NIL) then
            call get_buffer_create(generate_new_buffer_name("*vc-diff*"), diff_buffer, status)
            if (.not. status%ok()) return
            call vc_diff_internal(vc_allow_async_revert, fs, changes, status, buffer=diff_buffer)
            show = changes
        else
            ! Avoid querying the user again.
            show = .not. queried
        end if
        if (status%ok() .and. show) then
            str = vc_delistify(fs%files)
            if (len(str) >= 50) then
                if (size(fs%files) == 1) then
                    str = "1 file"
                else
                    str = itoa(size(fs%files))//" files"
                end if
            end if
            call yes_or_no_p("Discard changes in "//str//"? ", yes, status)
            if (status%ok() .and. .not. yes) call set_error(status, FM_GENERIC_ERROR, "Revert cancelled")
        end if
        if (diff_buffer%index /= 0) then
            if (buffer_live_p(diff_buffer)) call quit_windows_on(diff_buffer, vc_revert_show_diff == VC_REVERT_SHOW_DIFF_KILL, &
                                                                 st)
        end if
        if (.not. status%ok()) return
        call vc_revert_files(fs%backend, fs%files, status)
    contains

        !> save-some-buffers's PRED: the buffer visits one of the files.
        logical function in_fileset()
            in_fileset = .false.
            if (len(buffer_file_name()) > 0) in_fileset = member(buffer_file_name(), fs%files)
        end function in_fileset
    end subroutine vc_revert

    !> Return name of backup file for revision REV of FILE.
    !> If version backups should be used for FILE, and there exists
    !> such a backup for REV or the working revision of file, give
    !> its name as BACKUP; otherwise FOUND is false.  Port of
    !> vc.el:vc-version-backup-file.
    subroutine vc_version_backup_file(file, backup, found, rev)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: backup
        logical, intent(out) :: found
        character(len=*), intent(in), optional :: rev
        integer :: b
        backup = ""
        found = .false.
        b = vc_backend(file)
        if (b == 0) return
        if (.not. vc_call_make_version_backups_p(b, file)) return
        backup = vc_version_backup_file_name(file, rev)
        if (file_exists_p(backup)) then
            found = .true.
            return
        end if
        ! there is no automatic backup, but maybe the user made one manually
        backup = vc_version_backup_file_name(file, rev, manual=.true.)
        found = file_exists_p(backup)
    end subroutine vc_version_backup_file

    !> Revert FILE back to the repository working revision it was based on.
    !> If FILE is a directory, revert all files inside that directory.  Port
    !> of vc.el:vc-revert-file.
    recursive subroutine vc_revert_file(file, status)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        type(vc_string_type), allocatable :: flist(:)
        type(status_type) :: st
        character(len=:), allocatable :: backup_file, root
        logical :: dir, found
        integer :: b, state, k
        status = status_type()
        call with_properties_begin([string_of(file)], flist)
        dir = file_directory_p(file)
        found = .false.
        if (.not. dir) call vc_version_backup_file(file, backup_file, found)
        if (found) then
            call copy_file(backup_file, file, status, ok_if_already_exists=.true.)
            if (.not. status%ok()) return
            call vc_delete_automatic_version_backups(file)
        end if
        if (dir) then
            b = vc_responsible_backend(file, status)
            if (.not. status%ok()) return
        else
            b = vc_backend(file)
        end if
        if (b == 0) then
            call set_error(status, FM_GENERIC_ERROR, "File "//file//" is not under version control")
            return
        end if
        if (.not. associated(vc_backends(b)%revert)) then
            call vc_not_supported(status, "revert", b)
            return
        end if
        if (found) then
            call vc_backends(b)%revert(file, status, backup_file)
        else
            call vc_backends(b)%revert(file, status)
        end if
        if (.not. status%ok()) return
        state = vc_state(file)
        do k = 1, size(flist)
            call set_after_revert(flist(k)%s, state)
        end do
        call vc_root_dir(root, found, st)
        call vc_resynch_buffer(file, root, status, keep=.true., noquery=VC_NOQUERY_T)
    end subroutine vc_revert_file

    !> Set FILE's properties after reverting it from STATE, those the
    !> backend did not set (with-vc-properties's settings): an added file
    !> becomes unregistered, others up to date; the checkout time is the
    !> modification time.
    subroutine set_after_revert(file, state)
        character(len=*), intent(in) :: file
        integer, intent(in) :: state
        type(file_stat_type) :: attr
        logical :: found
        if (state == VC_ADDED .and. .not. vc_touched(VC_PROP_BACKEND)) call vc_file_set_backend(file, 0)
        if (.not. vc_touched(VC_PROP_STATE)) then
            if (state == VC_ADDED) then
                call vc_file_set_state(file, VC_UNREGISTERED)
            else if (state /= VC_STATE_NIL) then
                call vc_file_set_state(file, VC_UP_TO_DATE)
            end if
        end if
        if (.not. vc_touched(VC_PROP_CHECKOUT_TIME)) then
            call file_attributes(file, attr, found)
            if (found) then
                call vc_file_set_checkout_time(file, attr%mtime_sec, attr%mtime_nsec)
            else
                call vc_file_set_checkout_time(file)
            end if
        end if
    end subroutine set_after_revert

    !> Revert each of FILES to the repository working version it was based on.
    !> For entries in FILES that are directories, revert all files inside them.
    !> Port of vc.el:vc-revert-files.
    recursive subroutine vc_revert_files(b, files, status)
        integer, intent(in) :: b
        type(vc_string_type), intent(in) :: files(:)
        type(status_type), intent(out) :: status
        type(vc_string_type), allocatable :: flist(:)
        integer, allocatable :: states(:)
        type(status_type) :: st
        character(len=:), allocatable :: root
        logical :: found
        integer :: k
        status = status_type()
        if (size(files) == 0) return
        call message("Reverting "//vc_delistify(files)//"...")
        if (.not. associated(vc_backends(b)%revert_files)) then
            do k = 1, size(files)
                call vc_revert_file(files(k)%s, status)
                if (.not. status%ok()) return
            end do
        else
            call with_properties_begin(files, flist)
            ! Use `vc-file-getprop' directly here because we may be
            ! handling very many files and do not want to hit the disk.
            allocate (states(size(flist)))
            do k = 1, size(flist)
                states(k) = vc_file_get_state(flist(k)%s)
            end do
            call vc_backends(b)%revert_files(files, status)
            if (.not. status%ok()) return
            do k = 1, size(flist)
                call set_after_revert(flist(k)%s, states(k))
            end do
            call vc_root_dir(root, found, st)
            do k = 1, size(files)
                call vc_resynch_buffer(files(k)%s, root, status, keep=.true., noquery=VC_NOQUERY_T)
                if (.not. status%ok()) return
            end do
        end if
        call message("Reverting "//vc_delistify(files)//"...done")
    end subroutine vc_revert_files

    ! ------------------------------------------------------------------
    ! Deleting and renaming
    ! ------------------------------------------------------------------

    !> Delete file and mark it as such in the version control system.
    !> If called interactively, read FILES, defaulting to the current
    !> buffer's file name if it's under version control.
    !> NOCONFIRM true means don't
    !> prompt to confirm deletion.
    !> For some VCS, FILES can include directories.
    !> These are recursively deleted.  Port of vc.el:vc-delete-file.
    recursive subroutine vc_delete_file(files_in, status, noconfirm)
        type(vc_string_type), intent(in) :: files_in(:)
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: noconfirm
        type(vc_string_type), allocatable :: files(:), group(:)
        type(handle_type) :: buf, old
        type(status_type) :: st
        character(len=:), allocatable :: saved, root
        logical :: yes, found
        integer, allocatable :: backends(:)
        integer :: k, j, b, state
        status = status_type()
        allocate (files(size(files_in)), backends(size(files_in)))
        do k = 1, size(files_in)
            files(k)%s = expand(files_in(k)%s)
        end do
        do k = 1, size(files)
            buf = get_file_buffer(files(k)%s)
            if (file_directory_p(files(k)%s)) then
                b = vc_responsible_backend(files(k)%s, status)
                if (.not. status%ok()) return
            else
                b = vc_backend(files(k)%s)
            end if
            backends(k) = b
            if (b /= 0) then
                if (.not. associated(vc_backends(b)%delete_file)) then
                    call set_error(status, FM_GENERIC_ERROR, "Deleting files under "//vc_backend_name(b)// &
                                   " is not supported in VC")
                    return
                end if
            end if
            if (buffer_modified_p(buf)) then
                call set_error(status, FM_GENERIC_ERROR, "Please save or undo your changes before deleting "// &
                               files(k)%s)
                return
            end if
            state = vc_state(files(k)%s)
            if (state == VC_EDITED) then
                call set_error(status, FM_GENERIC_ERROR, "Please commit or undo your changes before deleting "// &
                               files(k)%s)
                return
            end if
            if (state == VC_CONFLICT) then
                call set_error(status, FM_GENERIC_ERROR, "Please resolve the conflicts before deleting "//files(k)%s)
                return
            end if
        end do
        if (.not. opt(noconfirm)) then
            if (size(files) > 1) then
                call y_or_n_p("Really want to delete these "//itoa(size(files))//" files? ", yes, status)
            else
                call y_or_n_p("Really want to delete "//file_name_nondirectory(files(1)%s)//"? ", yes, status)
            end if
            if (.not. status%ok()) return
            if (.not. yes) then
                call set_error(status, FM_GENERIC_ERROR, "Abort!")
                return
            end if
        end if
        do k = 1, size(files)
            buf = get_file_buffer(files(k)%s)
            if (.not. (file_directory_p(files(k)%s) .or. .not. make_backup_files .or. &
                       backup_file_name_p(files(k)%s) .or. .not. file_exists_p(files(k)%s))) then
                old = current_buffer()
                if (buf%index == 0) then
                    call find_file_noselect(files(k)%s, buf, status)
                    if (.not. status%ok()) return
                end if
                call set_buffer(buf, st)
                block
                    logical :: saved_inhibited
                    integer(int32) :: modes
                    saved_inhibited = buffers(cur)%backup_inhibited
                    buffers(cur)%backup_inhibited = .false.
                    call backup_buffer(modes)
                    buffers(cur)%backup_inhibited = saved_inhibited
                end block
                call set_buffer(old, st)
            end if
            if (backends(k) == 0) then
                call post_backend_deletion(files(k)%s)
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
                    call push(group, files(j)%s)
                    backends(j) = -1
                end if
            end do
            if (associated(vc_backends(b)%delete_files)) then
                saved = default_directory()
                call vc_root_dir(root, found, st, b)
                if (found) buffers(cur)%directory = root
                call vc_backends(b)%delete_files(group, status)
                buffers(cur)%directory = saved
                if (.not. status%ok()) return
                do j = 1, size(group)
                    call post_backend_deletion(group(j)%s)
                end do
            else
                do j = 1, size(group)
                    ! Changing default directory like this has the problem
                    ! that the containing directory for FILE may no longer
                    ! exists if all files within it are already `missing',
                    ! say.
                    saved = default_directory()
                    buffers(cur)%directory = file_name_directory(group(j)%s)
                    call vc_backends(b)%delete_file(group(j)%s, status)
                    buffers(cur)%directory = saved
                    if (.not. status%ok()) return
                    call post_backend_deletion(group(j)%s)
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
            type(file_stat_type) :: attr
            logical :: exists
            ! For the case of unregistered files, or if the backend didn't
            ! actually delete the file.
            call file_attributes(file, attr, exists, nofollow=.true.)
            if (exists) then
                if (file_directory_p(file) .and. .not. file_symlink_p(file)) then
                    call delete_directory(file, st, .true.)
                else
                    call delete_file(file, st)
                end if
            end if
            ! Forget what VC knew about the file.
            call vc_file_clearprops(file)
            ! Make sure the buffer is killed and *vc-dir* buffers are
            ! updated after this.
            call vc_root_dir(root, found, st)
            call vc_resynch_buffer(file, root, st, keep=.false., noquery=VC_NOQUERY_T)
        end subroutine post_backend_deletion
    end subroutine vc_delete_file

    !> Rename file OLD to NEW in both working tree and repository.
    !> When called interactively, read OLD and NEW, defaulting OLD to the
    !> current buffer's file name if it's under version control.
    !> If NEW is a directory name, rename FILE to a like-named file under NEW.
    !> For NEW to be recognized as a directory name, it should end in a slash.
    !> Signal a `file_already_exists' error if a file NEW already exists unless
    !> OK_IF_ALREADY_EXISTS is true.  Port of vc.el:vc-rename-file.
    recursive subroutine vc_rename_file(old_in, new_in, status, ok_if_already_exists)
        character(len=*), intent(in) :: old_in, new_in
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: ok_if_already_exists
        character(len=:), allocatable :: old, new, old_base, saved
        type(handle_type) :: oldbuf, newbuf, cbuf
        type(status_type) :: st
        type(vc_string_type), allocatable :: one_file(:)
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
        buffers(cur)%directory = file_name_directory(old)
        dirp = file_directory_p(old)
        body: block
            if (buffer_modified_p(oldbuf)) then
                call set_error(status, FM_GENERIC_ERROR, "Please save files before moving them")
                exit body
            end if
            newbuf = get_file_buffer(new)
            if (newbuf%index /= 0) then
                call set_error(status, FM_GENERIC_ERROR, "Already editing new file name")
                exit body
            end if
            ! Handle OK-IF-ALREADY-EXISTS here because it's not part of the VC
            ! backend `delete-file' API.
            if (file_exists_p(new)) then
                if (opt(ok_if_already_exists)) then
                    call one(one_file, new)
                    call vc_delete_file(one_file, status, .true.)
                    if (.not. status%ok()) exit body
                else
                    call set_error(status, FM_FILE_ALREADY_EXISTS, "File exists, "//new)
                    exit body
                end if
            end if
            if (.not. dirp) then
                state = vc_state(old)
                if (.not. (state == VC_UP_TO_DATE .or. state == VC_EDITED .or. state == VC_ADDED)) then
                    if (state == VC_LOCKED) then
                        call set_error(status, FM_GENERIC_ERROR, "Please check in files before moving them")
                    else
                        call set_error(status, FM_GENERIC_ERROR, "Please update files before moving them")
                    end if
                    exit body
                end if
            end if
            ! The rename commands for several VCS (at least Bzr, Git and
            ! Mercurial) will fail if asked to move a directory containing
            ! only untracked files.  So skip calling into the backend if OLD
            ! is a directory and we walk through the entirety of it without
            ! finding any VC-managed files.
            any_registered = .true.
            if (dirp) any_registered = vc_file_tree_walk_any(old)
            if (any_registered) then
                if (dirp) then
                    b = vc_responsible_backend(old, status)
                    if (.not. status%ok()) exit body
                else
                    b = vc_backend(old)
                end if
                if (b == 0 .or. .not. associated(vc_backends(b)%rename_file)) then
                    call vc_not_supported(status, "rename-file", b)
                    exit body
                end if
                call vc_backends(b)%rename_file(old, new, status)
                if (.not. status%ok()) exit body
            end if
            call vc_file_clearprops(old)
            call vc_file_clearprops(new)
            ! Move the actual file (unless the backend did it already)
            if (file_exists_p(old)) then
                call rename_file(old, new, status)
                if (.not. status%ok()) exit body
            end if
            ! ?? Renaming a file might change its contents due to keyword
            ! expansion.
            if (oldbuf%index /= 0) then
                cbuf = current_buffer()
                buffers(cbuf%index)%directory = saved
                call set_buffer(oldbuf, st)
                saved_ro = buffers(cur)%read_only
                call set_visited_file_name(new, status)
                buffers(cur)%read_only = saved_ro
                call vc_mode_line(new, vc_backend(new))
                call restore_buffer_modified_p(.false.)
                call set_buffer(cbuf, st)
                return
            end if
        end block body
        if (cur /= 0) buffers(cur)%directory = saved
    end subroutine vc_rename_file

    !> Whether DIRNAME contains a VC-managed file (vc-file-tree-walk
    !> throwing `done' at the first).
    recursive logical function vc_file_tree_walk_any(dirname) result(found)
        character(len=*), intent(in) :: dirname
        type(vc_string_type), allocatable :: files(:)
        call vc_file_tree_walk(dirname, files, first_only=.true.)
        found = size(files) > 0
    end function vc_file_tree_walk_any

    !> Walk recursively through DIRNAME.
    !> Give the VC-managed files underneath it as FILES (only the first
    !> with FIRST_ONLY).  Port of vc.el:vc-file-tree-walk and
    !> vc-file-tree-walk-internal (FUNC collecting the files).
    recursive subroutine vc_file_tree_walk(dirname, files, first_only)
        character(len=*), intent(in) :: dirname
        type(vc_string_type), allocatable, intent(out) :: files(:)
        logical, intent(in), optional :: first_only
        allocate (files(0))
        call walk(expand(dirname))
    contains

        recursive subroutine walk(file)
            use lisp_fm, only: list_elements, xstring
            character(len=*), intent(in) :: file
            type(value_type) :: names
            type(value_type), allocatable :: elts(:)
            type(status_type) :: st
            character(len=:), allocatable :: dir, f, dirf
            integer :: k, j
            logical :: excluded
            if (opt(first_only) .and. size(files) > 0) return
            if (.not. file_directory_p(file)) then
                if (vc_registered(file)) call push(files, file)
                return
            end if
            dir = file_name_as_directory(file)
            call directory_files(dir, names, st)
            if (.not. st%ok()) return
            call list_elements(names, elts)
            do k = 1, size(elts)
                f = xstring(elts(k))
                if (f == "." .or. f == "..") cycle
                excluded = .false.
                do j = 1, size(vc_directory_exclusion_list)
                    if (trim(vc_directory_exclusion_list(j)) == f) excluded = .true.
                end do
                if (excluded) cycle
                dirf = expand(f, dir)
                ! Avoid possible loops.
                if (file_symlink_p(dirf)) cycle
                call walk(dirf)
            end do
        end subroutine walk
    end subroutine vc_file_tree_walk

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    recursive subroutine cmd_vc_next_action(status)
        type(status_type), intent(out) :: status
        call vc_next_action(.not. nilp(current_prefix_arg), status)
    end subroutine cmd_vc_next_action

    recursive subroutine cmd_vc_register(status)
        type(status_type), intent(out) :: status
        call vc_register(status)
    end subroutine cmd_vc_register

    recursive subroutine cmd_vc_create_repo(status)
        type(status_type), intent(out) :: status
        integer :: b
        b = vc_read_backend("Create repository for: ", status)
        if (.not. status%ok()) return
        call vc_create_repo(b, status)
    end subroutine cmd_vc_create_repo

    recursive subroutine cmd_vc_ignore(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: rel_dir, file
        rel_dir = vc_ignore_base_dir(status)
        if (.not. status%ok()) return
        call read_file_name("File to ignore: ", file, status)
        if (.not. status%ok()) return
        if (file_name_absolute_p(file)) then
            if (file_in_directory_p(file, rel_dir)) file = file_relative_name(file, rel_dir)
        end if
        call vc_ignore(file, status, rel_dir,.not. nilp(current_prefix_arg))
    end subroutine cmd_vc_ignore

    recursive subroutine cmd_vc_diff(status)
        type(status_type), intent(out) :: status
        call vc_diff(status,.not. nilp(current_prefix_arg), .true., interactive=.true.)
    end subroutine cmd_vc_diff

    recursive subroutine cmd_vc_version_diff(status)
        type(status_type), intent(out) :: status
        type(vc_fileset_type) :: fs
        character(len=:), allocatable :: rev1, rev2
        call vc_deduce_fileset(fs, status, .true.)
        if (.not. status%ok()) return
        call vc_diff_build_argument_list_internal(fs, rev1, rev2, status)
        if (.not. status%ok()) return
        call vc_version_diff(rev1, rev2, status, .true.)
    end subroutine cmd_vc_version_diff

    recursive subroutine cmd_vc_root_version_diff(status)
        type(status_type), intent(out) :: status
        type(vc_fileset_type) :: args
        character(len=:), allocatable :: rev1, rev2, root
        logical :: found
        args%backend = vc_deduce_backend()
        if (args%backend == 0) then
            args%backend = vc_responsible_backend(default_directory(), status)
            if (.not. status%ok()) return
        end if
        call vc_call_root(args%backend, default_directory(), root, found, status)
        if (.not. status%ok()) return
        call one(args%files, root)
        call vc_diff_build_argument_list_internal(args, rev1, rev2, status)
        if (.not. status%ok()) return
        call vc_root_version_diff(rev1, rev2, status, .true.)
    end subroutine cmd_vc_root_version_diff

    recursive subroutine cmd_vc_root_diff(status)
        type(status_type), intent(out) :: status
        call vc_root_diff(.not. nilp(current_prefix_arg), status, .true., .true.)
    end subroutine cmd_vc_root_diff

    recursive subroutine cmd_vc_revision_other_window(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: rev
        type(handle_type) :: old
        type(status_type) :: st
        old = current_buffer()
        call vc_ensure_vc_buffer(status)
        if (.not. status%ok()) return
        call vc_read_revision("Revision to visit (default working revision): ", rev, status, &
                              [string_of(buffers(cur)%filename)])
        if (.not. status%ok()) return
        call vc_revision_other_window(rev, status)
        if (.not. status%ok() .and. buffer_live_p(old)) call set_buffer(old, st)
    end subroutine cmd_vc_revision_other_window

    recursive subroutine cmd_vc_revert(status)
        type(status_type), intent(out) :: status
        call vc_revert(status)
    end subroutine cmd_vc_revert

    recursive subroutine cmd_vc_delete_file(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file
        type(vc_string_type), allocatable :: files(:)
        if (len(buffer_file_name()) > 0 .and. vc_backend(buffer_file_name()) /= 0) then
            call read_file_name("VC delete file: ", file, status, default_filename=buffer_file_name(), &
                                mustmatch=REQUIRE_MATCH_T)
        else
            call read_file_name("VC delete file: ", file, status, mustmatch=REQUIRE_MATCH_T)
        end if
        if (.not. status%ok()) return
        call one(files, file)
        call vc_delete_file(files, status)
    end subroutine cmd_vc_delete_file

    recursive subroutine cmd_vc_rename_file(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: old, new
        if (len(buffer_file_name()) > 0 .and. vc_backend(buffer_file_name()) /= 0) then
            call read_file_name("VC rename file: ", old, status, default_filename=buffer_file_name(), &
                                mustmatch=REQUIRE_MATCH_T)
        else
            call read_file_name("VC rename file: ", old, status, mustmatch=REQUIRE_MATCH_T)
        end if
        if (.not. status%ok()) return
        call read_file_name("Rename to: ", new, status)
        if (.not. status%ok()) return
        call vc_rename_file(old, new, status)
    end subroutine cmd_vc_rename_file

    recursive subroutine cmd_vc_clear_context(status)
        type(status_type), intent(out) :: status
        status = status_type()
        call vc_clear_context()
    end subroutine cmd_vc_clear_context

    !> `vc_mode_line' as vc-mode-line-hook.
    subroutine mode_line_hook(file)
        character(len=*), intent(in) :: file
        call vc_mode_line(file)
    end subroutine mode_line_hook

    !> Register VC's commands and options.
    subroutine syms_of_vc()
        if (.not. allocated(vc_deduce_backend_nonvc_modes)) then
            vc_deduce_backend_nonvc_modes = [character(len=32) :: "dired_mode", "shell_mode", "eshell_mode", &
                                             "compilation_mode"]
        end if
        if (.not. allocated(locals)) allocate (locals(0))
        commands%vc_next_action%run => cmd_vc_next_action
        commands%vc_register%run => cmd_vc_register
        commands%vc_create_repo%run => cmd_vc_create_repo
        commands%vc_ignore%run => cmd_vc_ignore
        commands%vc_diff%run => cmd_vc_diff
        commands%vc_version_diff%run => cmd_vc_version_diff
        commands%vc_root_version_diff%run => cmd_vc_root_version_diff
        commands%vc_root_diff%run => cmd_vc_root_diff
        commands%vc_revision_other_window%run => cmd_vc_revision_other_window
        commands%vc_revert%run => cmd_vc_revert
        commands%vc_restore%run => cmd_vc_revert
        commands%vc_delete_file%run => cmd_vc_delete_file
        commands%vc_rename_file%run => cmd_vc_rename_file
        commands%vc_clear_context%run => cmd_vc_clear_context
        vc_mode_line_hook => mode_line_hook
        call add_local_variables_killer(kill_vc_locals)
        call syms_of_vc_dispatcher()
    end subroutine syms_of_vc
end module vc_fm
