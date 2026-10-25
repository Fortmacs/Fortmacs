! vc_git_fm.f90 -- VC backend for the git version control system.
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

!> The Git backend of VC: whether a file is registered, its state (from
!> `git status --porcelain'), working revision and branch for the mode
!> line, registering, unregistering, reverting, checking out, deleting
!> and renaming files, diffs (diff-index and diff-tree), other revisions
!> (cat-file), the previous revision, revision completion, the ignore
!> file and creating a repository.
!>
!> Committing (`vc_git_checkin') and its Log Edit mode
!> (`vc_git_log_edit_mode', with the Sign-Off and No-Verify headers).
!>
!> Deferred: the backend functions of the deferred VC commands (log,
!> annotate, dir-status, tags and branches, pull and push, stashes,
!> working trees, region history, patches and checking in patches,
!> asynchronous checkins, amending with \[vc_git_log_edit_toggle_amend]
!> and the published-history checks it needs), `vc_git_find_file_hook'
!> (Smerge mode for conflicts) and `vc_git_resolve_conflicts', the
!> difftool path of `vc_git_diff' (`vc_git_diff_switches' nil), the
!> coding systems of Git's output (UTF-8 is used), remote directories
!> and the other `vc_git_' options of those features.
module vc_git_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type, set_error, FM_GENERIC_ERROR
    use handle_fm, only: handle_type, null_handle
    use lisp_fm, only: value_type, nil, t_value, make_string, make_list, fixnump, xfixnum
    use buffer_fm, only: buffers, cur, current_buffer, set_buffer, get_buffer_create, kill_buffer, &
                         generate_new_buffer_name
    use editfns_fm, only: point, goto_char, point_min, point_max, buffer_string, bolp, bobp, pos_eol, &
                          buffer_substring
    use cmds_fm, only: forward_line
    use search_fm, only: string_match, re_search_forward, looking_at, SEARCH_NOERROR
    use subr_fm, only: match_string
    use fileio_fm, only: expand_file_name, file_directory_p, directory_name_p
    use files_fm, only: file_relative_name, executable_find, revert_buffer_in_progress
    use callproc_fm, only: call_process
    use vc_hooks_fm, only: vc_backends, vc_string_type, vc_switches_type, register_vc_backend, vc_find_root, &
                           vc_state, vc_mode_line_state, vc_working_revision, vc_file_get_git_symbolic_ref, &
                           vc_file_set_git_symbolic_ref, vc_display_status, vc_use_short_revision, &
                           VC_DISPLAY_STATUS_NO_BACKEND, VC_SWITCHES_T, VC_SWITCHES_NIL, VC_UP_TO_DATE, VC_EDITED, &
                           VC_ADDED, VC_REMOVED, VC_CONFLICT, VC_MISSING, VC_IGNORED, VC_UNREGISTERED, &
                           VC_MODEL_IMPLICIT
    use vc_dispatcher_fm, only: vc_do_command, VC_OKSTATUS_NIL, VC_OKSTATUS_ASYNC, VC_DEST_CURRENT
    use vc_fm, only: vc_switches
    use eval_fm, only: hook_type
    use commands_fm, only: commands
    use keymap_fm, only: keymap_set
    use modes_fm, only: major_modes
    use faces_fm, only: named_face_type, display_when, face_attrs
    use font_core_fm, only: font_lock_defaults, font_lock_keyword_type, font_lock_highlight_type, &
                            FONT_LOCK_OVERRIDE_PREPEND
    use font_lock_fm, only: font_lock_keyword, font_lock_highlight
    use log_edit_fm, only: log_edit_toggle_header, log_edit_extract_headers, log_edit_font_lock_keywords, &
                           log_edit_string_type, log_edit_header_type, syms_of_log_edit, LOG_EDIT_HEADER_FN
    implicit none(type, external)
    private

    public :: syms_of_vc_git, vc_git_registered, vc_git_state, vc_git_working_revision, vc_git_mode_line_string, &
              vc_git_root, vc_git_command, vc_git_previous_revision, vc_git_revision_table, vc_git_symbolic_commit, &
              vc_git_status_to_vc_state, vc_git_parse_program_version, vc_git_program_version

    !> String or list of strings specifying switches for Git diff under VC.
    !> If nil, use the value of `vc_diff_switches'.  If t, use no switches.
    !> Port of vc-git.el vc-git-diff-switches.
    type(vc_switches_type), target, public :: vc_git_diff_switches = vc_switches_type(kind=VC_SWITCHES_T)

    !> Name of the Git executable (excluding any arguments).  Port of
    !> vc-git.el vc-git-program.
    character(len=:), allocatable, public :: vc_git_program

    !> Control whether tags are returned by revision completion for Git.
    !>
    !> When true, only branches and remotes will be returned by
    !> `vc_git_revision_completion_table'.  This is used by various VC
    !> commands when completing branch names.  When false, tags are also
    !> included in the completions.  Port of vc-git.el
    !> vc-git-revision-complete-only-branches.
    logical, public :: vc_git_revision_complete_only_branches = .false.

    !> True to treat pathspecs in commands literally.
    !> Good example of file name that needs this: "test[56].xx".  Port of
    !> vc-git.el vc-git-use-literal-pathspecs.
    logical, public :: vc_git_use_literal_pathspecs = .true.

    !> Target length for Git commit summary lines.
    !> If a number, characters in Summary: lines beyond this length are
    !> displayed in the `vc_git_log_edit_summary_target_warning' face.
    !> A negative value means no highlighting.
    !>
    !> By setting this to an integer around 50, you can improve the
    !> compatibility of your commit messages with Git commands that
    !> print the summary line in width-constrained contexts.  However,
    !> many commit summaries will need to exceed this length.
    !>
    !> See also `vc_git_log_edit_summary_max_len'.  Port of vc-git.el
    !> vc-git-log-edit-summary-target-len (negative for nil).
    integer, public :: vc_git_log_edit_summary_target_len = -1

    !> Maximum length for Git commit summary lines.
    !> If a number, characters in summary lines beyond this length are
    !> displayed in the `vc_git_log_edit_summary_max_warning' face.
    !> A negative value means no highlighting.
    !>
    !> It is good practice to avoid writing summary lines longer than
    !> this because otherwise the summary line will be truncated in many
    !> contexts in which Git commands display summary lines.
    !>
    !> See also `vc_git_log_edit_summary_target_len'.  Port of vc-git.el
    !> vc-git-log-edit-summary-max-len.
    integer, public :: vc_git_log_edit_summary_max_len = 68

    !> Face for Git commit summary lines beyond the target length.
    !> See `vc_git_log_edit_summary_target_len'.  Port of vc-git.el
    !> vc-git-log-edit-summary-target-warning.
    type(named_face_type), target, public :: vc_git_log_edit_summary_target_warning
    !> Face for Git commit summary lines beyond the maximum length.
    !> See `vc_git_log_edit_summary_max_len'.  Port of vc-git.el
    !> vc-git-log-edit-summary-max-warning.
    type(named_face_type), target, public :: vc_git_log_edit_summary_max_warning

    !> Hook run by `vc_git_log_edit_mode'.
    type(hook_type), target, public :: vc_git_log_edit_mode_hook = hook_type(name="vc_git_log_edit_mode_hook")

    !> Keymap of `vc_git_log_edit_mode'.  Port of vc-git.el
    !> vc-git-log-edit-mode-map.
    type(handle_type), public :: vc_git_log_edit_mode_map

    !> Git object ID of the empty tree object.  Port of vc-git.el
    !> vc-git--empty-tree.
    character(len=*), parameter :: vc_git_empty_tree = "4b825dc642cb6eb9a060e54bf8d69288fbee4904"

    !> The version of Git (vc-git--program-version), "" until known.
    character(len=:), allocatable :: program_version

    character, parameter :: NUL = achar(0), LF = achar(10)

contains

    ! ------------------------------------------------------------------
    ! Internal commands
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

    !> Append nil to LIST (a flag passing nothing).
    subroutine push_nil(list)
        type(vc_string_type), allocatable, intent(inout) :: list(:)
        type(vc_string_type), allocatable :: grown(:)
        integer :: n
        if (.not. allocated(list)) allocate (list(0))
        n = size(list)
        allocate (grown(n + 1))
        grown(1:n) = list
        call move_alloc(grown, list)
    end subroutine push_nil

    !> The strings of A then B.
    subroutine append(list, more)
        type(vc_string_type), allocatable, intent(inout) :: list(:)
        type(vc_string_type), intent(in) :: more(:)
        type(vc_string_type), allocatable :: grown(:)
        integer :: n
        if (.not. allocated(list)) allocate (list(0))
        n = size(list)
        allocate (grown(n + size(more)))
        grown(1:n) = list
        grown(n + 1:) = more
        call move_alloc(grown, list)
    end subroutine append

    !> Return env vars for the `process_environment' of Git processes.
    !> Port of vc-git.el:vc-git--env-vars.
    subroutine env_vars(subcommand, env)
        character(len=*), intent(in) :: subcommand
        type(vc_string_type), allocatable, intent(out) :: env(:)
        allocate (env(0))
        call push(env, "GIT_DIR")
        if (vc_git_use_literal_pathspecs) call push(env, "GIT_LITERAL_PATHSPECS=1")
        ! Avoid optional repository locking during background operations
        ! (bug#21559, bug#80903).  Skipping these locks is always safe and
        ! can only lead to subsequent commands running more slowly.
        if (revert_buffer_in_progress .or. subcommand == "status") call push(env, "GIT_OPTIONAL_LOCKS=0")
    end subroutine env_vars

    !> The flags of `git' before SUBCOMMAND: --no-pager, and for older Git
    !> --no-optional-locks as `env_vars' sets GIT_OPTIONAL_LOCKS.  Port of
    !> vc-git.el:vc-git--no-optional-locks.
    subroutine leading_flags(subcommand, flags)
        character(len=*), intent(in) :: subcommand
        type(vc_string_type), allocatable, intent(out) :: flags(:)
        allocate (flags(0))
        call push(flags, "--no-pager")
        if (revert_buffer_in_progress .or. subcommand == "status") call push(flags, "--no-optional-locks")
    end subroutine leading_flags

    !> A wrapper around `vc_do_command' for use in vc_git_fm.
    !> The difference to `vc_do_command' is that this function always invokes
    !> `vc_git_program'.  BUFFER is the output buffer (*vc* if absent and
    !> CURRENT false; the current buffer with CURRENT).  Port of
    !> vc-git.el:vc-git-command.
    recursive subroutine vc_git_command(okstatus, files, flags, status, buffer, current, exit_status, process)
        integer, intent(in) :: okstatus
        type(vc_string_type), intent(in) :: files(:), flags(:)
        type(status_type), intent(out) :: status
        type(handle_type), intent(in), optional :: buffer
        logical, intent(in), optional :: current
        integer, intent(out), optional :: exit_status
        type(handle_type), intent(out), optional :: process
        type(vc_string_type), allocatable :: env(:), all(:), fl(:)
        type(handle_type) :: buf
        character(len=:), allocatable :: saved, root, subcommand
        logical :: rootdir, found, cur_buf
        status = status_type()
        subcommand = ""
        if (size(flags) > 0) then
            if (allocated(flags(1)%s)) subcommand = flags(1)%s
        end if
        call env_vars(subcommand, env)
        call leading_flags(subcommand, all)
        call append(all, flags)
        rootdir = .false.
        if (size(files) == 1) then
            if (directory_name_p(files(1)%s)) then
                call vc_git_root(files(1)%s, root, found)
                rootdir = found .and. root == files(1)%s
            end if
        end if
        saved = buffer_directory()
        if (rootdir) call set_buffer_directory(files(1)%s)
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
            allocate (fl(1))
            fl(1)%s = "."
        else if (rootdir) then
            allocate (fl(0))
        else
            fl = files
        end if
        cur_buf = .false.
        if (present(current)) cur_buf = current
        if (cur_buf) then
            call vc_do_command(VC_DEST_CURRENT, okstatus, vc_git_program, fl, all, status, exit_status=exit_status, &
                               process=process, environment=env)
        else
            if (present(buffer)) then
                buf = buffer
            else
                call get_buffer_create("*vc*", buf, status)
                if (.not. status%ok()) return
            end if
            call vc_do_command(VC_DEST_CURRENT, okstatus, vc_git_program, fl, all, status, buffer=buf, &
                               exit_status=exit_status, process=process, environment=env)
        end if
        call set_buffer_directory(saved)
    end subroutine vc_git_command

    !> The current buffer's `default_directory'.
    function buffer_directory() result(dir)
        character(len=:), allocatable :: dir
        dir = "./"
        if (cur == 0) return
        if (allocated(buffers(cur)%directory)) dir = buffers(cur)%directory
    end function buffer_directory

    subroutine set_buffer_directory(dir)
        character(len=*), intent(in) :: dir
        if (cur == 0) return
        buffers(cur)%directory = dir
    end subroutine set_buffer_directory

    !> Run `git COMMAND ARGS...' with the output (without standard error)
    !> in the current buffer when TO_BUFFER, else discarded; CODE is the
    !> exit status (-1 if killed by a signal).  Port of
    !> vc-git.el:vc-git--call.
    subroutine vc_git_call(to_buffer, command, args, code, status)
        logical, intent(in) :: to_buffer
        character(len=*), intent(in) :: command
        type(vc_string_type), intent(in) :: args(:)
        integer, intent(out) :: code
        type(status_type), intent(out) :: status
        type(vc_string_type), allocatable :: env(:), all(:)
        type(value_type), allocatable :: argv(:), envv(:)
        type(value_type) :: result
        integer :: k
        code = -1
        call env_vars(command, env)
        call leading_flags(command, all)
        call push(all, command)
        call append(all, args)
        allocate (argv(size(all)), envv(size(env)))
        do k = 1, size(all)
            argv(k) = make_string(all(k)%s)
        end do
        do k = 1, size(env)
            envv(k) = make_string(env(k)%s)
        end do
        if (to_buffer) then
            call call_process(vc_git_program, result, status, destination=t_value(), args=make_list(argv), &
                              error_file="/dev/null", environment=make_list(envv))
        else
            call call_process(vc_git_program, result, status, args=make_list(argv), environment=make_list(envv))
        end if
        if (.not. status%ok()) return
        if (fixnump(result)) code = int(xfixnum(result))
    end subroutine vc_git_call

    !> Run `git COMMAND ARGS...' and insert standard output in current buffer.
    !> Return whether the process exited with status zero.  Port of
    !> vc-git.el:vc-git--out-ok.
    logical function vc_git_out_ok(command, args) result(ok)
        character(len=*), intent(in) :: command
        type(vc_string_type), intent(in) :: args(:)
        type(status_type) :: st
        integer :: code
        call vc_git_call(.true., command, args, code, st)
        ok = st%ok() .and. code == 0
    end function vc_git_out_ok

    !> Make a temporary buffer current (with-temp-buffer); OLD is the
    !> buffer to go back to.
    subroutine temp_begin(old, tmp)
        type(handle_type), intent(out) :: old, tmp
        type(status_type) :: st
        character(len=:), allocatable :: dir
        old = current_buffer()
        dir = buffer_directory()
        call get_buffer_create(generate_new_buffer_name(" *temp*"), tmp, st)
        call set_buffer(tmp, st)
        call set_buffer_directory(dir)
    end subroutine temp_begin

    !> Kill the temporary buffer TMP and make OLD current again.
    subroutine temp_end(old, tmp)
        type(handle_type), intent(in) :: old, tmp
        type(status_type) :: st
        call kill_buffer(tmp, st)
        call set_buffer(old, st)
    end subroutine temp_end

    !> Run a git command on FILE and return its output as string (OK false
    !> on failure).  FILE can be absent.  Port of
    !> vc-git.el:vc-git--run-command-string.
    subroutine vc_git_run_command_string(args, str, ok, file)
        type(vc_string_type), intent(in) :: args(:)
        character(len=:), allocatable, intent(out) :: str
        logical, intent(out) :: ok
        character(len=*), intent(in), optional :: file
        type(vc_string_type), allocatable :: rest(:)
        type(handle_type) :: old, tmp
        integer :: k
        allocate (rest(0))
        do k = 2, size(args)
            call push(rest, args(k)%s)
        end do
        if (present(file)) call push(rest, file_relative_name(file))
        call temp_begin(old, tmp)
        ok = vc_git_out_ok(args(1)%s, rest)
        str = buffer_string()
        call temp_end(old, tmp)
        if (.not. ok) str = ""
    end subroutine vc_git_run_command_string

    !> The strings ARGS as a list (a helper for the short lists of git
    !> arguments).
    function strings(a1, a2, a3, a4, a5, a6, a7) result(list)
        character(len=*), intent(in) :: a1
        character(len=*), intent(in), optional :: a2, a3, a4, a5, a6, a7
        type(vc_string_type), allocatable :: list(:)
        allocate (list(0))
        call push(list, a1)
        if (present(a2)) call push(list, a2)
        if (present(a3)) call push(list, a3)
        if (present(a4)) call push(list, a4)
        if (present(a5)) call push(list, a5)
        if (present(a6)) call push(list, a6)
        if (present(a7)) call push(list, a7)
    end function strings

    !> Check if the git db is empty (no commit done yet).  Port of
    !> vc-git.el:vc-git--empty-db-p.
    logical function vc_git_empty_db_p() result(empty)
        type(status_type) :: st
        integer :: code
        call vc_git_call(.false., "rev-parse", strings("--verify", "HEAD"), code, st)
        empty = code /= 0
    end function vc_git_empty_db_p

    !> The version of Git, "0" if unknown.  Port of
    !> vc-git.el:vc-git--program-version (the parsing is
    !> `vc_git_parse_program_version').
    function vc_git_program_version() result(version)
        character(len=:), allocatable :: version
        character(len=:), allocatable :: s
        logical :: ok
        if (allocated(program_version)) then
            version = program_version
            return
        end if
        call vc_git_run_command_string(strings("version"), s, ok)
        if (.not. ok) s = ""
        version = vc_git_parse_program_version(s)
        program_version = version
    end function vc_git_program_version

    !> The version in VERSION_STRING, the output of `git version' ("0" if
    !> none).  Some Git versions append additional strings to the
    !> numerical version string. E.g., Git for Windows appends
    !> ".windows.N", while Git for Mac appends " (Apple Git-N)". Capture
    !> numerical version and ignore the rest.  Part of
    !> vc-git.el:vc-git--program-version.
    function vc_git_parse_program_version(version_string) result(version)
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
    end function vc_git_parse_program_version

    !> Whether version A is lower than or equal to B (version<=, for
    !> versions of numbers and dots).
    logical function version_le(a, b)
        character(len=*), intent(in) :: a, b
        integer :: ia, ib, na, nb
        ia = 1
        ib = 1
        do
            na = next_number(a, ia)
            nb = next_number(b, ib)
            if (na /= nb) then
                version_le = na < nb
                return
            end if
            if (ia > len(a) .and. ib > len(b)) then
                version_le = .true.
                return
            end if
        end do
    contains

        integer function next_number(s, i) result(n)
            character(len=*), intent(in) :: s
            integer, intent(inout) :: i
            n = 0
            if (i > len(s)) return
            do while (i <= len(s))
                if (s(i:i) == ".") exit
                if (s(i:i) >= "0" .and. s(i:i) <= "9") n = 10*n + iachar(s(i:i)) - 48
                i = i + 1
            end do
            i = i + 1
        end function next_number
    end function version_le

    ! ------------------------------------------------------------------
    ! State-querying functions
    ! ------------------------------------------------------------------

    !> Check whether FILE is registered with git.  Port of
    !> vc-git.el:vc-git-registered.
    logical function vc_git_registered(file) result(registered)
        character(len=:), allocatable :: dir, path, name, str
        type(handle_type) :: old, tmp
        logical :: found, ok
        character(len=*), intent(in) :: file
        registered = .false.
        call vc_git_root(file, dir, found)
        if (.not. found) return
        ! If git(1) isn't installed then the `with-demoted-errors'
        ! below will mean we get an error message echoed about that
        ! fact with every `find-file'.  That's noisy, and inconsistent
        ! with other backend's `vc-*-registered' functions which are
        ! quieter in the case that the VCS isn't installed.  So check
        ! up here that git(1) is available.  See also bug#18481.
        call executable_find(vc_git_program, path)
        if (len(path) == 0) return
        ! Do not use the `file-name-directory' here: git-ls-files
        ! sometimes fails to return the correct status for relative
        ! path specs.
        name = file_relative_name(file, dir)
        call temp_begin(old, tmp)
        call set_buffer_directory(dir)
        ok = vc_git_out_ok("ls-files", strings("-c", "-z", "--", name))
        ! If result is empty, use ls-tree to check for deleted file.
        if (point_min() == point_max()) ok = vc_git_out_ok("ls-tree", strings("--name-only", "-z", "HEAD", "--", name))
        str = buffer_string()
        call temp_end(old, tmp)
        if (len(str) > len(name)) registered = str(1:len(name) + 1) == name//NUL
    end function vc_git_registered

    !> Convert CODE_LIST to a VC status.
    !>
    !> Each element of CODE_LIST comes from the first two characters of
    !> a line returned by `git status --porcelain' and should be passed
    !> in the order given by `git status'.  Port of
    !> vc-git.el:vc-git--git-status-to-vc-state.
    integer function vc_git_status_to_vc_state(codes) result(state)
        type(vc_string_type), intent(in) :: codes(:)
        type(status_type) :: st
        character(len=:), allocatable :: code
        ! It is necessary to allow CODE-LIST to be a list because sometimes git
        ! status returns multiple lines, e.g. for a file that is removed from
        ! the index but is present in the HEAD and working tree.
        if (size(codes) == 0) then
            state = VC_UP_TO_DATE
        else if (size(codes) == 1) then
            code = codes(1)%s
            if (code == "!!") then
                state = VC_IGNORED
            else if (code == "??") then
                state = VC_UNREGISTERED
            else if (code == "D ") then
                state = VC_REMOVED
            else if (string_match("^.D$", code, st) > 0) then
                state = VC_MISSING
            else if (string_match("^[ M]+$", code, st) > 0) then
                state = VC_EDITED
            else if (string_match("^[ A]+$", code, st) > 0) then
                state = VC_ADDED
            else if (string_match("^[ U]+$", code, st) > 0) then
                state = VC_CONFLICT
            else
                state = VC_EDITED
            end if
        else if (size(codes) == 2 .and. codes(1)%s == "D " .and. codes(2)%s == "??") then
            ! I know of two cases when git state returns more than one element,
            ! in both cases returning '("D " "??")':
            ! 1. When a file is removed from the index but present in the
            !    HEAD and working tree.
            ! 2. When a file A is renamed to B in the index and then back to A
            !    in the working tree.
            ! In both of these instances, `unregistered' is a reasonable response.
            state = VC_UNREGISTERED
        else
            ! In other cases, let us return `edited'.
            state = VC_EDITED
        end if
    end function vc_git_status_to_vc_state

    !> Git-specific version of `vc_state'.  Port of vc-git.el:vc-git-state.
    integer function vc_git_state(file) result(state)
        character(len=*), intent(in) :: file
        type(vc_string_type), allocatable :: args(:), codes(:)
        character(len=:), allocatable :: status
        logical :: ok
        integer :: b, e
        ! It can't set `needs-update' or `needs-merge'. The rough
        ! equivalent would be that upstream branch for current branch is in
        ! fast-forward state i.e. current branch is direct ancestor of
        ! corresponding upstream branch, and the file was modified
        ! upstream.  We'd need to check against the upstream tracking
        ! branch for that (an extra process call or two).
        args = strings("status", "--porcelain", "-z", "--untracked-files")
        ! Just to be explicit, it's the default anyway.
        if (version_le("1.7.6.3", vc_git_program_version())) call push(args, "--ignored")
        call push(args, "--")
        call vc_git_run_command_string(args, status, ok, file)
        if (.not. ok) then
            ! If status is nil, there was an error calling git, likely because
            ! the file is not in a git repo.
            state = VC_UNREGISTERED
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
            if (e > b) call push(codes, status(b:min(b + 1, e - 1)))
            b = e + 1
        end do
        state = vc_git_status_to_vc_state(codes)
    end function vc_git_state

    !> Git-specific version of `vc_working_revision'.  Port of
    !> vc-git.el:vc-git-working-revision.
    subroutine vc_git_working_revision(file, rev, found)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: rev
        logical, intent(out) :: found
        if (len(file) < 0) return
        call vc_git_rev_parse("HEAD", rev, found)
    end subroutine vc_git_working_revision

    !> The working revision's symbolic name: HEAD.  Port of vc-git.el
    !> vc-git-working-revision-symbol.
    function vc_git_working_revision_symbol() result(s)
        character(len=:), allocatable :: s
        s = "HEAD"
    end function vc_git_working_revision_symbol

    !> The branch HEAD refers to (FOUND false when detached), cached in
    !> FILE's `vc-git-symbolic-ref'.  Port of
    !> vc-git.el:vc-git--symbolic-ref.
    subroutine vc_git_symbolic_ref(file, ref, found)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: ref
        logical, intent(out) :: found
        character(len=:), allocatable :: str
        type(status_type) :: st
        call vc_file_get_git_symbolic_ref(file, ref, found)
        if (found) return
        call vc_git_run_command_string(strings("symbolic-ref", "HEAD"), str, found)
        if (found) then
            if (string_match("^\(refs/heads/\)?\(.+\)$", str, st) > 0) then
                ref = match_string(2, str)
            else
                ref = str
            end if
            call vc_file_set_git_symbolic_ref(file, ref)
        else
            call vc_file_set_git_symbolic_ref(file)
        end if
    end subroutine vc_git_symbolic_ref

    !> Return a string for `vc_mode_line' to put in the mode line for FILE.
    !> Port of vc-git.el:vc-git-mode-line-string.
    subroutine vc_git_mode_line_string(file, s, found)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: s
        logical, intent(out) :: found
        character(len=:), allocatable :: echo, face, indicator, rev, disp_rev, locker
        logical :: have_rev, have_ref
        integer :: state
        state = vc_state(file, locker=locker)
        call vc_mode_line_state(state, echo, face, indicator, locker)
        call vc_working_revision(file, rev, have_rev, git_backend_index())
        call vc_git_symbolic_ref(file, disp_rev, have_ref)
        if (.not. have_ref) then
            disp_rev = ""
            if (have_rev) disp_rev = rev(1:min(7, len(rev)))
        end if
        s = ""
        if (vc_display_status /= VC_DISPLAY_STATUS_NO_BACKEND) s = "Git"
        s = s//indicator//disp_rev
        found = .true.
    end subroutine vc_git_mode_line_string

    !> The index of the Git backend.
    integer function git_backend_index() result(b)
        use vc_hooks_fm, only: vc_backend_named
        b = vc_backend_named("Git")
    end function git_backend_index

    !> The root of FILE's working tree (the directory with .git).  Port of
    !> vc-git.el:vc-git-root.
    subroutine vc_git_root(file, root, found)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: root
        logical, intent(out) :: found
        call vc_find_root(file, ".git", root, found)
    end subroutine vc_git_root

    !> Git's checkout model: `implicit'.  Port of
    !> vc-git.el:vc-git-checkout-model.
    integer function vc_git_checkout_model(files) result(model)
        type(vc_string_type), intent(in) :: files(:)
        model = VC_MODEL_IMPLICIT
        if (size(files) < 0) model = 0
    end function vc_git_checkout_model

    ! ------------------------------------------------------------------
    ! State-changing functions
    ! ------------------------------------------------------------------

    !> Create a new Git repository.  Port of vc-git.el:vc-git-create-repo.
    recursive subroutine vc_git_create_repo(status)
        type(status_type), intent(out) :: status
        type(vc_string_type) :: none(0)
        call vc_git_command(0, none, strings("init"), status)
    end subroutine vc_git_create_repo

    !> Register FILES into the git version-control system.  Port of
    !> vc-git.el:vc-git-register.
    recursive subroutine vc_git_register(files, status, comment)
        type(vc_string_type), intent(in) :: files(:)
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: comment
        type(vc_string_type), allocatable :: flist(:), dlist(:)
        integer :: k
        status = status_type()
        if (present(comment)) continue
        allocate (flist(0), dlist(0))
        do k = size(files), 1, -1
            if (file_directory_p(files(k)%s)) then
                call push(dlist, files(k)%s)
            else
                call push(flist, files(k)%s)
            end if
        end do
        if (size(flist) > 0) then
            call vc_git_command(0, flist, strings("update-index", "--add", "--"), status)
            if (.not. status%ok()) return
        end if
        if (size(dlist) > 0) call vc_git_command(0, dlist, strings("add"), status)
    end subroutine vc_git_register

    !> Remove FILE from the index.  Port of vc-git.el:vc-git-unregister.
    recursive subroutine vc_git_unregister(file, status)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        call vc_git_command(0, strings(file), strings("rm", "-f", "--cached", "--"), status)
    end subroutine vc_git_unregister

    !> Get revision REV of FILE into BUFFER.  Port of
    !> vc-git.el:vc-git-find-revision.
    recursive subroutine vc_git_find_revision(file, rev, buffer, status)
        character(len=*), intent(in) :: file, rev
        type(handle_type), intent(in) :: buffer
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: fn, fullname, root, r
        type(vc_string_type) :: none(0)
        logical :: ok, found
        call vc_git_run_command_string(strings("ls-files", "-z", "--full-name", "--"), fn, ok, file)
        ! ls-files does not return anything when looking for a revision of
        ! a file that has been renamed or removed.
        if (len(fn) == 0) then
            call vc_git_root(buffer_directory(), root, found)
            fullname = file_relative_name(file, root)
        else
            fullname = fn(1:len(fn) - 1)
        end if
        r = rev
        if (len(r) == 0) r = "HEAD"
        call vc_git_command(0, none, strings("cat-file", "--filters", r//":"//fullname), status, buffer=buffer)
    end subroutine vc_git_find_revision

    !> Return the git ignore file that controls FILE.  Port of
    !> vc-git.el:vc-git-find-ignore-file.
    subroutine vc_git_find_ignore_file(file, ignore, found)
        character(len=*), intent(in) :: file
        character(len=:), allocatable, intent(out) :: ignore
        logical, intent(out) :: found
        character(len=:), allocatable :: root
        type(status_type) :: st
        call vc_git_root(file, root, found)
        call expand_file_name(".gitignore", ignore, st, root)
        found = .true.
    end subroutine vc_git_find_ignore_file

    !> Check out REV (HEAD if absent) of FILE.  Port of
    !> vc-git.el:vc-git-checkout.
    recursive subroutine vc_git_checkout(file, status, rev)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: rev
        if (present(rev)) then
            call vc_git_command(0, strings(file), strings("checkout", rev), status)
        else
            call vc_git_command(0, strings(file), strings("checkout", "HEAD"), status)
        end if
    end subroutine vc_git_checkout

    !> Revert FILE to the version stored in the Git repository.  Port of
    !> vc-git.el:vc-git-revert.
    recursive subroutine vc_git_revert(file, status, contents_done)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: contents_done
        if (present(contents_done)) then
            call vc_git_command(0, strings(file), strings("update-index", "--"), status)
        else
            call vc_git_command(0, strings(file), strings("reset", "-q", "--"), status)
            if (.not. status%ok()) return
            call vc_git_command(VC_OKSTATUS_NIL, strings(file), strings("checkout", "-q", "--"), status)
        end if
    end subroutine vc_git_revert

    !> Revert FILES to the versions stored in the Git repository.  Port of
    !> vc-git.el:vc-git-revert-files.
    recursive subroutine vc_git_revert_files(files, status)
        type(vc_string_type), intent(in) :: files(:)
        type(status_type), intent(out) :: status
        call vc_git_command(0, files, strings("reset", "-q", "--"), status)
        if (.not. status%ok()) return
        call vc_git_command(VC_OKSTATUS_NIL, files, strings("checkout", "-q", "--"), status)
    end subroutine vc_git_revert_files

    !> Delete FILE with `git rm'.  Port of vc-git.el:vc-git-delete-file.
    recursive subroutine vc_git_delete_file(file, status)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        call vc_git_command(0, strings(file), strings("rm", "-f", "--"), status)
    end subroutine vc_git_delete_file

    !> Delete FILES with `git rm'.  Port of vc-git.el:vc-git-delete-files.
    recursive subroutine vc_git_delete_files(files, status)
        type(vc_string_type), intent(in) :: files(:)
        type(status_type), intent(out) :: status
        call vc_git_command(0, files, strings("rm", "-rf", "--"), status)
    end subroutine vc_git_delete_files

    !> Rename OLD to NEW with `git mv'.  Port of vc-git.el:vc-git-rename-file.
    recursive subroutine vc_git_rename_file(old, new, status)
        character(len=*), intent(in) :: old, new
        type(status_type), intent(out) :: status
        call vc_git_command(0, strings(old, new), strings("mv", "-f", "--"), status)
    end subroutine vc_git_rename_file

    !> Mark the conflicts in FILES as resolved (`git add').  Port of
    !> vc-git.el:vc-git-mark-resolved.
    recursive subroutine vc_git_mark_resolved(files, status)
        type(vc_string_type), intent(in) :: files(:)
        type(status_type), intent(out) :: status
        call vc_git_command(0, files, strings("add"), status)
    end subroutine vc_git_mark_resolved

    ! ------------------------------------------------------------------
    ! Diff and revisions
    ! ------------------------------------------------------------------

    !> Get a difference report using Git between two revisions of FILES.
    !> Port of vc-git.el:vc-git-diff (`vc_git_diff_switches' not nil; the
    !> difftool path is deferred).
    recursive subroutine vc_git_diff(files, buffer, async, status, rev1, rev2)
        type(vc_string_type), intent(in) :: files(:)
        type(handle_type), intent(in) :: buffer
        logical, intent(in) :: async
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: rev1, rev2
        type(vc_string_type), allocatable :: flags(:), sw(:)
        character(len=:), allocatable :: command, r1
        integer :: okstatus
        status = status_type()
        command = "diff-tree"
        if (present(rev2)) then
            r1 = vc_git_empty_tree
            if (present(rev1)) r1 = rev1
        else
            command = "diff-index"
            if (present(rev1)) then
                r1 = rev1
            else if (vc_git_empty_db_p()) then
                ! If there aren't any commits yet then there is no HEAD.
                ! So diff against the empty tree object.
                r1 = vc_git_empty_tree
            else
                r1 = "HEAD"
            end if
        end if
        if (vc_git_diff_switches%kind == VC_SWITCHES_NIL) then
            call set_error(status, FM_GENERIC_ERROR, "vc_git_diff with git difftool is not ported yet")
            return
        end if
        flags = strings(command, "--exit-code")
        call vc_switches(git_backend_index(), "diff", sw)
        call append(flags, sw)
        call push(flags, "-p")
        call push(flags, r1)
        if (present(rev2)) then
            call push(flags, rev2)
        else
            call push_nil(flags)
        end if
        call push(flags, "--")
        okstatus = 1
        if (async) okstatus = VC_OKSTATUS_ASYNC
        call vc_git_command(okstatus, files, flags, status, buffer=buffer)
    end subroutine vc_git_diff

    !> The revisions Git can complete: HEAD and the branches, tags (unless
    !> `vc_git_revision_complete_only_branches') and remotes.  Port of
    !> vc-git.el:vc-git-revision-table (as
    !> vc-git-revision-completion-table).
    subroutine vc_git_revision_table(files, table)
        type(vc_string_type), intent(in) :: files(:)
        type(vc_string_type), allocatable, intent(out) :: table(:)
        type(vc_string_type) :: none(0)
        type(handle_type) :: old, tmp
        type(status_type) :: st
        character(len=:), allocatable :: regexp
        integer(int64) :: found
        ! What about `files'?!?  --Stef
        if (size(files) < 0) return
        allocate (table(0))
        call push(table, "HEAD")
        call temp_begin(old, tmp)
        call vc_git_command(VC_OKSTATUS_NIL, none, strings("for-each-ref", "--format=%(refname)"), st, current=.true.)
        call goto_char(point_min())
        if (vc_git_revision_complete_only_branches) then
            regexp = "^refs/\(heads\|remotes\)/\(.*\)$"
        else
            regexp = "^refs/\(heads\|tags\|remotes\)/\(.*\)$"
        end if
        do
            call re_search_forward(regexp, found, st, noerror=SEARCH_NOERROR)
            if (found == 0) exit
            call push(table, match_in_buffer(2))
        end do
        call temp_end(old, tmp)
    end subroutine vc_git_revision_table

    !> The text of group N of the last buffer match.
    function match_in_buffer(n) result(s)
        integer, intent(in) :: n
        character(len=:), allocatable :: s
        s = match_string(n)
    end function match_in_buffer

    !> `--abbrev-commit' with `vc_use_short_revision', else
    !> `--no-abbrev-commit'.  Port of vc-git.el:vc-git--maybe-abbrev.
    function maybe_abbrev() result(s)
        character(len=:), allocatable :: s
        if (vc_use_short_revision) then
            s = "--abbrev-commit"
        else
            s = "--no-abbrev-commit"
        end if
    end function maybe_abbrev

    !> Git-specific version of `vc_previous_revision'.  Port of
    !> vc-git.el:vc-git-previous-revision.
    subroutine vc_git_previous_revision(file, rev, prev, found)
        character(len=*), intent(in) :: file, rev
        character(len=:), allocatable, intent(out) :: prev
        logical, intent(out) :: found
        character(len=:), allocatable :: fname, sym, carets
        type(handle_type) :: old, tmp
        type(status_type) :: st
        integer(int64) :: shortage, p
        logical :: ok
        integer :: n
        prev = ""
        found = .false.
        if (string_match("\`HEAD\(\^*\)\'", rev, st) > 0) then
            carets = match_string(1, rev)
            prev = "HEAD~"//itoa(1 + len(carets))
            found = .true.
        else if (string_match("\`HEAD~\([0-9]+\)\'", rev, st) > 0) then
            carets = match_string(1, rev)
            read (carets, *) n
            prev = "HEAD~"//itoa(1 + n)
            found = .true.
        else if (len(file) > 0) then
            fname = file_relative_name(file)
            call temp_begin(old, tmp)
            ok = vc_git_out_ok("rev-list", strings(maybe_abbrev(), "-2", rev, "--", fname))
            if (ok) then
                call goto_char(point_max())
                ok = bolp()
            end if
            if (ok) then
                shortage = forward_line(-1_int64)
                ok = shortage == 0 .and. .not. bobp()
            end if
            if (ok) then
                p = point()
                call buffer_substring(p, point_max() - 1, prev, st)
                found = st%ok()
            end if
            call temp_end(old, tmp)
            if (found) then
                call vc_git_symbolic_commit(prev, sym, ok)
                if (ok) prev = sym
            end if
        else
            ! We used to use "^" here, but that fails on MS-Windows if git
            ! is invoked via a batch file, in which case cmd.exe strips
            ! the "^" because it is a special character for cmd which
            ! process-file does not (and cannot) quote.
            call vc_git_rev_parse(rev//"~1", prev, found)
        end if
    end subroutine vc_git_previous_revision

    !> The revision REV names (FOUND false if none).  Port of
    !> vc-git.el:vc-git--rev-parse.
    subroutine vc_git_rev_parse(rev, out, found)
        character(len=*), intent(in) :: rev
        character(len=:), allocatable, intent(out) :: out
        logical, intent(out) :: found
        type(handle_type) :: old, tmp
        type(status_type) :: st
        type(vc_string_type), allocatable :: args(:)
        out = ""
        allocate (args(0))
        if (vc_use_short_revision) call push(args, "--short")
        call push(args, rev)
        call temp_begin(old, tmp)
        found = vc_git_out_ok("rev-parse", args)
        if (found) then
            call goto_char(point_min())
            call buffer_substring(point(), pos_eol(), out, st)
        end if
        call temp_end(old, tmp)
    end subroutine vc_git_rev_parse

    !> Translate revision string of COMMIT to a symbolic form.
    !> If FORCE is true, the result is allowed to include revision
    !> specifications like "master~8" (the 8th parent of the commit
    !> currently pointed to by the master branch), otherwise such revision
    !> specifications are rejected, and FOUND is false.  Port of
    !> vc-git.el:vc-git-symbolic-commit.
    subroutine vc_git_symbolic_commit(commit, sym, found, force)
        character(len=*), intent(in) :: commit
        character(len=:), allocatable, intent(out) :: sym
        logical, intent(out) :: found
        logical, intent(in), optional :: force
        type(handle_type) :: old, tmp
        type(status_type) :: st
        integer(int64) :: shortage
        logical :: f
        sym = ""
        f = .false.
        if (present(force)) f = force
        call temp_begin(old, tmp)
        found = vc_git_out_ok("name-rev", strings("--no-undefined", "--name-only", commit))
        if (found) then
            call goto_char(point_min())
            if (.not. f) found = .not. looking_at("^.*[~^].*$", st)
        end if
        if (found) then
            shortage = forward_line(2_int64)
            found = shortage == 1 .and. bolp()
        end if
        if (found) call buffer_substring(point_min(), point_max() - 1, sym, st)
        call temp_end(old, tmp)
    end subroutine vc_git_symbolic_commit

    !> N as a decimal string.
    function itoa(n) result(s)
        integer, intent(in) :: n
        character(len=:), allocatable :: s
        character(len=16) :: buf
        write (buf, "(i0)") n
        s = trim(buf)
    end function itoa

    ! ------------------------------------------------------------------
    ! Checking in
    ! ------------------------------------------------------------------

    !> Toggle whether to add the "Signed-off-by" line at the end of commit message.
    !> Port of vc-git.el:vc-git-log-edit-toggle-signoff.
    recursive subroutine vc_git_log_edit_toggle_signoff(status)
        type(status_type), intent(out) :: status
        logical :: on
        on = log_edit_toggle_header("Sign-Off", "yes", status)
    end subroutine vc_git_log_edit_toggle_signoff

    !> Toggle whether to bypass the pre-commit and commit-msg hooks.  Port
    !> of vc-git.el:vc-git-log-edit-toggle-no-verify.
    recursive subroutine vc_git_log_edit_toggle_no_verify(status)
        type(status_type), intent(out) :: status
        logical :: on
        on = log_edit_toggle_header("No-Verify", "yes", status)
    end subroutine vc_git_log_edit_toggle_no_verify

    ! Port of vc-git.el:vc-git--log-edit-summary-check.
    subroutine log_edit_summary_check(limit, found, status)
        integer(int64), intent(in) :: limit
        logical, intent(out) :: found
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: regex
        integer(int64) :: pos
        found = .false.
        call re_search_forward("^Summary: ", pos, status, bound=limit, noerror=SEARCH_NOERROR)
        if (pos == 0 .or. .not. status%ok()) return
        if (vc_git_log_edit_summary_max_len >= 0 .and. vc_git_log_edit_summary_target_len >= 0) then
            regex = ".\{,"//itoa(vc_git_log_edit_summary_target_len)//"\}\(.\{,"// &
                    itoa(vc_git_log_edit_summary_max_len - vc_git_log_edit_summary_target_len)//"\}\)\(.*\)"
        else if (vc_git_log_edit_summary_max_len >= 0) then
            regex = ".\{,"//itoa(vc_git_log_edit_summary_max_len)//"\}\(?2:.*\)"
        else if (vc_git_log_edit_summary_target_len >= 0) then
            regex = ".\{,"//itoa(vc_git_log_edit_summary_target_len)//"\}\(.*\)"
        else
            return
        end if
        call re_search_forward(regex, pos, status, bound=limit, noerror=SEARCH_NOERROR)
        found = pos /= 0
    end subroutine log_edit_summary_check

    !> Major mode for editing Git log messages.
    !> It is based on `log_edit_mode', and has Git-specific extensions.
    !> Port of vc-git.el:vc-git-log-edit-mode (\[vc_git_log_edit_toggle_amend]
    !> is deferred).
    recursive subroutine vc_git_log_edit_mode(status)
        type(status_type), intent(out) :: status
        type(font_lock_keyword_type), allocatable :: keywords(:), more(:)
        type(font_lock_highlight_type) :: h(2)
        logical :: saved
        if (.not. associated(major_modes%vc_git_log_edit_mode%run)) call define_log_edit_mode()
        call major_modes%vc_git_log_edit_mode%begin(saved, status)
        if (.not. status%ok()) return
        call log_edit_font_lock_keywords(keywords)
        h(1) = font_lock_highlight(1, face="vc_git_log_edit_summary_target_warning", override=FONT_LOCK_OVERRIDE_PREPEND, &
                                   laxmatch=.true.)
        h(2) = font_lock_highlight(2, face="vc_git_log_edit_summary_max_warning", override=FONT_LOCK_OVERRIDE_PREPEND, &
                                   laxmatch=.true.)
        allocate (more(1))
        more(1) = font_lock_keyword(highlights=h, matcher=log_edit_summary_check)
        keywords = [keywords, more]
        call font_lock_defaults%set_local(keywords=keywords, keywords_only=.true.)
        call major_modes%vc_git_log_edit_mode%finish(saved, status)
    end subroutine vc_git_log_edit_mode

    ! Define Git's Log Edit mode, its keymap and faces.
    subroutine define_log_edit_mode()
        type(named_face_type), pointer :: f
        type(status_type) :: st
        f => vc_git_log_edit_summary_target_warning
        call f%define("vc_git_log_edit_summary_target_warning", &
                      "Face for Git commit summary lines beyond the target length."//LF// &
                      "See `vc_git_log_edit_summary_target_len'.")
        call f%spec_entry(display_when(), face_attrs(inherit="warning"))
        f => vc_git_log_edit_summary_max_warning
        call f%define("vc_git_log_edit_summary_max_warning", &
                      "Face for Git commit summary lines beyond the maximum length."//LF// &
                      "See `vc_git_log_edit_summary_max_len'.")
        call f%spec_entry(display_when(), face_attrs(inherit="error"))
        if (.not. associated(major_modes%log_edit_mode%run)) call syms_of_log_edit()
        call major_modes%vc_git_log_edit_mode%define("vc_git_log_edit_mode", "Log-Edit/git", vc_git_log_edit_mode, &
                                                     vc_git_log_edit_mode_hook, parent=major_modes%log_edit_mode)
        vc_git_log_edit_mode_map = major_modes%vc_git_log_edit_mode%map
        commands%vc_git_log_edit_mode%run => vc_git_log_edit_mode
        commands%vc_git_log_edit_toggle_signoff%run => vc_git_log_edit_toggle_signoff
        commands%vc_git_log_edit_toggle_no_verify%run => vc_git_log_edit_toggle_no_verify
        call keymap_set(vc_git_log_edit_mode_map, "C-c C-s", "vc_git_log_edit_toggle_signoff", st)
        call keymap_set(vc_git_log_edit_mode_map, "C-c C-n", "vc_git_log_edit_toggle_no_verify", st)
    end subroutine define_log_edit_mode

    ! (boolean-arg-fn "--amend") and friends: the argument if the header's
    ! value is "yes".
    subroutine amend_arg(value, args)
        character(len=*), intent(in) :: value
        type(log_edit_string_type), allocatable, intent(out) :: args(:)
        call boolean_arg(value, "--amend", args)
    end subroutine amend_arg

    subroutine no_verify_arg(value, args)
        character(len=*), intent(in) :: value
        type(log_edit_string_type), allocatable, intent(out) :: args(:)
        call boolean_arg(value, "--no-verify", args)
    end subroutine no_verify_arg

    subroutine signoff_arg(value, args)
        character(len=*), intent(in) :: value
        type(log_edit_string_type), allocatable, intent(out) :: args(:)
        call boolean_arg(value, "--signoff", args)
    end subroutine signoff_arg

    subroutine boolean_arg(value, argument, args)
        character(len=*), intent(in) :: value, argument
        type(log_edit_string_type), allocatable, intent(out) :: args(:)
        if (value == "yes") then
            allocate (args(1))
            args(1)%s = argument
        else
            allocate (args(0))
        end if
    end subroutine boolean_arg

    ! Port of vc-git.el:vc-git--log-edit-extract-headers: MSG and the
    ! command line arguments ARGS.
    recursive subroutine log_edit_extract_headers_git(comment, msg, args, status)
        character(len=*), intent(in) :: comment
        character(len=:), allocatable, intent(out) :: msg
        type(log_edit_string_type), allocatable, intent(out) :: args(:)
        type(status_type), intent(out) :: status
        type(log_edit_header_type) :: headers(5)
        headers(1)%name = "Author"
        headers(1)%cmdarg = "--author"
        headers(2)%name = "Date"
        headers(2)%cmdarg = "--date"
        headers(3)%name = "Amend"
        headers(3)%kind = LOG_EDIT_HEADER_FN
        headers(3)%fn => amend_arg
        headers(4)%name = "No-Verify"
        headers(4)%kind = LOG_EDIT_HEADER_FN
        headers(4)%fn => no_verify_arg
        headers(5)%name = "Sign-Off"
        headers(5)%kind = LOG_EDIT_HEADER_FN
        headers(5)%fn => signoff_arg
        call log_edit_extract_headers(headers, comment, msg, args, status)
    end subroutine log_edit_extract_headers_git

    !> Git-specific version of `vc_backend_checkin'.
    !> REV is ignored.  Port of vc-git.el:vc-git-checkin.
    recursive subroutine vc_git_checkin(files, comment, status, rev)
        type(vc_string_type), intent(in) :: files(:)
        character(len=*), intent(in) :: comment
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: rev
        if (present(rev)) continue
        call vc_git_checkin_1(comment, files, status)
    end subroutine vc_git_checkin

    !> Workhorse routine for `vc_git_checkin'.
    !> COMMENT is the commit message.  FILES is the list of files to check
    !> in.  Port of vc-git.el:vc-git--checkin (checking in a patch, and the
    !> asynchronous commit of `vc_async_checkin', are deferred; the message
    !> goes on the command line, as on GNU/Linux).
    recursive subroutine vc_git_checkin_1(comment, files, status)
        use files_fm, only: abbreviate_file_name
        character(len=*), intent(in) :: comment
        type(vc_string_type), intent(in) :: files(:)
        type(status_type), intent(out) :: status
        type(log_edit_string_type), allocatable :: extracted(:)
        type(vc_string_type), allocatable :: flags(:), fl(:)
        character(len=:), allocatable :: file1, root, saved, msg
        logical :: found, only
        integer :: k
        status = status_type()
        if (size(files) > 0) then
            file1 = files(1)%s
        else
            file1 = buffer_directory()
        end if
        call vc_git_root(file1, root, found)
        if (.not. found) then
            call set_error(status, FM_GENERIC_ERROR, "Invalid call to `vc_git_checkin'")
            return
        end if
        only = size(files) > 1
        if (.not. only) only = root /= abbreviate_file_name(file1)
        saved = buffer_directory()
        file1 = root
        call expand_file_name(file1, root, status)
        if (.not. status%ok()) return
        call set_buffer_directory(root)
        call log_edit_extract_headers_git(comment, msg, extracted, status)
        if (status%ok()) then
            allocate (flags(0))
            call push(flags, "commit")
            call push(flags, "-m")
            call push(flags, msg)
            do k = 1, size(extracted)
                call push(flags, extracted(k)%s)
            end do
            ! When operating on the whole tree, better pass
            ! "-a" than ".", since "."  fails when we're
            ! committing a merge.
            if (only) then
                call push(flags, "--only")
                call push(flags, "--")
                fl = files
            else
                call push(flags, "-a")
                allocate (fl(0))
            end if
            call vc_git_command(0, fl, flags, status)
        end if
        call set_buffer_directory(saved)
    end subroutine vc_git_checkin_1

    !> Make the Git backend known to VC.
    subroutine syms_of_vc_git()
        integer :: k
        if (.not. allocated(vc_git_program)) vc_git_program = "git"
        call define_log_edit_mode()
        call register_vc_backend("Git", k)
        associate (b => vc_backends(k))
            b%checkin => vc_git_checkin
            b%log_edit_mode => vc_git_log_edit_mode
            b%registered => vc_git_registered
            b%state => vc_git_state
            b%working_revision => vc_git_working_revision
            b%working_revision_symbol => vc_git_working_revision_symbol
            b%mode_line_string => vc_git_mode_line_string
            b%root => vc_git_root
            ! (defalias 'vc-git-responsible-p #'vc-git-root)
            b%responsible_p => vc_git_root
            b%find_ignore_file => vc_git_find_ignore_file
            b%checkout_model => vc_git_checkout_model
            b%register => vc_git_register
            b%unregister => vc_git_unregister
            b%revert => vc_git_revert
            b%revert_files => vc_git_revert_files
            b%diff => vc_git_diff
            b%find_revision => vc_git_find_revision
            b%checkout => vc_git_checkout
            b%delete_file => vc_git_delete_file
            b%delete_files => vc_git_delete_files
            b%rename_file => vc_git_rename_file
            b%previous_revision => vc_git_previous_revision
            b%revision_completion_table => vc_git_revision_table
            b%mark_resolved => vc_git_mark_resolved
            b%create_repo => vc_git_create_repo
            b%diff_switches => vc_git_diff_switches
        end associate
    end subroutine syms_of_vc_git
end module vc_git_fm
