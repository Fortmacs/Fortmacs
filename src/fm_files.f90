! fm_files.f90 -- file handling commands.
! Ported from GNU Emacs 31.1 lisp/files.el.
!
! Copyright (C) 1985-1987, 1992-2026 Free Software Foundation, Inc.
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

!> Visiting files, saving buffers with backups, and leaving Emacs.
!>
!> Not yet: the minibuffer (so no C-x C-f, and a buffer with no file
!> can't be saved), `yes-or-no-p` (`y-or-n-p` asks instead), auto-save,
!> file locks, major modes and local variables, uniquify (buffer names
!> get "<2>"), version-control backups and `backup-directory-alist`, and
!> `file-precious-flag`.
module fm_files
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use fm_error, only: status_type, set_error, condition_matches, FM_GENERIC_ERROR, &
                        FM_USER_ERROR, FM_FILE_ERROR
    use fm_handle, only: handle_type
    use fm_lisp, only: value_type, make_fixnum, nilp
    use fm_lread, only: sym
    use fm_buffer, only: buffers, cur, current_buffer, resolve_buffer, set_buffer, &
                         get_buffer_create, generate_new_buffer_name, get_file_buffer, &
                         buffer_list, kill_buffer, erase_buffer, set_buffer_read_only
    use fm_fileio, only: file_stat_type, expand_file_name, file_name_directory, &
                         file_name_nondirectory, directory_file_name, file_name_as_directory, &
                         get_homedir, file_exists_p, file_readable_p, file_writable_p, &
                         file_directory_p, file_accessible_directory_p, file_attributes, &
                         file_modes, set_file_modes, rename_file, copy_file, &
                         insert_file_contents, write_region, verify_visited_file_modtime, &
                         file_regular_p, file_symlink_p, user_uid, group_gid
    use fm_keymap, only: define_key
    use fm_callint, only: defcommand, current_prefix_arg, prefix_numeric_value
    use fm_keyboard, only: request_exit
    use fm_xdisp, only: message
    use fm_subr, only: y_or_n_p
    use fm_map_ynp, only: map_y_or_n_p, YNP_SKIP, YNP_ASK
    implicit none(type, external)
    private

    public :: syms_of_files
    public :: find_file_noselect, find_file, create_file_buffer, abbreviate_file_name
    public :: save_buffer, basic_save_buffer, backup_buffer, make_backup_file_name, &
              normal_backup_enable_predicate, file_ownership_preserved_p
    public :: save_some_buffers, save_buffers_kill_emacs, temporary_file_directory

    !> files.el make-backup-files.
    logical, public :: make_backup_files = .true.

    !> Buffers save_some_buffers is asking about (its map-y-or-n-p list).
    type(handle_type), allocatable :: pending(:)

contains

    ! ------------------------------------------------------------------
    ! Visiting
    ! ------------------------------------------------------------------

    !> `filename` with the home directory replaced by "~".  Port of
    !> files.el:abbreviate-file-name (no directory-abbrev-alist).
    function abbreviate_file_name(filename) result(abbrev)
        character(len=*), intent(in) :: filename
        character(len=:), allocatable :: abbrev
        character(len=:), allocatable :: home
        integer :: n

        abbrev = filename
        home = directory_file_name(get_homedir())
        n = len(home)
        ! Don't abbreviate a home directory of "/" (abbreviated-home-dir).
        if (n <= 1) return
        if (len(filename) < n) return
        if (filename(1:n) /= home) return
        if (len(filename) == n) then
            abbrev = "~"
        else if (filename(n + 1:n + 1) == "/") then
            abbrev = "~"//filename(n + 1:)
        end if
    end function abbreviate_file_name

    !> A new buffer named after `filename` (sans directory), made unique
    !> with "<N>"; a leading space gets a "|" before it.
    !> Port of files.el:create-file-buffer (no uniquify).
    subroutine create_file_buffer(filename, h, status)
        character(len=*), intent(in) :: filename
        type(handle_type), intent(out) :: h
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: lastname

        lastname = file_name_nondirectory(directory_file_name(filename))
        if (len(lastname) == 0) lastname = filename
        if (lastname(1:1) == " ") lastname = "|"//lastname
        call get_buffer_create(generate_new_buffer_name(lastname), h, status)
    end subroutine create_file_buffer

    !> The buffer visiting `filename`, reading the file into a new buffer
    !> if none does yet.  The buffer is not selected.  A file that doesn't
    !> exist still gets a buffer, which will create it when saved.
    !> Port of files.el:find-file-noselect with find-file-noselect-1 (no
    !> wildcards, truenames, raw files, or checks that an already-visited
    !> file changed on disk).
    subroutine find_file_noselect(filename, buf, status)
        character(len=*), intent(in) :: filename
        type(handle_type), intent(out) :: buf
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: absname
        type(handle_type) :: old
        type(status_type) :: st
        integer(int64) :: inserted
        logical :: error

        call expand_file_name(filename, absname, status)
        if (.not. status%ok()) return
        if (file_directory_p(absname)) then
            call set_error(status, FM_GENERIC_ERROR, abbreviate_file_name(absname)//" is a directory")
            return
        end if
        buf = get_file_buffer(absname)
        if (buf%index /= 0) return

        call create_file_buffer(absname, buf, status)
        if (.not. status%ok()) return
        old = current_buffer()
        call set_buffer(buf, status)
        if (.not. status%ok()) return
        ! find-file-noselect-1
        call erase_buffer(status)
        if (.not. status%ok()) return
        error = .false.
        call insert_file_contents(absname, .true., inserted, status)
        if (.not. status%ok()) then
            if (.not. condition_matches(status%code, FM_FILE_ERROR)) then
                call set_buffer(old, st)
                return
            end if
            if (file_exists_p(absname)) then
                ! The file is there but couldn't be read.
                call set_buffer(old, st)
                call kill_buffer(buf, st)
                buf = handle_type()
                if (.not. file_readable_p(absname)) then
                    status = status_type()
                    call set_error(status, FM_FILE_ERROR, "File is not readable: "//absname)
                end if
                return
            end if
            status = status_type()
            error = .true.
        end if
        buffers(cur)%directory = file_name_directory(buffers(cur)%filename)
        call after_find_file(error, .true.)
        call set_buffer(old, status)
    end subroutine find_file_noselect

    !> Set up the current buffer after visiting its file: read-only if the
    !> file can't be written, and a message about new or protected files
    !> when `warn`.  Port of files.el:after-find-file (no auto-save, modes,
    !> local variables or hooks).
    subroutine after_find_file(error, warn)
        logical, intent(in) :: error, warn
        character(len=:), allocatable :: msg, file
        integer(int32) :: modes
        type(status_type) :: st

        file = buffers(cur)%filename
        buffers(cur)%read_only = .not. file_writable_p(file)
        msg = ""
        if (error .and. file_exists_p(file)) then
            buffers(cur)%read_only = .true.
            msg = "File exists, but cannot be read"
        else if (error .and. file_symlink_p(file)) then
            msg = "Symbolic link that points to nonexistent file"
        else if (.not. buffers(cur)%read_only) then
            if (error) msg = "(New file)"
        else if (.not. error) then
            msg = "Note: file is write protected"
        else if (file_accessible_directory_p(buffers(cur)%directory)) then
            msg = "File not found and directory write-protected"
        else
            buffers(cur)%read_only = .false.
            if (.not. file_directory_p(buffers(cur)%directory)) then
                msg = "Use M-x make-directory RET RET to create the directory and its parents"
            end if
        end if
        if (warn .and. len(msg) > 0) call message(msg)
        ! A backup file, or one with no write bits, starts read-only.
        if (.not. buffers(cur)%read_only) then
            modes = file_modes(file)
            if (backup_file_name_p(file) .or. (modes >= 0 .and. iand(modes, int(o'222', int32)) == 0)) then
                call set_buffer_read_only(current_buffer(), .true., st)
            end if
        end if
    end subroutine after_find_file

    !> Visit `filename` and make its buffer current (and so shown in the
    !> selected window).  Port of files.el:find-file for programs (the
    !> interactive file name prompt needs the minibuffer).
    subroutine find_file(filename, status)
        character(len=*), intent(in) :: filename
        type(status_type), intent(out) :: status
        type(handle_type) :: buf

        call find_file_noselect(filename, buf, status)
        if (.not. status%ok()) return
        call set_buffer(buf, status)
    end subroutine find_file

    ! ------------------------------------------------------------------
    ! Backups
    ! ------------------------------------------------------------------

    !> Whether `file` is a backup file name (ends in "~").  Port of
    !> files.el:backup-file-name-p (without numbered versions).
    pure logical function backup_file_name_p(file)
        character(len=*), intent(in) :: file

        backup_file_name_p = .false.
        if (len(file) > 0) backup_file_name_p = file(len(file):len(file)) == "~"
    end function backup_file_name_p

    !> The backup name of `file`: FILE~.  Port of
    !> files.el:make-backup-file-name (backup-directory-alist nil).
    pure function make_backup_file_name(file) result(backup)
        character(len=*), intent(in) :: file
        character(len=:), allocatable :: backup

        backup = file//"~"
    end function make_backup_file_name

    !> The temporary directory, as a directory name: $TMPDIR, else "/tmp".
    !> (callproc.c:init_callproc sets temporary-file-directory.)
    function temporary_file_directory() result(dir)
        character(len=:), allocatable :: dir
        integer :: length, stat

        call get_environment_variable("TMPDIR", length=length, status=stat)
        if (stat == 0 .and. length > 0) then
            allocate (character(len=length) :: dir)
            call get_environment_variable("TMPDIR", dir)
        else
            dir = "/tmp"
        end if
        dir = file_name_as_directory(dir)
    end function temporary_file_directory

    !> Whether to back up `name`: not for files under the temporary
    !> directory or "/tmp".  Port of files.el:normal-backup-enable-predicate.
    logical function normal_backup_enable_predicate(name)
        character(len=*), intent(in) :: name

        normal_backup_enable_predicate = .not. (under(temporary_file_directory()) .or. under("/tmp"))
    contains
        !> compare-strings says `name` extends `dir`.
        logical function under(dir)
            character(len=*), intent(in) :: dir

            under = len(name) > len(dir)
            if (under) under = name(1:len(dir)) == dir
        end function under
    end function normal_backup_enable_predicate

    !> Whether deleting and rewriting `file` keeps its owner (and with
    !> `group`, its group); true if it doesn't exist.
    !> Port of files.el:file-ownership-preserved-p.
    logical function file_ownership_preserved_p(file, group)
        character(len=*), intent(in) :: file
        logical, intent(in) :: group
        type(file_stat_type) :: attr, parent
        character(len=:), allocatable :: dir
        logical :: found

        call file_attributes(file, attr, found)
        file_ownership_preserved_p = .true.
        if (.not. found) return
        file_ownership_preserved_p = .false.
        if (attr%uid /= user_uid()) return
        if (group .and. attr%gid /= group_gid()) return
        dir = file_name_directory(file)
        if (len(dir) == 0) dir = "."
        call file_attributes(dir, parent, found)
        if (.not. found) return
        ! A file made in a setuid directory may take its owner, and one
        ! made in a setgid directory its group.
        if (.not. (parent%uid == user_uid() .or. iand(parent%mode, int(o'4000', int32)) == 0)) return
        if (group .and. parent%gid /= attr%gid) return
        file_ownership_preserved_p = .true.
    end function file_ownership_preserved_p

    !> Back up the current buffer's file before its first save, if
    !> appropriate, by renaming it to FILE~ or (to keep its owner, links
    !> or special bits) by copying it there.  After a rename, `setmodes`
    !> is the old file's permission bits, to give the new file; otherwise
    !> it is -1.  Port of files.el:backup-buffer (no numbered versions; a
    !> symbolic link is backed up by copying, beside the link).
    subroutine backup_buffer(setmodes)
        integer(int32), intent(out) :: setmodes
        character(len=:), allocatable :: file, backupname
        type(file_stat_type) :: attr
        type(status_type) :: status
        integer(int32) :: modes
        logical :: found, make_copy

        setmodes = -1
        if (.not. make_backup_files .or. buffers(cur)%backed_up) return
        if (.not. allocated(buffers(cur)%filename)) return
        file = buffers(cur)%filename
        ! backup-inhibited, set by find-file from backup-enable-predicate
        if (.not. normal_backup_enable_predicate(file)) return
        call file_attributes(file, attr, found)
        if (.not. found) return
        if (.not. file_regular_p(file)) return
        backupname = make_backup_file_name(file)
        modes = file_modes(file)
        make_copy = file_symlink_p(file) .or. &
                    (modes >= 0 .and. iand(modes, int(o'6000', int32)) /= 0) .or. &
                    .not. file_writable_p(file_name_directory(file)) .or. &
                    .not. file_ownership_preserved_p(file, .true.)
        if (make_copy) then
            call copy_file(file, backupname, status)
        else
            call rename_file(file, backupname, status)
            if (status%ok()) setmodes = modes
        end if
        ! A failed backup is ignored (condition-case file-error), and the
        ! fallback to %backup%~ in user-emacs-directory is not ported.
        if (status%ok()) then
            buffers(cur)%backed_up = .true.
        else
            call message("Cannot write backup file "//backupname)
        end if
    end subroutine backup_buffer

    ! ------------------------------------------------------------------
    ! Saving
    ! ------------------------------------------------------------------

    !> Save the current buffer in its visited file if modified.  `arg` is
    !> the numeric prefix: 4 (C-u) backs up the previous version on the
    !> next save, 16 (C-u C-u) now, 64 both; 0 never.
    !> Port of files.el:save-buffer.
    subroutine save_buffer(arg, called_interactively, status)
        integer(int64), intent(in) :: arg
        logical, intent(in) :: called_interactively
        type(status_type), intent(out) :: status
        logical :: modp, saved_make_backup_files

        modp = buffers(cur)%text%save_modiff < buffers(cur)%text%modiff
        saved_make_backup_files = make_backup_files
        make_backup_files = (make_backup_files .and. arg /= 0) .or. arg == 16 .or. arg == 64
        if (modp .and. (arg == 16 .or. arg == 64)) buffers(cur)%backed_up = .false.
        if (modp .and. allocated(buffers(cur)%filename)) then
            call message("Saving file "//buffers(cur)%filename//"...")
        end if
        call basic_save_buffer(called_interactively, status)
        make_backup_files = saved_make_backup_files
        if (.not. status%ok()) return
        if (modp .and. (arg == 4 .or. arg == 64)) buffers(cur)%backed_up = .false.
    end subroutine save_buffer

    !> Save the current buffer in its visited file, if it was modified or
    !> the file doesn't exist; otherwise say so when called interactively.
    !> Port of files.el:basic-save-buffer with basic-save-buffer-2 (no
    !> hooks, final newline, file-precious-flag or break-hardlink-on-save).
    subroutine basic_save_buffer(called_interactively, status)
        logical, intent(in) :: called_interactively
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file, dir, wrote
        integer(int32) :: setmodes
        logical :: modified, ok, tempsetmodes
        type(status_type) :: st

        modified = buffers(cur)%text%save_modiff < buffers(cur)%text%modiff
        if (.not. modified .and. allocated(buffers(cur)%filename)) then
            modified = .not. file_exists_p(buffers(cur)%filename)
        end if
        if (.not. modified) then
            if (called_interactively) call message("(No changes need to be saved)")
            return
        end if
        if (.not. allocated(buffers(cur)%filename)) then
            ! Upstream asks for a file name in the minibuffer.
            call set_error(status, FM_USER_ERROR, "Buffer "//buffers(cur)%name//" is not visiting a file")
            return
        end if
        file = buffers(cur)%filename

        if (.not. verify_visited_file_modtime() .and. file_exists_p(file)) then
            call y_or_n_p(file_name_nondirectory(file)//" has changed since visited or saved.  Save anyway? ", &
                          ok, status)
            if (.not. status%ok()) return
            if (.not. ok) then
                call set_error(status, FM_USER_ERROR, "Save not confirmed")
                return
            end if
        end if

        ! basic-save-buffer offers to create a missing directory
        ! (make-directory is not ported yet).
        dir = file_name_directory(file)
        if (.not. file_exists_p(file) .and. .not. file_exists_p(dir)) then
            call set_error(status, FM_GENERIC_ERROR, dir//": No such directory")
            return
        end if

        ! basic-save-buffer-2
        tempsetmodes = .false.
        if (.not. file_writable_p(file)) then
            if (.not. file_directory_p(dir)) then
                if (file_exists_p(dir)) then
                    call set_error(status, FM_GENERIC_ERROR, dir//" is not a directory")
                else
                    call set_error(status, FM_GENERIC_ERROR, dir//": No such directory")
                end if
                return
            end if
            if (.not. file_exists_p(file)) then
                call set_error(status, FM_GENERIC_ERROR, "Directory "//dir//" write-protected")
                return
            end if
            call y_or_n_p("File "//file_name_nondirectory(file)// &
                          " is write-protected; try to save anyway? ", ok, status)
            if (.not. status%ok()) return
            if (.not. ok) then
                call set_error(status, FM_GENERIC_ERROR, &
                               "Attempt to save to a file that you aren't allowed to write")
                return
            end if
            tempsetmodes = .true.
        end if
        call backup_buffer(setmodes)
        ! Make a write-protected file writable while writing it; its bits
        ! are restored below.
        if (tempsetmodes .and. setmodes < 0) then
            setmodes = file_modes(file)
            if (.not. file_writable_p(file)) call set_file_modes(file, ior(setmodes, int(o'200', int32)), st)
        end if
        call write_region(file, .true., status, message=wrote)
        if (setmodes >= 0) call set_file_modes(file, setmodes, st)
        if (.not. status%ok()) return
        call message(wrote)
    end subroutine basic_save_buffer

    !> map-y-or-n-p prompter for save_some_buffers.
    subroutine save_prompter(k, how, prompt)
        integer, intent(in) :: k
        integer, intent(out) :: how
        character(len=:), allocatable, intent(out) :: prompt
        integer(int64) :: i
        type(status_type) :: status

        how = YNP_SKIP
        prompt = ""
        call resolve_buffer(pending(k), i, status)
        if (.not. status%ok()) return
        how = YNP_ASK
        if (.not. allocated(buffers(i)%filename)) then
            prompt = "Save buffer "//buffers(i)%name//"? "
        else if (buffers(i)%name == file_name_nondirectory(buffers(i)%filename) .or. &
                 index(buffers(i)%name, file_name_nondirectory(buffers(i)%filename)//"<") == 1) then
            prompt = "Save file "//buffers(i)%filename//"? "
        else
            prompt = "Save file "//buffers(i)%filename//" (buffer "//buffers(i)%name//")? "
        end if
    end subroutine save_prompter

    !> map-y-or-n-p actor for save_some_buffers: save that buffer.
    subroutine save_actor(k, status)
        integer, intent(in) :: k
        type(status_type), intent(out) :: status
        type(handle_type) :: old
        type(status_type) :: st

        old = current_buffer()
        call set_buffer(pending(k), status)
        if (.not. status%ok()) return
        call save_buffer(1_int64, .false., status)
        call set_buffer(old, st)
    end subroutine save_actor

    !> Ask about saving each modified file-visiting buffer, then save
    !> those the user accepts (all, without asking, if `arg`).
    !> Port of files.el:save-some-buffers (PRED nil; no
    !> buffer-save-without-query, buffer-offer-save or
    !> save-some-buffers-functions).
    subroutine save_some_buffers(arg, called_interactively, status)
        logical, intent(in) :: arg, called_interactively
        type(status_type), intent(out) :: status
        type(handle_type), allocatable :: all(:)
        integer(int64) :: k, i, n
        integer :: done

        ! files--buffers-needing-to-be-saved
        all = buffer_list()
        allocate (pending(size(all)))
        n = 0
        do k = 1, size(all, kind=int64)
            call resolve_buffer(all(k), i, status)
            if (.not. status%ok()) cycle
            if (.not. allocated(buffers(i)%filename)) cycle
            if (buffers(i)%text%save_modiff >= buffers(i)%text%modiff) cycle
            n = n + 1
            pending(n) = all(k)
        end do
        status = status_type()
        done = 0
        if (arg) then
            do k = 1, n
                call save_actor(int(k), status)
                if (.not. status%ok()) exit
                done = done + 1
            end do
        else
            call map_y_or_n_p(int(n), save_prompter, save_actor, done, status)
        end if
        deallocate (pending)
        if (.not. status%ok()) return
        if (n == 0 .and. called_interactively) call message("(No files need saving)")
    end subroutine save_some_buffers

    !> Whether any live buffer visits a file and is modified.
    logical function modified_file_buffers_exist()
        type(handle_type), allocatable :: all(:)
        integer(int64) :: k, i
        type(status_type) :: status

        modified_file_buffers_exist = .false.
        all = buffer_list()
        do k = 1, size(all, kind=int64)
            call resolve_buffer(all(k), i, status)
            if (.not. status%ok()) cycle
            if (.not. allocated(buffers(i)%filename)) cycle
            if (buffers(i)%text%save_modiff < buffers(i)%text%modiff) then
                modified_file_buffers_exist = .true.
                return
            end if
        end do
    end function modified_file_buffers_exist

    !> Offer to save each modified file buffer, then leave Emacs, asking
    !> first if any stay unsaved.  Port of files.el:save-buffers-kill-emacs
    !> (no processes, kill-emacs-query-functions or confirm-kill-emacs).
    subroutine save_buffers_kill_emacs(arg, status)
        logical, intent(in) :: arg
        type(status_type), intent(out) :: status
        logical :: ok

        if (modified_file_buffers_exist()) then
            call save_some_buffers(arg, .false., status)
            if (.not. status%ok()) return
        end if
        if (modified_file_buffers_exist()) then
            ! Upstream uses yes-or-no-p, which needs the minibuffer.
            call y_or_n_p("Modified buffers exist; exit anyway? ", ok, status)
            if (.not. status%ok() .or. .not. ok) return
        end if
        call request_exit()
    end subroutine save_buffers_kill_emacs

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    !> C-x C-s.
    subroutine cmd_save_buffer(status)
        type(status_type), intent(out) :: status

        call save_buffer(prefix_numeric_value(current_prefix_arg), .true., status)
    end subroutine cmd_save_buffer

    !> C-x s.
    subroutine cmd_save_some_buffers(status)
        type(status_type), intent(out) :: status

        call save_some_buffers(.not. nilp(current_prefix_arg), .true., status)
    end subroutine cmd_save_some_buffers

    !> C-x C-c: leave Emacs.  Port of files.el:save-buffers-kill-terminal
    !> (no emacsclient frames).
    subroutine cmd_save_buffers_kill_terminal(status)
        type(status_type), intent(out) :: status

        call save_buffers_kill_emacs(.not. nilp(current_prefix_arg), status)
    end subroutine cmd_save_buffers_kill_terminal

    !> Register the commands and their keys in `ctl_x_map`.
    subroutine syms_of_files(ctl_x_map)
        type(handle_type), intent(in) :: ctl_x_map
        type(value_type) :: key(1)
        type(status_type) :: status

        call defcommand("save-buffer", cmd_save_buffer)
        call defcommand("save-some-buffers", cmd_save_some_buffers)
        call defcommand("save-buffers-kill-terminal", cmd_save_buffers_kill_terminal)
        ! files.el: (define-key ctl-x-map "\C-s" 'save-buffer)
        key(1) = make_fixnum(19_int64)
        call define_key(ctl_x_map, key, sym("save-buffer"), status)
        ! files.el: (define-key ctl-x-map "s" 'save-some-buffers)
        key(1) = make_fixnum(int(iachar("s"), int64))
        call define_key(ctl_x_map, key, sym("save-some-buffers"), status)
        ! files.el: (define-key ctl-x-map "\C-c" 'save-buffers-kill-terminal)
        key(1) = make_fixnum(3_int64)
        call define_key(ctl_x_map, key, sym("save-buffers-kill-terminal"), status)
    end subroutine syms_of_files

end module fm_files
