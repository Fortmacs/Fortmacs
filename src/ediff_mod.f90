! ediff_mod.f90 -- Ediff: a visual interface to diff.
! Ported from GNU Emacs 31.1 lisp/vc/ediff.el (in part: comparing two and
! three files or buffers).
!
! Copyright (C) 1994-2026 Free Software Foundation, Inc.
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

!> The Ediff commands that start a session: `ediff_files' (`ediff'),
!> `ediff_files3' (`ediff3'), `ediff_buffers' (`ebuffers'),
!> `ediff_buffers3' (`ebuffers3'), `ediff_current_file' and
!> `ediff_backup'.  Files are visited, buffers written to temporary
!> files, and `set_up_ediff' does the rest.
module ediff_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type, set_error, format_message, FM_GENERIC_ERROR, FM_USER_ERROR
    use handle_mod, only: handle_type, null_handle
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_buffer, get_or_make_buffer, is_buffer_live, &
                          close_buffer, is_buffer_modified
    use editfns_mod, only: widen
    use hooks_mod, only: hook_type
    use value_mod, only: command_type
    use commands_mod, only: commands
    use derived_mod, only: major_mode_type, major_mode_named
    use fileio_mod, only: expand_file_name, file_name_directory, file_name_nondirectory, is_file_directory, &
                          is_file_readable, file_exists, insert_file_contents, is_visited_file_modtime_current
    use files_mod, only: find_file_noselect, abbreviate_file_name, is_backup_file_name, file_name_sans_versions, &
                         revert_buffer_function, revert_function_procedure, delete_file
    use diff_mod, only: diff_latest_backup_file
    use minibuffer_mod, only: read_file_name, read_buffer, REQUIRE_MATCH_YES
    use subr_mod, only: add_to_history
    use xdisp_mod, only: message
    use ediff_init_mod, only: init_ediff_init
    use ediff_util_mod, only: set_up_ediff, read_ediff_file_name, create_ediff_temp_file, verify_ediff_file_buffer, &
                              ediff_other_buffer, init_ediff_util
    use ediff_wind_mod, only: init_ediff_wind
    use ediff_diff_mod, only: init_ediff_diff
    implicit none(type, external)
    private

    public :: ediff_files, ediff_files3, ediff_buffers, ediff_buffers3, ediff_current_file, ediff_backup, &
              find_ediff_file, get_ediff_default_file_name, init_ediff

    !> If .true., Ediff will use previous directory as default when
    !> reading file name.  Port of ediff.el ediff-use-last-dir.
    logical, public :: ediff_use_last_dir = .false.

    !> Last directory used by an Ediff command for file-A.  Port of
    !> ediff.el ediff-last-dir-A.
    character(len=:), allocatable, public :: ediff_last_dir_a
    !> Last directory used by an Ediff command for file-B.  Port of
    !> ediff.el ediff-last-dir-B.
    character(len=:), allocatable, public :: ediff_last_dir_b
    !> Last directory used by an Ediff command for file-C.  Port of
    !> ediff.el ediff-last-dir-C.
    character(len=:), allocatable, public :: ediff_last_dir_c
    !> Last directory used by an Ediff command for the ancestor file.
    !> Port of ediff.el ediff-last-dir-ancestor.
    character(len=:), allocatable, public :: ediff_last_dir_ancestor

    !> Which of the last directories `find_ediff_file' records
    !> (upstream's LAST-DIR symbol).
    integer, parameter :: LAST_DIR_NONE = 0, LAST_DIR_A = 1, LAST_DIR_B = 2, LAST_DIR_C = 3, LAST_DIR_ANCESTOR = 4

contains

    !> A plausible default for ediff's first file: DEFAULT if given, else
    !> the name of the file the buffer visits (without its directory); ""
    !> if none.  Port of ediff.el:ediff-get-default-file-name (the Dired
    !> case, with FILENO, is deferred).
    function get_ediff_default_file_name(default) result(name)
        character(len=*), intent(in), optional :: default
        character(len=:), allocatable :: name

        name = ""

        if (present(default)) then
            name = default
        else if (current > 0) then
            if (allocated(buffers(current)%filename)) name = file_name_nondirectory(buffers(current)%filename)
        end if
    end function get_ediff_default_file_name

    !> Run Ediff on two files, FILE_A and FILE_B.
    !> STARTUP_HOOKS holds functions that Fortmacs calls without arguments
    !> after setting up the Ediff buffers.  Port of ediff.el:ediff-files.
    recursive subroutine ediff_files(file_a, file_b, status, startup_hooks)
        character(len=*), intent(in) :: file_a, file_b
        type(status_type), intent(out) :: status
        type(hook_type), intent(inout), optional :: startup_hooks
        character(len=:), allocatable :: fb

        call file_in_directory(file_a, file_b, fb, status)
        if (.not. status%ok()) return
        call ediff_files_internal(file_a, fb, "", commands%ediff_files, status, startup_hooks)
    end subroutine ediff_files

    !> FILE, or when it is a directory, the file named like REFERENCE in
    !> it.  Not a port: (if (file-directory-p FILE) (expand-file-name
    !> (file-name-nondirectory REFERENCE) FILE) FILE).
    subroutine file_in_directory(reference, file, result, status)
        character(len=*), intent(in) :: reference, file
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status

        status = status_type()

        if (is_file_directory(file)) then
            call expand_file_name(file_name_nondirectory(reference), result, status, file)
        else
            result = file
        end if
    end subroutine file_in_directory

    !> Run Ediff on three files, FILE_A, FILE_B, and FILE_C.
    !> STARTUP_HOOKS holds functions that Fortmacs calls without arguments
    !> after setting up the Ediff buffers.  Port of ediff.el:ediff-files3.
    recursive subroutine ediff_files3(file_a, file_b, file_c, status, startup_hooks)
        character(len=*), intent(in) :: file_a, file_b, file_c
        type(status_type), intent(out) :: status
        type(hook_type), intent(inout), optional :: startup_hooks
        character(len=:), allocatable :: fb, fc

        call file_in_directory(file_a, file_b, fb, status)
        if (.not. status%ok()) return
        call file_in_directory(file_a, file_c, fc, status)
        if (.not. status%ok()) return
        call ediff_files_internal(file_a, fb, fc, commands%ediff_files3, status, startup_hooks)
    end subroutine ediff_files3

    !> Visit FILE and arrange its buffer to Ediff's liking.
    !> FILE is the file name.
    !> LAST_DIR is the last directory (A, B, C or the ancestor's) where
    !> FILE's directory name should be recorded.
    !> BUFFER is the buffer into which the file is visited.  Port of
    !> ediff.el:ediff-find-file (compressed and remote files, which need
    !> temporary copies, are deferred with file name handlers).
    recursive subroutine find_ediff_file(file_in, last_dir, buffer, status)
        character(len=*), intent(in) :: file_in
        integer, intent(in) :: last_dir
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file, dir
        type(handle_type) :: old
        type(status_type) :: st

        status = status_type()

        if (.not. is_file_readable(file_in)) then
            call set_error(status, FM_USER_ERROR, format_message("File `")//file_in// &
                           format_message("' does not exist or is not readable"))
            return
        else if (is_file_directory(file_in)) then
            call set_error(status, FM_USER_ERROR, format_message("File `")//file_in//format_message("' is a directory"))
            return
        end if

        ! Some of the commands, below, require full file name.
        call expand_file_name(file_in, file, status)
        if (.not. status%ok()) return
        ! Record the directory of the file.
        call expand_file_name(file_name_directory(file), dir, status)
        if (.not. status%ok()) return

        select case (last_dir)
        case (LAST_DIR_A)
            ediff_last_dir_a = dir
        case (LAST_DIR_B)
            ediff_last_dir_b = dir
        case (LAST_DIR_C)
            ediff_last_dir_c = dir
        case (LAST_DIR_ANCESTOR)
            ediff_last_dir_ancestor = dir
        case default
            continue
        end select

        ! Setup the buffer.
        call find_file_noselect(file, buffer, status)
        if (.not. status%ok()) return
        old = current_buffer()
        call set_buffer(buffer, status)
        if (.not. status%ok()) return
        ! Make sure the entire file is seen.
        call widen()
        ! Plain file---just check that the file matches the buffer.
        call verify_ediff_file_buffer(status)

        ! Make the old buffer current again, keeping the first error.
        if (is_buffer_live(old)) then
            call set_buffer(old, st)
            if (status%ok()) status = st
        end if
    end subroutine find_ediff_file

    !> Visit the files and set up the session.  Port of
    !> ediff.el:ediff-files-internal (MERGE-BUFFER-FILE and the merge
    !> jobs' ancestor directory are Deferred with merging).
    recursive subroutine ediff_files_internal(file_a, file_b, file_c, job_name, status, startup_hooks)
        character(len=*), intent(in) :: file_a, file_b, file_c
        type(command_type), pointer, intent(in) :: job_name
        type(status_type), intent(out) :: status
        type(hook_type), intent(inout), optional :: startup_hooks
        type(handle_type) :: buffer_a, buffer_b, buffer_c, control

        status = status_type()

        if (file_a == file_b) then
            call set_error(status, FM_GENERIC_ERROR, "Files A and B are the same")
            return
        end if

        if (len(file_c) > 0) then
            if (file_a == file_c) then
                call set_error(status, FM_GENERIC_ERROR, "Files A and C are the same")
                return
            else if (file_b == file_c) then
                call set_error(status, FM_GENERIC_ERROR, "Files B and C are the same")
                return
            end if
        end if

        call message("Reading file "//file_a//" ... ")
        call find_ediff_file(file_a, LAST_DIR_A, buffer_a, status)
        if (.not. status%ok()) return
        call message("Reading file "//file_b//" ... ")
        call find_ediff_file(file_b, LAST_DIR_B, buffer_b, status)
        if (.not. status%ok()) return
        buffer_c = null_handle()

        if (len(file_c) > 0) then
            call message("Reading file "//file_c//" ... ")
            call find_ediff_file(file_c, LAST_DIR_C, buffer_c, status)
            if (.not. status%ok()) return
        end if

        call set_up_ediff(buffer_a, buffer_file_name(buffer_a), buffer_b, buffer_file_name(buffer_b), buffer_c, &
                          buffer_file_name(buffer_c), &
                          job_name, control, status, startup_hooks)
    end subroutine ediff_files_internal

    !> The file BUFFER visits, "" if none.  Port of
    !> ediff.el:ediff--buffer-file-name (no magic file names).
    function buffer_file_name(buffer) result(name)
        type(handle_type), intent(in) :: buffer
        character(len=:), allocatable :: name

        name = ""
        if (.not. is_buffer_live(buffer)) return
        if (allocated(buffers(buffer%index)%filename)) name = buffers(buffer%index)%filename
    end function buffer_file_name

    !> Start ediff between current buffer and its file on disk.
    !> This command can be used instead of `revert_buffer'.  If there is
    !> nothing to revert then this command fails.
    !>
    !> Non-interactively, STARTUP_HOOKS holds functions that Fortmacs calls
    !> without arguments after setting up the Ediff buffers.  Port of
    !> ediff.el:ediff-current-file (auto-save files are deferred with
    !> auto-saving).
    recursive subroutine ediff_current_file(status, startup_hooks)
        type(status_type), intent(out) :: status
        type(hook_type), intent(inout), optional :: startup_hooks
        procedure(revert_function_procedure), pointer :: fn
        type(major_mode_type), pointer :: current_major
        type(handle_type) :: this, saved_file_buffer
        type(status_type) :: st
        character(len=:), allocatable :: file_name, saved_file_name, major
        integer(int64) :: inserted
        logical :: modified, revertible

        status = status_type()
        this = current_buffer()
        fn => revert_buffer_function%current()
        revertible = associated(fn)

        if (.not. revertible .and. allocated(buffers(current)%filename)) then
            if (file_exists(buffers(current)%filename)) then
                call is_buffer_modified(this, modified, status)
                if (.not. status%ok()) return
                revertible = modified .or. .not. is_visited_file_modtime_current()
            end if
        end if

        if (.not. revertible) then
            call set_error(status, FM_GENERIC_ERROR, "Nothing to revert")
            return
        end if

        if (.not. allocated(buffers(current)%filename)) then
            call set_error(status, FM_GENERIC_ERROR, "Buffer does not seem to be associated with any file")
            return
        end if

        file_name = buffers(current)%filename
        saved_file_name = "FILE="//file_name
        major = buffers(current)%major_mode
        saved_file_buffer = get_buffer(saved_file_name)

        if (is_buffer_live(saved_file_buffer)) then
            call close_buffer(saved_file_buffer, status)
            if (.not. status%ok()) return
        end if

        call get_or_make_buffer(saved_file_name, saved_file_buffer, status)
        if (.not. status%ok()) return
        call set_buffer(saved_file_buffer, status)
        if (.not. status%ok()) return
        call insert_file_contents(file_name, .false., inserted, status)

        if (status%ok()) then
            ! Assume same modes.
            current_major => major_mode_named(major)

            if (associated(current_major)) then
                if (associated(current_major%execute)) call current_major%execute(status)
            end if
        end if

        ! Make the old buffer current again, keeping the first error.
        call set_buffer(this, st)
        if (status%ok()) status = st
        if (.not. status%ok()) return
        call ediff_buffers(saved_file_buffer, this, status, startup_hooks)
    end subroutine ediff_current_file

    !> Run Ediff on FILE and its backup file.
    !> Uses the latest backup, if there are several numerical backups.
    !> If this file is a backup, `ediff' it with its original.  Port of
    !> ediff.el:ediff-backup.
    recursive subroutine ediff_backup(file, status)
        character(len=*), intent(in) :: file
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: bak, ori
        logical :: found

        ! The code is taken from `diff_backup'.
        if (is_backup_file_name(file)) then
            bak = file
            ori = file_name_sans_versions(file)
        else
            call diff_latest_backup_file(file, bak, found)

            if (.not. found) then
                call set_error(status, FM_GENERIC_ERROR, "No backup found for "//file)
                return
            end if

            ori = file
        end if

        call ediff_files(bak, ori, status)
    end subroutine ediff_backup

    !> Run Ediff on two buffers, BUFFER_A and BUFFER_B.
    !> STARTUP_HOOKS holds functions that Fortmacs calls without arguments
    !> after setting up the Ediff buffers.  JOB_NAME is the Ediff job type
    !> (a command); it defaults to commands%ediff_buffers.  Port of
    !> ediff.el:ediff-buffers.
    recursive subroutine ediff_buffers(buffer_a, buffer_b, status, startup_hooks, job_name)
        type(handle_type), intent(in) :: buffer_a, buffer_b
        type(status_type), intent(out) :: status
        type(hook_type), intent(inout), optional :: startup_hooks
        type(command_type), pointer, intent(in), optional :: job_name
        type(command_type), pointer :: job

        job => commands%ediff_buffers
        if (present(job_name)) job => job_name
        call ediff_buffers_internal(buffer_a, buffer_b, null_handle(), job, status, startup_hooks)
    end subroutine ediff_buffers

    !> Run Ediff on three buffers, BUFFER_A, BUFFER_B, and BUFFER_C.
    !> STARTUP_HOOKS holds functions that Fortmacs calls without arguments
    !> after setting up the Ediff buffers.  JOB_NAME is the Ediff job type
    !> (a command); it defaults to commands%ediff_buffers3.  Port of
    !> ediff.el:ediff-buffers3.
    recursive subroutine ediff_buffers3(buffer_a, buffer_b, buffer_c, status, startup_hooks, job_name)
        type(handle_type), intent(in) :: buffer_a, buffer_b, buffer_c
        type(status_type), intent(out) :: status
        type(hook_type), intent(inout), optional :: startup_hooks
        type(command_type), pointer, intent(in), optional :: job_name
        type(command_type), pointer :: job

        job => commands%ediff_buffers3
        if (present(job_name)) job => job_name
        call ediff_buffers_internal(buffer_a, buffer_b, buffer_c, job, status, startup_hooks)
    end subroutine ediff_buffers3

    !> Write the buffers to temporary files, set up the session and delete
    !> the files.  Port of ediff.el:ediff-buffers-internal (the files are
    !> deleted after the setup, as upstream deletes them on the way out;
    !> MERGE-BUFFER-FILE is deferred with merging).
    recursive subroutine ediff_buffers_internal(buffer_a, buffer_b, buffer_c, job_name, status, startup_hooks)
        type(handle_type), intent(in) :: buffer_a, buffer_b, buffer_c
        type(command_type), pointer, intent(in) :: job_name
        type(status_type), intent(out) :: status
        type(hook_type), intent(inout), optional :: startup_hooks
        character(len=:), allocatable :: file_a, file_b, file_c
        type(handle_type) :: control
        type(status_type) :: st
        logical :: buffer_c_is_alive, three_way_comparison_job

        status = status_type()
        buffer_c_is_alive = is_buffer_live(buffer_c)
        file_c = ""

        if (.not. is_buffer_live(buffer_a)) then
            call set_error(status, FM_USER_ERROR, "Buffer #<killed buffer> doesn't exist")
            return
        else if (.not. is_buffer_live(buffer_b)) then
            call set_error(status, FM_USER_ERROR, "Buffer #<killed buffer> doesn't exist")
            return
        end if

        three_way_comparison_job = associated(job_name, commands%ediff_files3) .or. &
                                   associated(job_name, commands%ediff_buffers3)

        if (three_way_comparison_job .and. .not. buffer_c_is_alive) then
            call set_error(status, FM_USER_ERROR, "Buffer #<killed buffer> doesn't exist")
            return
        end if

        call make_temp(buffer_a, file_a)
        if (status%ok()) call make_temp(buffer_b, file_b)
        if (status%ok() .and. buffer_c_is_alive) call make_temp(buffer_c, file_c)

        if (status%ok()) then
            call set_up_ediff(buffer_a, file_a, buffer_b, file_b, buffer_c, file_c, job_name, control, status, startup_hooks)
        end if

        ! Delete the temporary files, keeping the first error.
        if (allocated(file_a)) call delete_temp(file_a)
        if (allocated(file_b)) call delete_temp(file_b)
        if (len(file_c) > 0) call delete_temp(file_c)

    contains

        ! A temporary file with the text of BUFFER, named after its file.
        subroutine make_temp(buffer, file)
            type(handle_type), intent(in) :: buffer
            character(len=:), allocatable, intent(out) :: file

            if (allocated(buffers(buffer%index)%filename)) then
                call create_ediff_temp_file(buffer, file, status, file_name_nondirectory(buffers(buffer%index)%filename))
            else
                call create_ediff_temp_file(buffer, file, status)
            end if
        end subroutine make_temp

        ! Delete FILE if it exists; its error goes to STATUS unless an
        ! error is already recorded there.
        subroutine delete_temp(file)
            character(len=*), intent(in) :: file

            if (.not. file_exists(file)) return
            call delete_file(file, st)
            if (status%ok()) status = st
        end subroutine delete_temp
    end subroutine ediff_buffers_internal

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    ! The default directory of reading file A: the last one, or
    ! `default_directory'.  Not a port.
    function directory_a() result(dir)
        character(len=:), allocatable :: dir

        dir = ""

        if (ediff_use_last_dir) then
            if (allocated(ediff_last_dir_a)) dir = ediff_last_dir_a
        else if (allocated(buffers(current)%directory)) then
            dir = buffers(current)%directory
        end if
    end function directory_a

    ! Read the file after FILE: in DIR_LAST (the last one) or the
    ! directory of FILE, adding FILE's name in it to the file name history.
    ! Not a port.
    recursive subroutine read_next_file(prompt, file, dir_last, result, status)
        character(len=*), intent(in) :: prompt, file
        character(len=:), allocatable, intent(in) :: dir_last
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: dir, expanded

        if (ediff_use_last_dir) then
            dir = ""
            if (allocated(dir_last)) dir = dir_last
        else
            dir = file_name_directory(file)
        end if

        call expand_file_name(file_name_nondirectory(file), expanded, status, dir)
        if (.not. status%ok()) return
        call add_to_history("file_name_history", abbreviate_file_name(expanded), status=status)
        if (.not. status%ok()) return
        call read_ediff_file_name(prompt, dir, get_ediff_default_file_name(file), result, status)
    end subroutine read_next_file

    !> Run Ediff on two files, FILE_A and FILE_B.  Port of
    !> ediff.el:ediff-files (the command; also `ediff').
    recursive subroutine cmd_ediff_files(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: f, ff

        call read_ediff_file_name("File A to compare", directory_a(), get_ediff_default_file_name(), f, status, .true.)
        if (.not. status%ok()) return
        call read_next_file("File B to compare", f, ediff_last_dir_b, ff, status)
        if (.not. status%ok()) return
        call ediff_files(f, ff, status)
    end subroutine cmd_ediff_files

    !> Run Ediff on three files, FILE_A, FILE_B, and FILE_C.  Port of
    !> ediff.el:ediff-files3 (the command; also `ediff3').
    recursive subroutine cmd_ediff_files3(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: f, ff, fff

        call read_ediff_file_name("File A to compare", directory_a(), get_ediff_default_file_name(), f, status, .true.)
        if (.not. status%ok()) return
        call read_next_file("File B to compare", f, ediff_last_dir_b, ff, status)
        if (.not. status%ok()) return
        call read_next_file("File C to compare", ff, ediff_last_dir_c, fff, status)
        if (.not. status%ok()) return
        call ediff_files3(f, ff, fff, status)
    end subroutine cmd_ediff_files3

    recursive subroutine cmd_ediff_current_file(status)
        type(status_type), intent(out) :: status

        call ediff_current_file(status)
    end subroutine cmd_ediff_current_file

    recursive subroutine cmd_ediff_backup(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file

        call read_file_name("Ediff (file with backup): ", file, status)
        if (.not. status%ok()) return
        call ediff_backup(file, status)
    end subroutine cmd_ediff_backup

    !> Run Ediff on two buffers, BUFFER_A and BUFFER_B.  Port of
    !> ediff.el:ediff-buffers (the command; also `ebuffers').
    recursive subroutine cmd_ediff_buffers(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: bf, bff, default

        call ediff_other_buffer([character(len=1) :: ""], default)
        call read_buffer("Buffer A to compare: ", bf, status, def=default, require_match=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        call ediff_other_buffer([character(len=len(bf)) :: bf], default)
        call read_buffer("Buffer B to compare: ", bff, status, def=default, require_match=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        call ediff_buffers(get_buffer(bf), get_buffer(bff), status)
    end subroutine cmd_ediff_buffers

    !> Run Ediff on three buffers, BUFFER_A, BUFFER_B, and BUFFER_C.  Port
    !> of ediff.el:ediff-buffers3 (the command; also `ebuffers3').
    recursive subroutine cmd_ediff_buffers3(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: bf, bff, bfff, default

        call ediff_other_buffer([character(len=1) :: ""], default)
        call read_buffer("Buffer A to compare: ", bf, status, def=default, require_match=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        call ediff_other_buffer([character(len=len(bf)) :: bf], default)
        call read_buffer("Buffer B to compare: ", bff, status, def=default, require_match=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        call ediff_other_buffer([character(len=max(len(bf), len(bff))) :: bf, bff], default)
        call read_buffer("Buffer C to compare: ", bfff, status, def=default, require_match=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        call ediff_buffers3(get_buffer(bf), get_buffer(bff), get_buffer(bfff), status)
    end subroutine cmd_ediff_buffers3

    !> Register the commands of Ediff and set up its modules.  Not a port.
    subroutine init_ediff()
        if (associated(commands%ediff_files%execute)) return
        call init_ediff_init()
        call init_ediff_wind()
        call init_ediff_diff()
        call init_ediff_util()
        commands%ediff_files%execute => cmd_ediff_files
        commands%ediff%execute => cmd_ediff_files
        commands%ediff_files3%execute => cmd_ediff_files3
        commands%ediff3%execute => cmd_ediff_files3
        commands%ediff_current_file%execute => cmd_ediff_current_file
        commands%ediff_backup%execute => cmd_ediff_backup
        commands%ediff_buffers%execute => cmd_ediff_buffers
        commands%ebuffers%execute => cmd_ediff_buffers
        commands%ediff_buffers3%execute => cmd_ediff_buffers3
        commands%ebuffers3%execute => cmd_ediff_buffers3
    end subroutine init_ediff
end module ediff_mod
