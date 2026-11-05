! fortmacs_diff_mod.f90 -- Ediff: a visual interface to diff.
! Ported from GNU Emacs 31.1 lisp/vc/ediff.el (in part: comparing two and
! three files or buffers, and merging them).
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
!> `ediff_buffers3' (`ebuffers3'), `ediff_current_file', `ediff_backup'
!> and the merge jobs `ediff_merge_files' (`ediff_merge'),
!> `ediff_merge_files_with_ancestor' (`ediff_merge_with_ancestor'),
!> `ediff_merge_buffers' and `ediff_merge_buffers_with_ancestor'.  Files
!> are visited, buffers written to temporary files, and `set_up_ediff'
!> does the rest.  The directory jobs (`ediff_directories' and the
!> others, in fortmacs_diff_directories_mod) make a session group of
!> fortmacs_diff_multiple_mod.
module fortmacs_diff_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use error_mod, only: status_type, set_error, format_message, GENERIC_ERROR_CODE, USER_ERROR_CODE
    use handle_mod, only: handle_type, null_handle
    use buffer_mod, only: buffers, current, current_buffer, set_buffer, get_buffer, get_or_make_buffer, is_buffer_live, &
                          close_buffer, is_buffer_modified
    use editing_functions_mod, only: widen
    use hooks_mod, only: hook_type
    use value_mod, only: command_type
    use commands_mod, only: commands
    use derived_mod, only: major_mode_type, major_mode_named
    use file_input_output_mod, only: expand_file_name, file_name_directory, file_name_nondirectory, is_file_directory, &
                                     is_file_readable, file_exists, insert_file_contents, is_visited_file_modtime_current
    use files_mod, only: find_file_noselect, abbreviate_file_name, is_backup_file_name, file_name_sans_versions, &
                         revert_buffer_function, revert_function_procedure, delete_file
    use diff_mod, only: diff_latest_backup_file
    use minibuffer_mod, only: read_file_name, read_buffer, REQUIRE_MATCH_YES
    use basic_subprograms_mod, only: add_to_history
    use display_engine_mod, only: message
    use fortmacs_diff_settings_mod, only: initialize_ediff_init, ediff_session_index
    use fortmacs_diff_utilities_mod, only: set_up_ediff, read_ediff_file_name, create_ediff_temporary_file, &
                                           verify_ediff_file_buffer, ediff_other_buffer, initialize_ediff_utility
    use fortmacs_diff_window_mod, only: initialize_ediff_wind
    use fortmacs_diff_diff_mod, only: initialize_ediff_diff
    use fortmacs_diff_merging_mod, only: do_ediff_merge, initialize_ediff_merg
    implicit none(type, external)
    private

    public :: ediff_files, ediff_files3, ediff_buffers, ediff_buffers3, ediff_current_file, ediff_backup, &
              ediff_merge_files, ediff_merge_files_with_ancestor, ediff_merge_buffers, ediff_merge_buffers_with_ancestor, &
              merge_ediff_on_startup, find_ediff_file, get_ediff_default_file_name, initialize_ediff, ediff_directories, &
              ediff_directories3, ediff_merge_directories, ediff_merge_directories_with_ancestor, ediff_directories_internal

    !> If .true., Ediff will use previous directory as default when
    !> reading file name.  Port of ediff.el ediff-use-last-dir.
    logical, public :: ediff_use_last_directory = .false.

    !> Last directory used by an Ediff command for file-A.  Port of
    !> ediff.el ediff-last-dir-A.
    character(len=:), allocatable, public :: ediff_last_directory_a
    !> Last directory used by an Ediff command for file-B.  Port of
    !> ediff.el ediff-last-dir-B.
    character(len=:), allocatable, public :: ediff_last_directory_b
    !> Last directory used by an Ediff command for file-C.  Port of
    !> ediff.el ediff-last-dir-C.
    character(len=:), allocatable, public :: ediff_last_directory_c
    !> Last directory used by an Ediff command for the ancestor file.
    !> Port of ediff.el ediff-last-dir-ancestor.
    character(len=:), allocatable, public :: ediff_last_directory_ancestor

    !> Which of the last directories `find_ediff_file' records
    !> (upstream's LAST-DIR symbol).
    integer, parameter :: LAST_DIRECTORY_NONE = 0, LAST_DIRECTORY_A = 1, LAST_DIRECTORY_B = 2, LAST_DIRECTORY_C = 3, &
                          LAST_DIRECTORY_ANCESTOR = 4

    !> Last directory used by an Ediff command as the output directory for
    !> merge.  Port of ediff.el ediff-last-merge-autostore-dir.
    character(len=:), allocatable, public :: ediff_last_merge_autostore_directory

    interface
        !> Run Ediff on directories DIRECTORY1 and DIRECTORY2, comparing files.
        !> Consider only files that have the same name in both directories.
        !>
        !> REGULAR_EXPRESSION is "" or a regular expression; only file names that match
        !> the regexp are considered.  Port of ediff.el:ediff-directories.
        !> (In fortmacs_diff_directories_mod.)
        recursive module subroutine ediff_directories(directory1, directory2, regular_expression, status)
            implicit none(type, external)
            character(len=*), intent(in) :: directory1, directory2, regular_expression
            type(status_type), intent(out) :: status
        end subroutine ediff_directories

        !> Run Ediff on directories DIRECTORY1, DIRECTORY2, and DIRECTORY3, comparing files.
        !> Consider only files that have the same name in all three
        !> directories.
        !>
        !> REGULAR_EXPRESSION is "" or a regular expression; only file names that match
        !> the regexp are considered.  Port of ediff.el:ediff-directories3.
        !> (In fortmacs_diff_directories_mod.)
        recursive module subroutine ediff_directories3(directory1, directory2, directory3, regular_expression, status)
            implicit none(type, external)
            character(len=*), intent(in) :: directory1, directory2, directory3, regular_expression
            type(status_type), intent(out) :: status
        end subroutine ediff_directories3

        !> Run Ediff on two directories, DIRECTORY1 and DIRECTORY2, merging files
        !> that have the same name in both.  The third argument, REGULAR_EXPRESSION, is ""
        !> or a regular expression; only file names that match the regexp
        !> are considered.  MERGE_AUTOSTORE_DIRECTORY is the directory in which to
        !> store merged files.  Port of ediff.el:ediff-merge-directories.
        !> (In fortmacs_diff_directories_mod.)
        recursive module subroutine ediff_merge_directories(directory1, directory2, regular_expression, status, &
                                                            merge_autostore_directory)
            implicit none(type, external)
            character(len=*), intent(in) :: directory1, directory2, regular_expression
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: merge_autostore_directory
        end subroutine ediff_merge_directories

        !> Merge files in DIRECTORY1 and DIRECTORY2 using files in ANCESTOR_DIRECTORY as
        !> ancestors.
        !> Ediff merges files that have identical names in DIRECTORY1, DIRECTORY2.  If two
        !> files in DIRECTORY1 and DIRECTORY2 don't have an ancestor in
        !> ANCESTOR_DIRECTORY, Ediff will merge without ancestor.  The fourth
        !> argument, REGULAR_EXPRESSION, is "" or a regular expression; only file names
        !> that match the regexp are considered.  MERGE_AUTOSTORE_DIRECTORY is the
        !> directory in which to store merged files.  Port of
        !> ediff.el:ediff-merge-directories-with-ancestor.  (In
        !> fortmacs_diff_directories_mod.)
        recursive module subroutine ediff_merge_directories_with_ancestor(directory1, directory2, ancestor_directory, &
                                                                          regular_expression, &
                                                                          status, &
                                                                          merge_autostore_directory)
            implicit none(type, external)
            character(len=*), intent(in) :: directory1, directory2, ancestor_directory, regular_expression
            type(status_type), intent(out) :: status
            character(len=*), intent(in), optional :: merge_autostore_directory
        end subroutine ediff_merge_directories_with_ancestor

        !> Run the session group job JOB_NAME (commands%ediff_directories,
        !> commands%ediff_directories3, commands%ediff_merge_directories or
        !> commands%ediff_merge_directories_with_ancestor) on directories
        !> DIRECTORY1 and DIRECTORY2 (and DIRECTORY3, the ancestors when merging; "" for
        !> none): its sessions compare or merge the files with
        !> `ediff_files', `ediff_files3', `ediff_merge_files' or
        !> `ediff_merge_files_with_ancestor'.  REGULAR_EXPRESSION is "" or a regular
        !> expression; only file names that match the regexp are
        !> considered.  STARTUP_HOOKS run in the group buffer once it is
        !> made.  MERGE_AUTOSTORE_DIRECTORY is the directory in which to store
        !> merged files.  Port of ediff.el:ediff-directories-internal (the
        !> job gives ACTION).  (In fortmacs_diff_directories_mod.)
        recursive module subroutine ediff_directories_internal(directory1, directory2, directory3, regular_expression, job_name, &
                                                               status, &
                                                               startup_hooks, &
                                                               merge_autostore_directory)
            implicit none(type, external)
            character(len=*), intent(in) :: directory1, directory2, directory3, regular_expression
            type(command_type), pointer, intent(in) :: job_name
            type(status_type), intent(out) :: status
            type(hook_type), intent(inout), optional :: startup_hooks
            character(len=*), intent(in), optional :: merge_autostore_directory
        end subroutine ediff_directories_internal

        !> Register the directory commands of ediff.el and set up
        !> fortmacs_diff_multiple_mod.  Not a port.  (In fortmacs_diff_directories_mod.)
        module subroutine initialize_ediff_directories()
            implicit none(type, external)
        end subroutine initialize_ediff_directories
    end interface

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
    !> LAST_DIRECTORY is the last directory (A, B, C or the ancestor's) where
    !> FILE's directory name should be recorded.
    !> BUFFER is the buffer into which the file is visited.  Port of
    !> ediff.el:ediff-find-file (compressed and remote files, which need
    !> temporary copies, are deferred with file name handlers).
    recursive subroutine find_ediff_file(file_in, last_directory, buffer, status)
        character(len=*), intent(in) :: file_in
        integer, intent(in) :: last_directory
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file, directory
        type(handle_type) :: old
        type(status_type) :: st

        status = status_type()

        if (.not. is_file_readable(file_in)) then
            call set_error(status, USER_ERROR_CODE, format_message("File `")//file_in// &
                           format_message("' does not exist or is not readable"))
            return
        else if (is_file_directory(file_in)) then
            call set_error(status, USER_ERROR_CODE, format_message("File `")//file_in//format_message("' is a directory"))
            return
        end if

        ! Some of the commands, below, require full file name.
        call expand_file_name(file_in, file, status)
        if (.not. status%ok()) return
        ! Record the directory of the file.
        call expand_file_name(file_name_directory(file), directory, status)
        if (.not. status%ok()) return

        select case (last_directory)
        case (LAST_DIRECTORY_A)
            ediff_last_directory_a = directory
        case (LAST_DIRECTORY_B)
            ediff_last_directory_b = directory
        case (LAST_DIRECTORY_C)
            ediff_last_directory_c = directory
        case (LAST_DIRECTORY_ANCESTOR)
            ediff_last_directory_ancestor = directory
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

    !> Visit the files and set up the session; MERGE_BUFFER_FILE, if
    !> present, is the file to be associated with the merge buffer.
    !> Port of ediff.el:ediff-files-internal.
    recursive subroutine ediff_files_internal(file_a, file_b, file_c, job_name, status, startup_hooks, merge_buffer_file)
        character(len=*), intent(in) :: file_a, file_b, file_c
        type(command_type), pointer, intent(in) :: job_name
        type(status_type), intent(out) :: status
        type(hook_type), intent(inout), optional :: startup_hooks
        character(len=*), intent(in), optional :: merge_buffer_file
        type(handle_type) :: buffer_a, buffer_b, buffer_c, control

        status = status_type()

        if (file_a == file_b) then
            call set_error(status, GENERIC_ERROR_CODE, "Files A and B are the same")
            return
        end if

        if (len(file_c) > 0) then
            if (file_a == file_c) then
                call set_error(status, GENERIC_ERROR_CODE, "Files A and C are the same")
                return
            else if (file_b == file_c) then
                call set_error(status, GENERIC_ERROR_CODE, "Files B and C are the same")
                return
            end if
        end if

        call message("Reading file "//file_a//" ... ")
        call find_ediff_file(file_a, LAST_DIRECTORY_A, buffer_a, status)
        if (.not. status%ok()) return
        call message("Reading file "//file_b//" ... ")
        call find_ediff_file(file_b, LAST_DIRECTORY_B, buffer_b, status)
        if (.not. status%ok()) return
        buffer_c = null_handle()

        if (len(file_c) > 0) then
            call message("Reading file "//file_c//" ... ")

            if (associated(job_name, commands%ediff_merge_files_with_ancestor)) then
                call find_ediff_file(file_c, LAST_DIRECTORY_ANCESTOR, buffer_c, status)
            else
                call find_ediff_file(file_c, LAST_DIRECTORY_C, buffer_c, status)
            end if

            if (.not. status%ok()) return
        end if

        call set_up_ediff(buffer_a, buffer_file_name(buffer_a), buffer_b, buffer_file_name(buffer_b), buffer_c, &
                          buffer_file_name(buffer_c), job_name, control, status, startup_hooks, merge_buffer_file)
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
            call set_error(status, GENERIC_ERROR_CODE, "Nothing to revert")
            return
        end if

        if (.not. allocated(buffers(current)%filename)) then
            call set_error(status, GENERIC_ERROR_CODE, "Buffer does not seem to be associated with any file")
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
                call set_error(status, GENERIC_ERROR_CODE, "No backup found for "//file)
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
    !> the files; MERGE_BUFFER_FILE, if present, is the file to be
    !> associated with the merge buffer.  Port of
    !> ediff.el:ediff-buffers-internal (the files are deleted after the
    !> setup, as upstream deletes them on the way out).
    recursive subroutine ediff_buffers_internal(buffer_a, buffer_b, buffer_c, job_name, status, startup_hooks, &
                                                merge_buffer_file)
        type(handle_type), intent(in) :: buffer_a, buffer_b, buffer_c
        type(command_type), pointer, intent(in) :: job_name
        type(status_type), intent(out) :: status
        type(hook_type), intent(inout), optional :: startup_hooks
        character(len=*), intent(in), optional :: merge_buffer_file
        character(len=:), allocatable :: file_a, file_b, file_c
        type(handle_type) :: control
        type(status_type) :: st
        logical :: buffer_c_is_alive, three_way_comparison_job

        status = status_type()
        buffer_c_is_alive = is_buffer_live(buffer_c)
        file_c = ""

        if (.not. is_buffer_live(buffer_a)) then
            call set_error(status, USER_ERROR_CODE, "Buffer #<killed buffer> doesn't exist")
            return
        else if (.not. is_buffer_live(buffer_b)) then
            call set_error(status, USER_ERROR_CODE, "Buffer #<killed buffer> doesn't exist")
            return
        end if

        three_way_comparison_job = associated(job_name, commands%ediff_files3) .or. &
                                   associated(job_name, commands%ediff_buffers3)

        if (three_way_comparison_job .and. .not. buffer_c_is_alive) then
            call set_error(status, USER_ERROR_CODE, "Buffer #<killed buffer> doesn't exist")
            return
        end if

        call make_temporary(buffer_a, file_a)
        if (status%ok()) call make_temporary(buffer_b, file_b)
        if (status%ok() .and. buffer_c_is_alive) call make_temporary(buffer_c, file_c)

        if (status%ok()) then
            call set_up_ediff(buffer_a, file_a, buffer_b, file_b, buffer_c, file_c, job_name, control, status, startup_hooks, &
                              merge_buffer_file)
        end if

        ! Delete the temporary files, keeping the first error.
        if (allocated(file_a)) call delete_temporary(file_a)
        if (allocated(file_b)) call delete_temporary(file_b)
        if (len(file_c) > 0) call delete_temporary(file_c)

    contains

        ! A temporary file with the text of BUFFER, named after its file.
        subroutine make_temporary(buffer, file)
            type(handle_type), intent(in) :: buffer
            character(len=:), allocatable, intent(out) :: file

            if (allocated(buffers(buffer%index)%filename)) then
                call create_ediff_temporary_file(buffer, file, status, file_name_nondirectory(buffers(buffer%index)%filename))
            else
                call create_ediff_temporary_file(buffer, file, status)
            end if
        end subroutine make_temporary

        ! Delete FILE if it exists; its error goes to STATUS unless an
        ! error is already recorded there.
        subroutine delete_temporary(file)
            character(len=*), intent(in) :: file

            if (.not. file_exists(file)) return
            call delete_file(file, st)
            if (status%ok()) status = st
        end subroutine delete_temporary
    end subroutine ediff_buffers_internal

    ! ------------------------------------------------------------------
    ! Merging files and buffers
    ! ------------------------------------------------------------------

    !> Merge the differences of session K's buffers into its merge buffer
    !> (a startup hook of the merge jobs).  Port of
    !> ediff.el:ediff-merge-on-startup.
    recursive subroutine merge_ediff_on_startup(status)
        type(status_type), intent(out) :: status
        integer :: k

        status = status_type()
        k = ediff_session_index()
        if (k == 0) return
        call do_ediff_merge(k, 0, status)
    end subroutine merge_ediff_on_startup

    !> STARTUP_HOOKS (none if absent) with `merge_ediff_on_startup' added
    !> first, in HOOKS.  Not a port: what ediff.el's merge commands do to
    !> their STARTUP-HOOKS.
    subroutine merge_startup_hooks(startup_hooks, hooks)
        type(hook_type), intent(in), optional :: startup_hooks
        type(hook_type), intent(inout), target :: hooks

        if (present(startup_hooks)) hooks = startup_hooks
        call hooks%prepend(merge_ediff_on_startup)
    end subroutine merge_startup_hooks

    !> Merge two files without ancestor.
    !> FILE_A and FILE_B are the names of the files to be merged.
    !> STARTUP_HOOKS holds functions that Fortmacs calls without arguments
    !> after setting up the Ediff buffers.  MERGE_BUFFER_FILE is the name
    !> of the file to be associated with the merge buffer..  Port of
    !> ediff.el:ediff-merge-files.
    recursive subroutine ediff_merge_files(file_a, file_b, status, startup_hooks, merge_buffer_file)
        character(len=*), intent(in) :: file_a, file_b
        type(status_type), intent(out) :: status
        type(hook_type), intent(in), optional :: startup_hooks
        character(len=*), intent(in), optional :: merge_buffer_file
        type(hook_type), target :: hooks
        character(len=:), allocatable :: fb

        call merge_startup_hooks(startup_hooks, hooks)
        call file_in_directory(file_a, file_b, fb, status)
        if (.not. status%ok()) return
        call ediff_files_internal(file_a, fb, "", commands%ediff_merge_files, status, hooks, merge_buffer_file)
    end subroutine ediff_merge_files

    !> Merge two files with ancestor.
    !> FILE_A and FILE_B are the names of the files to be merged, and
    !> FILE_ANCESTOR is the name of the ancestor file.  STARTUP_HOOKS holds
    !> functions that Fortmacs calls without arguments after setting up the
    !> Ediff buffers.  MERGE_BUFFER_FILE is the name of the file to be
    !> associated with the merge buffer.  Port of
    !> ediff.el:ediff-merge-files-with-ancestor.
    recursive subroutine ediff_merge_files_with_ancestor(file_a, file_b, file_ancestor, status, startup_hooks, &
                                                         merge_buffer_file)
        character(len=*), intent(in) :: file_a, file_b, file_ancestor
        type(status_type), intent(out) :: status
        type(hook_type), intent(in), optional :: startup_hooks
        character(len=*), intent(in), optional :: merge_buffer_file
        type(hook_type), target :: hooks
        character(len=:), allocatable :: fb

        call merge_startup_hooks(startup_hooks, hooks)
        call file_in_directory(file_a, file_b, fb, status)
        if (.not. status%ok()) return
        call ediff_files_internal(file_a, fb, file_ancestor, commands%ediff_merge_files_with_ancestor, status, hooks, &
                                  merge_buffer_file)
    end subroutine ediff_merge_files_with_ancestor

    !> Merge buffers without ancestor.
    !> BUFFER_A and BUFFER_B are the buffers to be merged.
    !> STARTUP_HOOKS holds functions that Fortmacs calls without arguments
    !> after setting up the Ediff buffers.  JOB_NAME is the Ediff job type
    !> (a command); it defaults to commands%ediff_merge_buffers, but can
    !> also be commands%ediff_merge_files_with_ancestor,
    !> commands%ediff_buffers, commands%ediff_buffers3, or
    !> commands%ediff_merge_buffers_with_ancestor.  MERGE_BUFFER_FILE is the
    !> name of the file to be associated with the merge buffer.  Port of
    !> ediff.el:ediff-merge-buffers.
    recursive subroutine ediff_merge_buffers(buffer_a, buffer_b, status, startup_hooks, job_name, merge_buffer_file)
        type(handle_type), intent(in) :: buffer_a, buffer_b
        type(status_type), intent(out) :: status
        type(hook_type), intent(in), optional :: startup_hooks
        type(command_type), pointer, intent(in), optional :: job_name
        character(len=*), intent(in), optional :: merge_buffer_file
        type(hook_type), target :: hooks
        type(command_type), pointer :: job

        call merge_startup_hooks(startup_hooks, hooks)
        job => commands%ediff_merge_buffers
        if (present(job_name)) job => job_name
        call ediff_buffers_internal(buffer_a, buffer_b, null_handle(), job, status, hooks, merge_buffer_file)
    end subroutine ediff_merge_buffers

    !> Merge buffers with ancestor.
    !> BUFFER_A and BUFFER_B are the buffers to be merged, and
    !> BUFFER_ANCESTOR is their ancestor.  STARTUP_HOOKS holds functions
    !> that Fortmacs calls without arguments after setting up the Ediff
    !> buffers.  JOB_NAME is the Ediff job type (a command); it defaults to
    !> commands%ediff_merge_buffers_with_ancestor, but can also be
    !> commands%ediff_merge_files_with_ancestor, commands%ediff_buffers,
    !> commands%ediff_buffers3, or commands%ediff_merge_buffers.
    !> MERGE_BUFFER_FILE is the name of the file to be associated with the
    !> merge buffer.  Port of ediff.el:ediff-merge-buffers-with-ancestor.
    recursive subroutine ediff_merge_buffers_with_ancestor(buffer_a, buffer_b, buffer_ancestor, status, startup_hooks, &
                                                           job_name, merge_buffer_file)
        type(handle_type), intent(in) :: buffer_a, buffer_b, buffer_ancestor
        type(status_type), intent(out) :: status
        type(hook_type), intent(in), optional :: startup_hooks
        type(command_type), pointer, intent(in), optional :: job_name
        character(len=*), intent(in), optional :: merge_buffer_file
        type(hook_type), target :: hooks
        type(command_type), pointer :: job

        call merge_startup_hooks(startup_hooks, hooks)
        job => commands%ediff_merge_buffers_with_ancestor
        if (present(job_name)) job => job_name
        call ediff_buffers_internal(buffer_a, buffer_b, buffer_ancestor, job, status, hooks, merge_buffer_file)
    end subroutine ediff_merge_buffers_with_ancestor

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    ! The default directory of reading file A: the last one, or
    ! `default_directory'.  Not a port.
    function directory_a() result(directory)
        character(len=:), allocatable :: directory

        directory = ""

        if (ediff_use_last_directory) then
            if (allocated(ediff_last_directory_a)) directory = ediff_last_directory_a
        else if (allocated(buffers(current)%directory)) then
            directory = buffers(current)%directory
        end if
    end function directory_a

    ! Read the file after FILE: in DIRECTORY_LAST (the last one) or the
    ! directory of FILE, adding FILE's name in it to the file name history.
    ! Not a port.
    recursive subroutine read_next_file(prompt, file, directory_last, result, status)
        character(len=*), intent(in) :: prompt, file
        character(len=:), allocatable, intent(in) :: directory_last
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: directory, expanded

        if (ediff_use_last_directory) then
            directory = ""
            if (allocated(directory_last)) directory = directory_last
        else
            directory = file_name_directory(file)
        end if

        call expand_file_name(file_name_nondirectory(file), expanded, status, directory)
        if (.not. status%ok()) return
        call add_to_history("file_name_history", abbreviate_file_name(expanded), status=status)
        if (.not. status%ok()) return
        call read_ediff_file_name(prompt, directory, get_ediff_default_file_name(file), result, status)
    end subroutine read_next_file

    !> Run Ediff on two files, FILE_A and FILE_B.  Port of
    !> ediff.el:ediff-files (the command; also `ediff').
    recursive subroutine execute_ediff_files(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: f, ff

        call read_ediff_file_name("File A to compare", directory_a(), get_ediff_default_file_name(), f, status, .true.)
        if (.not. status%ok()) return
        call read_next_file("File B to compare", f, ediff_last_directory_b, ff, status)
        if (.not. status%ok()) return
        call ediff_files(f, ff, status)
    end subroutine execute_ediff_files

    !> Run Ediff on three files, FILE_A, FILE_B, and FILE_C.  Port of
    !> ediff.el:ediff-files3 (the command; also `ediff3').
    recursive subroutine execute_ediff_files3(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: f, ff, fff

        call read_ediff_file_name("File A to compare", directory_a(), get_ediff_default_file_name(), f, status, .true.)
        if (.not. status%ok()) return
        call read_next_file("File B to compare", f, ediff_last_directory_b, ff, status)
        if (.not. status%ok()) return
        call read_next_file("File C to compare", ff, ediff_last_directory_c, fff, status)
        if (.not. status%ok()) return
        call ediff_files3(f, ff, fff, status)
    end subroutine execute_ediff_files3

    recursive subroutine execute_ediff_current_file(status)
        type(status_type), intent(out) :: status

        call ediff_current_file(status)
    end subroutine execute_ediff_current_file

    recursive subroutine execute_ediff_backup(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: file

        call read_file_name("Ediff (file with backup): ", file, status)
        if (.not. status%ok()) return
        call ediff_backup(file, status)
    end subroutine execute_ediff_backup

    !> Run Ediff on two buffers, BUFFER_A and BUFFER_B.  Port of
    !> ediff.el:ediff-buffers (the command; also `ebuffers').
    recursive subroutine execute_ediff_buffers(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: bf, bff, default

        call ediff_other_buffer([character(len=1) :: ""], default)
        call read_buffer("Buffer A to compare: ", bf, status, def=default, require_match=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        call ediff_other_buffer([character(len=len(bf)) :: bf], default)
        call read_buffer("Buffer B to compare: ", bff, status, def=default, require_match=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        call ediff_buffers(get_buffer(bf), get_buffer(bff), status)
    end subroutine execute_ediff_buffers

    !> Run Ediff on three buffers, BUFFER_A, BUFFER_B, and BUFFER_C.  Port
    !> of ediff.el:ediff-buffers3 (the command; also `ebuffers3').
    recursive subroutine execute_ediff_buffers3(status)
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
    end subroutine execute_ediff_buffers3

    !> Merge two files without ancestor.
    !> FILE_A and FILE_B are the names of the files to be merged.  Port of
    !> ediff.el:ediff-merge-files (the command; also `ediff_merge').
    recursive subroutine execute_ediff_merge_files(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: f, ff

        call read_ediff_file_name("File A to merge", directory_a(), get_ediff_default_file_name(), f, status, .true.)
        if (.not. status%ok()) return
        call read_next_file("File B to merge", f, ediff_last_directory_b, ff, status)
        if (.not. status%ok()) return
        call ediff_merge_files(f, ff, status)
    end subroutine execute_ediff_merge_files

    !> Merge two files with ancestor.
    !> FILE_A and FILE_B are the names of the files to be merged, and
    !> FILE_ANCESTOR is the name of the ancestor file.  Port of
    !> ediff.el:ediff-merge-files-with-ancestor (the command; also
    !> `ediff_merge_with_ancestor').
    recursive subroutine execute_ediff_merge_files_with_ancestor(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: f, ff, fff

        call read_ediff_file_name("File A to merge", directory_a(), get_ediff_default_file_name(), f, status, .true.)
        if (.not. status%ok()) return
        call read_next_file("File B to merge", f, ediff_last_directory_b, ff, status)
        if (.not. status%ok()) return
        call read_next_file("Ancestor file", ff, ediff_last_directory_ancestor, fff, status)
        if (.not. status%ok()) return
        call ediff_merge_files_with_ancestor(f, ff, fff, status)
    end subroutine execute_ediff_merge_files_with_ancestor

    !> Merge buffers without ancestor.
    !> BUFFER_A and BUFFER_B are the buffers to be merged.  Port of
    !> ediff.el:ediff-merge-buffers (the command).
    recursive subroutine execute_ediff_merge_buffers(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: bf, bff, default

        call ediff_other_buffer([character(len=1) :: ""], default)
        call read_buffer("Buffer A to merge: ", bf, status, def=default, require_match=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        call ediff_other_buffer([character(len=len(bf)) :: bf], default)
        call read_buffer("Buffer B to merge: ", bff, status, def=default, require_match=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        call ediff_merge_buffers(get_buffer(bf), get_buffer(bff), status)
    end subroutine execute_ediff_merge_buffers

    !> Merge buffers with ancestor.
    !> BUFFER_A and BUFFER_B are the buffers to be merged, and
    !> BUFFER_ANCESTOR is their ancestor.  Port of
    !> ediff.el:ediff-merge-buffers-with-ancestor (the command).
    recursive subroutine execute_ediff_merge_buffers_with_ancestor(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: bf, bff, bfff, default

        call ediff_other_buffer([character(len=1) :: ""], default)
        call read_buffer("Buffer A to merge: ", bf, status, def=default, require_match=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        call ediff_other_buffer([character(len=len(bf)) :: bf], default)
        call read_buffer("Buffer B to merge: ", bff, status, def=default, require_match=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        call ediff_other_buffer([character(len=max(len(bf), len(bff))) :: bf, bff], default)
        call read_buffer("Ancestor buffer: ", bfff, status, def=default, require_match=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        call ediff_merge_buffers_with_ancestor(get_buffer(bf), get_buffer(bff), get_buffer(bfff), status)
    end subroutine execute_ediff_merge_buffers_with_ancestor

    !> Register the commands of Ediff and set up its modules.  Not a port.
    subroutine initialize_ediff()
        if (associated(commands%ediff_files%execute)) return
        call initialize_ediff_init()
        call initialize_ediff_wind()
        call initialize_ediff_diff()
        call initialize_ediff_utility()
        call initialize_ediff_merg()
        commands%ediff_files%execute => execute_ediff_files
        commands%ediff%execute => execute_ediff_files
        commands%ediff_files3%execute => execute_ediff_files3
        commands%ediff3%execute => execute_ediff_files3
        commands%ediff_current_file%execute => execute_ediff_current_file
        commands%ediff_backup%execute => execute_ediff_backup
        commands%ediff_buffers%execute => execute_ediff_buffers
        commands%ebuffers%execute => execute_ediff_buffers
        commands%ediff_buffers3%execute => execute_ediff_buffers3
        commands%ebuffers3%execute => execute_ediff_buffers3
        commands%ediff_merge_files%execute => execute_ediff_merge_files
        commands%ediff_merge%execute => execute_ediff_merge_files
        commands%ediff_merge_files_with_ancestor%execute => execute_ediff_merge_files_with_ancestor
        commands%ediff_merge_with_ancestor%execute => execute_ediff_merge_files_with_ancestor
        commands%ediff_merge_buffers%execute => execute_ediff_merge_buffers
        commands%ediff_merge_buffers_with_ancestor%execute => execute_ediff_merge_buffers_with_ancestor
        call initialize_ediff_directories()
    end subroutine initialize_ediff
end module fortmacs_diff_mod
