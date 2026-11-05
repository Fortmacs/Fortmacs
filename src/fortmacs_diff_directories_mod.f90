! fortmacs_diff_directories_mod.f90 -- Ediff's directory jobs.
! Ported from GNU Emacs 31.1 lisp/vc/ediff.el (in part: the directory
! jobs, which make session groups).
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

! The procedures of fortmacs_diff_mod that make session groups with
! fortmacs_diff_multiple_mod (which uses fortmacs_diff_mod), with their commands.
submodule(fortmacs_diff_mod) fortmacs_diff_directories_mod
    use minibuffer_mod, only: read_directory_name, REQUIRE_MATCH_OTHER
    use minibuffer_reading_mod, only: read_string
    use replace_mod, only: format_prompt
    use basic_subprograms_mod, only: read_y_or_n
    use fortmacs_diff_settings_mod, only: fortmacs_diff_autostore_merges, is_fortmacs_diff_merge_metajob, &
                                          strip_fortmacs_diff_last_directory
    use fortmacs_diff_multiple_mod, only: fortmacs_diff_session_group_type, intersect_fortmacs_diff_directories, &
                                          prepare_fortmacs_diff_meta_buffer, show_fortmacs_diff_meta_buffer, &
                                          get_fortmacs_diff_default_filtering_regular_expression, &
                                          initialize_fortmacs_diff_multiple
    implicit none(type, external)

contains

    recursive module subroutine fortmacs_diff_directories(directory1, directory2, regular_expression, status)
        character(len=*), intent(in) :: directory1, directory2, regular_expression
        type(status_type), intent(out) :: status

        call fortmacs_diff_directories_internal(directory1, directory2, "", regular_expression, &
                                                commands%fortmacs_diff_directories, status)
    end subroutine fortmacs_diff_directories

    recursive module subroutine fortmacs_diff_directories3(directory1, directory2, directory3, regular_expression, status)
        character(len=*), intent(in) :: directory1, directory2, directory3, regular_expression
        type(status_type), intent(out) :: status

        call fortmacs_diff_directories_internal(directory1, directory2, directory3, regular_expression, &
                                                commands%fortmacs_diff_directories3, status)
    end subroutine fortmacs_diff_directories3

    recursive module subroutine fortmacs_diff_merge_directories(directory1, directory2, regular_expression, status, &
                                                                merge_autostore_directory)
        character(len=*), intent(in) :: directory1, directory2, regular_expression
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: merge_autostore_directory

        call fortmacs_diff_directories_internal(directory1, directory2, "", regular_expression, &
                                                commands%fortmacs_diff_merge_directories, status, &
                                                merge_autostore_directory=merge_autostore_directory)
    end subroutine fortmacs_diff_merge_directories

    recursive module subroutine fortmacs_diff_merge_directories_with_ancestor(directory1, directory2, ancestor_directory, &
                                                                              regular_expression, &
                                                                              status, &
                                                                              merge_autostore_directory)
        character(len=*), intent(in) :: directory1, directory2, ancestor_directory, regular_expression
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: merge_autostore_directory

        call fortmacs_diff_directories_internal(directory1, directory2, ancestor_directory, regular_expression, &
                                                commands%fortmacs_diff_merge_directories_with_ancestor, &
                                                status, merge_autostore_directory=merge_autostore_directory)
    end subroutine fortmacs_diff_merge_directories_with_ancestor

    recursive module subroutine fortmacs_diff_directories_internal(directory1, directory2, directory3, regular_expression, &
                                                                   job_name, &
                                                                   status, &
                                                                   startup_hooks, &
                                                                   merge_autostore_directory)
        character(len=*), intent(in) :: directory1, directory2, directory3, regular_expression
        type(command_type), pointer, intent(in) :: job_name
        type(status_type), intent(out) :: status
        type(hook_type), intent(inout), optional :: startup_hooks
        character(len=*), intent(in), optional :: merge_autostore_directory
        type(fortmacs_diff_session_group_type) :: group
        type(handle_type) :: meta_buffer
        character(len=:), allocatable :: directory_c_name, autostore, default
        logical :: answer

        status = status_type()
        directory_c_name = directory3

        if (len(directory_c_name) > 0) then
            if (.not. is_file_directory(directory_c_name)) directory_c_name = file_name_directory(directory_c_name)
        end if

        if (directory1 == directory2) then
            call set_error(status, USER_ERROR_CODE, "Directories A and B are the same: "//directory1)
            return
        else if (associated(job_name, commands%fortmacs_diff_directories3) .and. directory1 == directory_c_name) then
            call set_error(status, USER_ERROR_CODE, "Directories A and C are the same: "//directory1)
            return
        else if (associated(job_name, commands%fortmacs_diff_directories3) .and. directory2 == directory_c_name) then
            call set_error(status, USER_ERROR_CODE, "Directories B and C are the same: "//directory1)
            return
        end if

        if (present(merge_autostore_directory)) then
            autostore = merge_autostore_directory
        else if (.not. fortmacs_diff_autostore_merges%is_off() .and. is_fortmacs_diff_merge_metajob(job_name)) then
            if (fortmacs_diff_use_last_directory) then
                default = ""
                if (allocated(fortmacs_diff_last_merge_autostore_directory)) default = fortmacs_diff_last_merge_autostore_directory
            else
                call strip_fortmacs_diff_last_directory(directory1, default, status)
                if (.not. status%ok()) return
            end if

            call read_directory_name("Save merged files in directory: ", autostore, status, directory=default, &
                                     mustmatch=REQUIRE_MATCH_OTHER)
            if (.not. status%ok()) return
        end if

        ! Verify we are not merging into an orig directory.
        if (allocated(autostore)) then
            if (autostore == directory1) then
                call confirm("Directory for saving merged files = Directory A.  Sure? ")
            else if (autostore == directory2) then
                call confirm("Directory for saving merged files = Directory B.  Sure? ")
            else if (len(directory_c_name) > 0 .and. autostore == directory_c_name) then
                call confirm("Directory for saving merged files = Ancestor Directory.  Sure? ")
            end if

            if (.not. status%ok()) return
        else
            autostore = ""
        end if

        call intersect_fortmacs_diff_directories(job_name, regular_expression, directory1, directory2, directory_c_name, &
                                                 autostore, group, &
                                                 status)
        if (.not. status%ok()) return
        call prepare_fortmacs_diff_meta_buffer(group, "*Ediff Session Group Panel", .false., meta_buffer, status, startup_hooks)
        if (.not. status%ok()) return
        call show_fortmacs_diff_meta_buffer(status, meta_buffer)

    contains

        ! Ask PROMPT; no gives the user error of an aborted merge.
        recursive subroutine confirm(prompt)
            character(len=*), intent(in) :: prompt

            call read_y_or_n(prompt, answer, status)
            if (.not. status%ok()) return
            if (.not. answer) call set_error(status, USER_ERROR_CODE, "Directory merge aborted")
        end subroutine confirm
    end subroutine fortmacs_diff_directories_internal

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    ! The default directory of reading directory A: the last one, or
    ! `default_directory'.  Port of ediff.el:ediff-get-default-directory-name
    ! (Dired's directory at point is Deferred).
    function default_directory_name() result(directory)
        character(len=:), allocatable :: directory

        directory = ""

        if (fortmacs_diff_use_last_directory) then
            if (allocated(fortmacs_diff_last_directory_a)) directory = fortmacs_diff_last_directory_a
        else if (current > 0) then
            if (allocated(buffers(current)%directory)) directory = buffers(current)%directory
        end if
    end function default_directory_name

    ! Read the directory after F with PROMPT: in LAST (the last one, with
    ! `fortmacs_diff_use_last_directory') or the parent of F.  Not a port.
    recursive subroutine read_next_directory(prompt, f, last, result, status)
        character(len=*), intent(in) :: prompt, f
        character(len=:), allocatable, intent(in) :: last
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: directory

        if (fortmacs_diff_use_last_directory) then
            directory = ""
            if (allocated(last)) directory = last
        else
            call strip_fortmacs_diff_last_directory(f, directory, status)
            if (.not. status%ok()) return
        end if

        call read_directory_name(prompt, result, status, directory=directory, mustmatch=REQUIRE_MATCH_OTHER)
    end subroutine read_next_directory

    ! Read the filter of the file names.  Not a port.
    recursive subroutine read_filter(regular_expression, status)
        character(len=:), allocatable, intent(out) :: regular_expression
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: default

        default = get_fortmacs_diff_default_filtering_regular_expression()
        call read_string(format_prompt("Filter filenames through regular expression", default), regular_expression, status, &
                         history="fortmacs_diff_filtering_regular_expression_history", default=default)
    end subroutine read_filter

    !> Run Ediff on directories DIRECTORY1 and DIRECTORY2, comparing files.
    !> Consider only files that have the same name in both directories.
    !>
    !> REGULAR_EXPRESSION is "" or a regular expression; only file names that match
    !> the regexp are considered.  Port of ediff.el:ediff-directories (the
    !> command; also `edirs').
    recursive subroutine execute_fortmacs_diff_directories(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: f, ff, regular_expression

        call read_directory_name("Directory A to compare: ", f, status, directory=default_directory_name(), &
                                 mustmatch=REQUIRE_MATCH_OTHER)
        if (.not. status%ok()) return
        call read_next_directory("Directory B to compare: ", f, fortmacs_diff_last_directory_b, ff, status)
        if (.not. status%ok()) return
        call read_filter(regular_expression, status)
        if (.not. status%ok()) return
        call fortmacs_diff_directories(f, ff, regular_expression, status)
    end subroutine execute_fortmacs_diff_directories

    !> Run Ediff on directories DIRECTORY1, DIRECTORY2, and DIRECTORY3, comparing files.
    !> Consider only files that have the same name in all three
    !> directories.
    !>
    !> REGULAR_EXPRESSION is "" or a regular expression; only file names that match
    !> the regexp are considered.  Port of ediff.el:ediff-directories3 (the
    !> command; also `edirs3').
    recursive subroutine execute_fortmacs_diff_directories3(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: f, ff, fff, regular_expression

        call read_directory_name("Directory A to compare:", f, status, directory=default_directory_name())
        if (.not. status%ok()) return
        call read_next_directory("Directory B to compare:", f, fortmacs_diff_last_directory_b, ff, status)
        if (.not. status%ok()) return
        call read_next_directory("Directory C to compare:", ff, fortmacs_diff_last_directory_c, fff, status)
        if (.not. status%ok()) return
        call read_filter(regular_expression, status)
        if (.not. status%ok()) return
        call fortmacs_diff_directories3(f, ff, fff, regular_expression, status)
    end subroutine execute_fortmacs_diff_directories3

    !> Run Ediff on two directories, DIRECTORY1 and DIRECTORY2, merging files
    !> that have the same name in both.  The third argument, REGULAR_EXPRESSION, is ""
    !> or a regular expression; only file names that match the regexp are
    !> considered.  Port of ediff.el:ediff-merge-directories (the command;
    !> also `edirs_merge').
    recursive subroutine execute_fortmacs_diff_merge_directories(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: f, ff, regular_expression

        call read_directory_name("Directory A to merge:", f, status, directory=default_directory_name(), &
                                 mustmatch=REQUIRE_MATCH_OTHER)
        if (.not. status%ok()) return
        call read_next_directory("Directory B to merge:", f, fortmacs_diff_last_directory_b, ff, status)
        if (.not. status%ok()) return
        call read_filter(regular_expression, status)
        if (.not. status%ok()) return
        call fortmacs_diff_merge_directories(f, ff, regular_expression, status)
    end subroutine execute_fortmacs_diff_merge_directories

    !> Merge files in DIRECTORY1 and DIRECTORY2 using files in ANCESTOR_DIRECTORY as
    !> ancestors.
    !> Ediff merges files that have identical names in DIRECTORY1, DIRECTORY2.  If two
    !> files in DIRECTORY1 and DIRECTORY2 don't have an ancestor in
    !> ANCESTOR_DIRECTORY, Ediff will merge without ancestor.  The fourth
    !> argument, REGULAR_EXPRESSION, is "" or a regular expression; only file names
    !> that match the regexp are considered.  Port of
    !> ediff.el:ediff-merge-directories-with-ancestor (the command; also
    !> `edirs_merge_with_ancestor').
    recursive subroutine execute_fortmacs_diff_merge_directories_with_ancestor(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: f, ff, fff, regular_expression

        call read_directory_name("Directory A to merge:", f, status, directory=default_directory_name())
        if (.not. status%ok()) return
        call read_next_directory("Directory B to merge:", f, fortmacs_diff_last_directory_b, ff, status)
        if (.not. status%ok()) return
        call read_next_directory("Ancestor directory:", ff, fortmacs_diff_last_directory_c, fff, status)
        if (.not. status%ok()) return
        call read_filter(regular_expression, status)
        if (.not. status%ok()) return
        call fortmacs_diff_merge_directories_with_ancestor(f, ff, fff, regular_expression, status)
    end subroutine execute_fortmacs_diff_merge_directories_with_ancestor

    module subroutine initialize_fortmacs_diff_directories()
        commands%fortmacs_diff_directories%execute => execute_fortmacs_diff_directories
        commands%edirs%execute => execute_fortmacs_diff_directories
        commands%fortmacs_diff_directories3%execute => execute_fortmacs_diff_directories3
        commands%edirs3%execute => execute_fortmacs_diff_directories3
        commands%fortmacs_diff_merge_directories%execute => execute_fortmacs_diff_merge_directories
        commands%edirs_merge%execute => execute_fortmacs_diff_merge_directories
        commands%fortmacs_diff_merge_directories_with_ancestor%execute => execute_fortmacs_diff_merge_directories_with_ancestor
        commands%edirs_merge_with_ancestor%execute => execute_fortmacs_diff_merge_directories_with_ancestor
        call initialize_fortmacs_diff_multiple()
    end subroutine initialize_fortmacs_diff_directories
end submodule fortmacs_diff_directories_mod
