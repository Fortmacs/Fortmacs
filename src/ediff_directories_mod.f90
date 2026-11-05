! ediff_directories_mod.f90 -- Ediff's directory jobs.
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

! The procedures of ediff_mod that make session groups with
! ediff_multiple_mod (which uses ediff_mod), with their commands.
submodule(ediff_mod) ediff_directories_mod
    use minibuffer_mod, only: read_directory_name, REQUIRE_MATCH_OTHER
    use minibuf_mod, only: read_string
    use replace_mod, only: format_prompt
    use subr_mod, only: read_y_or_n
    use ediff_init_mod, only: ediff_autostore_merges, is_ediff_merge_metajob, strip_ediff_last_directory
    use ediff_multiple_mod, only: ediff_session_group_type, intersect_ediff_directories, prepare_ediff_meta_buffer, &
                                  show_ediff_meta_buffer, get_ediff_default_filtering_regexp, init_ediff_multiple
    implicit none(type, external)

contains

    recursive module subroutine ediff_directories(directory1, directory2, regexp, status)
        character(len=*), intent(in) :: directory1, directory2, regexp
        type(status_type), intent(out) :: status

        call ediff_directories_internal(directory1, directory2, "", regexp, commands%ediff_directories, status)
    end subroutine ediff_directories

    recursive module subroutine ediff_directories3(directory1, directory2, directory3, regexp, status)
        character(len=*), intent(in) :: directory1, directory2, directory3, regexp
        type(status_type), intent(out) :: status

        call ediff_directories_internal(directory1, directory2, directory3, regexp, commands%ediff_directories3, status)
    end subroutine ediff_directories3

    recursive module subroutine ediff_merge_directories(directory1, directory2, regexp, status, merge_autostore_directory)
        character(len=*), intent(in) :: directory1, directory2, regexp
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: merge_autostore_directory

        call ediff_directories_internal(directory1, directory2, "", regexp, commands%ediff_merge_directories, status, &
                                        merge_autostore_directory=merge_autostore_directory)
    end subroutine ediff_merge_directories

    recursive module subroutine ediff_merge_directories_with_ancestor(directory1, directory2, ancestor_directory, regexp, &
                                                                      status, &
                                                                      merge_autostore_directory)
        character(len=*), intent(in) :: directory1, directory2, ancestor_directory, regexp
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: merge_autostore_directory

        call ediff_directories_internal(directory1, directory2, ancestor_directory, regexp, &
                                        commands%ediff_merge_directories_with_ancestor, &
                                        status, merge_autostore_directory=merge_autostore_directory)
    end subroutine ediff_merge_directories_with_ancestor

    recursive module subroutine ediff_directories_internal(directory1, directory2, directory3, regexp, job_name, status, &
                                                           startup_hooks, &
                                                           merge_autostore_directory)
        character(len=*), intent(in) :: directory1, directory2, directory3, regexp
        type(command_type), pointer, intent(in) :: job_name
        type(status_type), intent(out) :: status
        type(hook_type), intent(inout), optional :: startup_hooks
        character(len=*), intent(in), optional :: merge_autostore_directory
        type(ediff_session_group_type) :: group
        type(handle_type) :: meta_buffer
        character(len=:), allocatable :: directory_c_name, autostore, default
        logical :: answer

        status = status_type()
        directory_c_name = directory3

        if (len(directory_c_name) > 0) then
            if (.not. is_file_directory(directory_c_name)) directory_c_name = file_name_directory(directory_c_name)
        end if

        if (directory1 == directory2) then
            call set_error(status, FM_USER_ERROR, "Directories A and B are the same: "//directory1)
            return
        else if (associated(job_name, commands%ediff_directories3) .and. directory1 == directory_c_name) then
            call set_error(status, FM_USER_ERROR, "Directories A and C are the same: "//directory1)
            return
        else if (associated(job_name, commands%ediff_directories3) .and. directory2 == directory_c_name) then
            call set_error(status, FM_USER_ERROR, "Directories B and C are the same: "//directory1)
            return
        end if

        if (present(merge_autostore_directory)) then
            autostore = merge_autostore_directory
        else if (.not. ediff_autostore_merges%is_off() .and. is_ediff_merge_metajob(job_name)) then
            if (ediff_use_last_dir) then
                default = ""
                if (allocated(ediff_last_merge_autostore_directory)) default = ediff_last_merge_autostore_directory
            else
                call strip_ediff_last_directory(directory1, default, status)
                if (.not. status%ok()) return
            end if

            call read_directory_name("Save merged files in directory: ", autostore, status, dir=default, &
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

        call intersect_ediff_directories(job_name, regexp, directory1, directory2, directory_c_name, autostore, group, status)
        if (.not. status%ok()) return
        call prepare_ediff_meta_buffer(group, "*Ediff Session Group Panel", .false., meta_buffer, status, startup_hooks)
        if (.not. status%ok()) return
        call show_ediff_meta_buffer(status, meta_buffer)

    contains

        ! Ask PROMPT; no gives the user error of an aborted merge.
        recursive subroutine confirm(prompt)
            character(len=*), intent(in) :: prompt

            call read_y_or_n(prompt, answer, status)
            if (.not. status%ok()) return
            if (.not. answer) call set_error(status, FM_USER_ERROR, "Directory merge aborted")
        end subroutine confirm
    end subroutine ediff_directories_internal

    ! ------------------------------------------------------------------
    ! Commands
    ! ------------------------------------------------------------------

    ! The default directory of reading directory A: the last one, or
    ! `default_directory'.  Port of ediff.el:ediff-get-default-directory-name
    ! (Dired's directory at point is Deferred).
    function default_directory_name() result(directory)
        character(len=:), allocatable :: directory

        directory = ""

        if (ediff_use_last_dir) then
            if (allocated(ediff_last_dir_a)) directory = ediff_last_dir_a
        else if (current > 0) then
            if (allocated(buffers(current)%directory)) directory = buffers(current)%directory
        end if
    end function default_directory_name

    ! Read the directory after F with PROMPT: in LAST (the last one, with
    ! `ediff_use_last_dir') or the parent of F.  Not a port.
    recursive subroutine read_next_directory(prompt, f, last, result, status)
        character(len=*), intent(in) :: prompt, f
        character(len=:), allocatable, intent(in) :: last
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: directory

        if (ediff_use_last_dir) then
            directory = ""
            if (allocated(last)) directory = last
        else
            call strip_ediff_last_directory(f, directory, status)
            if (.not. status%ok()) return
        end if

        call read_directory_name(prompt, result, status, dir=directory, mustmatch=REQUIRE_MATCH_OTHER)
    end subroutine read_next_directory

    ! Read the filter of the file names.  Not a port.
    recursive subroutine read_filter(regexp, status)
        character(len=:), allocatable, intent(out) :: regexp
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: default

        default = get_ediff_default_filtering_regexp()
        call read_string(format_prompt("Filter filenames through regular expression", default), regexp, status, &
                         history="ediff_filtering_regexp_history", default=default)
    end subroutine read_filter

    !> Run Ediff on directories DIRECTORY1 and DIRECTORY2, comparing files.
    !> Consider only files that have the same name in both directories.
    !>
    !> REGEXP is "" or a regular expression; only file names that match
    !> the regexp are considered.  Port of ediff.el:ediff-directories (the
    !> command; also `edirs').
    recursive subroutine cmd_ediff_directories(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: f, ff, regexp

        call read_directory_name("Directory A to compare: ", f, status, dir=default_directory_name(), &
                                 mustmatch=REQUIRE_MATCH_OTHER)
        if (.not. status%ok()) return
        call read_next_directory("Directory B to compare: ", f, ediff_last_dir_b, ff, status)
        if (.not. status%ok()) return
        call read_filter(regexp, status)
        if (.not. status%ok()) return
        call ediff_directories(f, ff, regexp, status)
    end subroutine cmd_ediff_directories

    !> Run Ediff on directories DIRECTORY1, DIRECTORY2, and DIRECTORY3, comparing files.
    !> Consider only files that have the same name in all three
    !> directories.
    !>
    !> REGEXP is "" or a regular expression; only file names that match
    !> the regexp are considered.  Port of ediff.el:ediff-directories3 (the
    !> command; also `edirs3').
    recursive subroutine cmd_ediff_directories3(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: f, ff, fff, regexp

        call read_directory_name("Directory A to compare:", f, status, dir=default_directory_name())
        if (.not. status%ok()) return
        call read_next_directory("Directory B to compare:", f, ediff_last_dir_b, ff, status)
        if (.not. status%ok()) return
        call read_next_directory("Directory C to compare:", ff, ediff_last_dir_c, fff, status)
        if (.not. status%ok()) return
        call read_filter(regexp, status)
        if (.not. status%ok()) return
        call ediff_directories3(f, ff, fff, regexp, status)
    end subroutine cmd_ediff_directories3

    !> Run Ediff on two directories, DIRECTORY1 and DIRECTORY2, merging files
    !> that have the same name in both.  The third argument, REGEXP, is ""
    !> or a regular expression; only file names that match the regexp are
    !> considered.  Port of ediff.el:ediff-merge-directories (the command;
    !> also `edirs_merge').
    recursive subroutine cmd_ediff_merge_directories(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: f, ff, regexp

        call read_directory_name("Directory A to merge:", f, status, dir=default_directory_name(), &
                                 mustmatch=REQUIRE_MATCH_OTHER)
        if (.not. status%ok()) return
        call read_next_directory("Directory B to merge:", f, ediff_last_dir_b, ff, status)
        if (.not. status%ok()) return
        call read_filter(regexp, status)
        if (.not. status%ok()) return
        call ediff_merge_directories(f, ff, regexp, status)
    end subroutine cmd_ediff_merge_directories

    !> Merge files in DIRECTORY1 and DIRECTORY2 using files in ANCESTOR_DIRECTORY as
    !> ancestors.
    !> Ediff merges files that have identical names in DIRECTORY1, DIRECTORY2.  If two
    !> files in DIRECTORY1 and DIRECTORY2 don't have an ancestor in
    !> ANCESTOR_DIRECTORY, Ediff will merge without ancestor.  The fourth
    !> argument, REGEXP, is "" or a regular expression; only file names
    !> that match the regexp are considered.  Port of
    !> ediff.el:ediff-merge-directories-with-ancestor (the command; also
    !> `edirs_merge_with_ancestor').
    recursive subroutine cmd_ediff_merge_directories_with_ancestor(status)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: f, ff, fff, regexp

        call read_directory_name("Directory A to merge:", f, status, dir=default_directory_name())
        if (.not. status%ok()) return
        call read_next_directory("Directory B to merge:", f, ediff_last_dir_b, ff, status)
        if (.not. status%ok()) return
        call read_next_directory("Ancestor directory:", ff, ediff_last_dir_c, fff, status)
        if (.not. status%ok()) return
        call read_filter(regexp, status)
        if (.not. status%ok()) return
        call ediff_merge_directories_with_ancestor(f, ff, fff, regexp, status)
    end subroutine cmd_ediff_merge_directories_with_ancestor

    module subroutine init_ediff_directories()
        commands%ediff_directories%execute => cmd_ediff_directories
        commands%edirs%execute => cmd_ediff_directories
        commands%ediff_directories3%execute => cmd_ediff_directories3
        commands%edirs3%execute => cmd_ediff_directories3
        commands%ediff_merge_directories%execute => cmd_ediff_merge_directories
        commands%edirs_merge%execute => cmd_ediff_merge_directories
        commands%ediff_merge_directories_with_ancestor%execute => cmd_ediff_merge_directories_with_ancestor
        commands%edirs_merge_with_ancestor%execute => cmd_ediff_merge_directories_with_ancestor
        call init_ediff_multiple()
    end subroutine init_ediff_directories
end submodule ediff_directories_mod
