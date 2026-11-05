! project_cross_reference_mod.f90 -- the project commands that search with xref.
! Ported from GNU Emacs 31.1 lisp/progmodes/project.el (in part:
! project-find-regexp and project-or-external-find-regexp).
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

!> `find_project_regular_expression' and `find_project_or_external_regular_expression': a
!> submodule because they use cross_reference_mod, which uses project_mod.
submodule(project_mod) project_cross_reference_mod
    use cross_reference_mod, only: cross_reference_fetcher_type, cross_reference_item_type, cross_reference_matches_in_files, &
                                   show_cross_references, CROSS_REFERENCE_DISPLAY_ACTION_NONE
    use grep_mod, only: grep_read_files
    implicit none(type, external)

    ! The fetcher of the search commands: the matches for REGULAR_EXPRESSION in FILES
    ! (relative to DIRECTORY when not absolute).  Port of the
    ! (apply-partially #'project--find-regexp-in-files REGULAR_EXPRESSION FILES) of
    ! project.el.
    type, extends(cross_reference_fetcher_type) :: project_regular_expression_fetcher_type
        character(len=:), allocatable :: regular_expression
        character(len=:), allocatable :: directory
        type(string_type), allocatable :: files(:)
    contains
        procedure :: fetch => find_regular_expression_in_files
    end type project_regular_expression_fetcher_type

contains

    module procedure find_project_regular_expression
    type(project_regular_expression_fetcher_type) :: fetcher
    class(project_type), allocatable :: pr
    type(handle_type) :: buffer
    type(string_type) :: none(0)
    character(len=:), allocatable :: caller_directory, saved_directory, directory, patterns
    logical :: saved_relative

    caller_directory = current_directory()
    call get_current_project(pr, status, maybe_prompt=.true.)
    if (.not. status%ok()) return
    fetcher%regular_expression = regular_expression
    fetcher%directory = pr%root()

    if (current_prefix_argument%kind == PREFIX_NONE) then
        saved_directory = current_directory()
        call set_directory(fetcher%directory)
        saved_relative = project_files_relative_names
        project_files_relative_names = .true.
        call pr%list_files(fetcher%files, status)
        project_files_relative_names = saved_relative
        call set_directory(saved_directory)
        if (.not. status%ok()) return
    else
        call read_directory_name("Base directory: ", directory, status, directory=caller_directory, mustmatch=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        fetcher%directory = file_name_as_directory(directory)
        call grep_read_files(regular_expression, patterns, status)
        if (.not. status%ok()) return
        saved_directory = current_directory()
        call set_directory(fetcher%directory)
        saved_relative = project_files_relative_names
        project_files_relative_names = .true.
        call project_files_in_directory(directory, none, fetcher%files, status, patterns)
        project_files_relative_names = saved_relative
        call set_directory(saved_directory)
        if (.not. status%ok()) return
    end if

    call show_regular_expression_matches(fetcher, buffer, status)
    end procedure find_project_regular_expression

    module procedure find_project_or_external_regular_expression
    type(project_regular_expression_fetcher_type) :: fetcher
    class(project_type), allocatable :: pr
    type(handle_type) :: buffer
    type(string_type), allocatable :: roots(:), external(:)
    character(len=:), allocatable :: saved_directory
    integer :: k

    call get_current_project(pr, status, maybe_prompt=.true.)
    if (.not. status%ok()) return
    fetcher%regular_expression = regular_expression
    fetcher%directory = pr%root()
    allocate (roots(0))
    call append_string(roots, pr%root())
    external = pr%external_roots(status)
    if (.not. status%ok()) return

    do k = 1, size(external)
        call append_string(roots, external(k)%text)
    end do

    saved_directory = current_directory()
    call set_directory(fetcher%directory)
    call pr%list_files(fetcher%files, status, roots)
    call set_directory(saved_directory)
    if (.not. status%ok()) return
    call show_regular_expression_matches(fetcher, buffer, status)
    end procedure find_project_or_external_regular_expression

    ! Show the matches FETCHER finds with `show_cross_references', from its
    ! directory (upstream's let-bound `default-directory').  Port of the
    ! (xref-show-xrefs ... nil) of project.el:project-find-regexp.
    recursive subroutine show_regular_expression_matches(fetcher, buffer, status)
        type(project_regular_expression_fetcher_type), intent(inout) :: fetcher
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: saved_directory

        saved_directory = current_directory()
        call set_directory(fetcher%directory)
        call show_cross_references(fetcher, CROSS_REFERENCE_DISPLAY_ACTION_NONE, buffer, status)
        call set_directory(saved_directory)
    end subroutine show_regular_expression_matches

    ! The matches for the fetcher's regexp in its files: a `user_error'
    ! error condition occurs when there are no files or no matches.
    ! Port of project.el:project--find-regexp-in-files.
    recursive subroutine find_regular_expression_in_files(this, items, status)
        class(project_regular_expression_fetcher_type), intent(inout) :: this
        type(cross_reference_item_type), allocatable, intent(out) :: items(:)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: files(:), saved_directory
        integer :: k, width

        allocate (items(0))
        status = status_type()

        if (size(this%files) == 0) then
            call set_error(status, USER_ERROR_CODE, "Empty file list")
            return
        end if

        width = 1

        do k = 1, size(this%files)
            width = max(width, len(this%files(k)%text))
        end do

        allocate (character(len=width) :: files(size(this%files)))

        do k = 1, size(this%files)
            files(k) = this%files(k)%text
        end do

        saved_directory = current_directory()
        call set_directory(this%directory)
        call cross_reference_matches_in_files(this%regular_expression, files, items, status)
        call set_directory(saved_directory)
        if (.not. status%ok()) return

        if (size(items) == 0) then
            call set_error(status, USER_ERROR_CODE, "No matches for: "//this%regular_expression)
            return
        end if
    end subroutine find_regular_expression_in_files

    ! The current buffer's `default_directory' ("./" for none), as
    ! project_mod's private `default_directory' gives it (gfortran does
    ! not export a private procedure of the parent to a submodule).
    ! Not a port.
    function current_directory() result(directory)
        character(len=:), allocatable :: directory

        directory = "./"
        if (current == 0) return
        if (allocated(buffers(current)%directory)) directory = buffers(current)%directory
    end function current_directory

    ! Make DIRECTORY the current buffer's `default_directory'.  Not a port.
    subroutine set_directory(directory)
        character(len=*), intent(in) :: directory

        if (current == 0) return
        buffers(current)%directory = directory
    end subroutine set_directory
end submodule project_cross_reference_mod
