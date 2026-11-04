! project_xref_mod.f90 -- the project commands that search with xref.
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

!> `find_project_regexp' and `find_project_or_external_regexp': a
!> submodule because they use xref_mod, which uses project_mod.
submodule(project_mod) project_xref_mod
    use xref_mod, only: xref_fetcher_type, xref_item_type, xref_matches_in_files, show_xrefs, &
                        XREF_DISPLAY_ACTION_NONE
    use grep_mod, only: grep_read_files
    implicit none(type, external)

    ! The fetcher of the search commands: the matches for REGEXP in FILES
    ! (relative to DIRECTORY when not absolute).  Port of the
    ! (apply-partially #'project--find-regexp-in-files REGEXP FILES) of
    ! project.el.
    type, extends(xref_fetcher_type) :: project_regexp_fetcher_type
        character(len=:), allocatable :: regexp
        character(len=:), allocatable :: directory
        type(string_type), allocatable :: files(:)
    contains
        procedure :: fetch => find_regexp_in_files
    end type project_regexp_fetcher_type

contains

    module procedure find_project_regexp
    type(project_regexp_fetcher_type) :: fetcher
    class(project_type), allocatable :: pr
    type(handle_type) :: buffer
    type(string_type) :: none(0)
    character(len=:), allocatable :: caller_dir, saved_dir, dir, patterns
    logical :: saved_relative

    caller_dir = current_directory()
    call get_current_project(pr, status, maybe_prompt=.true.)
    if (.not. status%ok()) return
    fetcher%regexp = regexp
    fetcher%directory = pr%root()

    if (current_prefix_arg%kind == PREFIX_NONE) then
        saved_dir = current_directory()
        call set_directory(fetcher%directory)
        saved_relative = project_files_relative_names
        project_files_relative_names = .true.
        call pr%list_files(fetcher%files, status)
        project_files_relative_names = saved_relative
        call set_directory(saved_dir)
        if (.not. status%ok()) return
    else
        call read_directory_name("Base directory: ", dir, status, dir=caller_dir, mustmatch=REQUIRE_MATCH_YES)
        if (.not. status%ok()) return
        fetcher%directory = file_name_as_directory(dir)
        call grep_read_files(regexp, patterns, status)
        if (.not. status%ok()) return
        saved_dir = current_directory()
        call set_directory(fetcher%directory)
        saved_relative = project_files_relative_names
        project_files_relative_names = .true.
        call project_files_in_directory(dir, none, fetcher%files, status, patterns)
        project_files_relative_names = saved_relative
        call set_directory(saved_dir)
        if (.not. status%ok()) return
    end if

    call show_regexp_matches(fetcher, buffer, status)
    end procedure find_project_regexp

    module procedure find_project_or_external_regexp
    type(project_regexp_fetcher_type) :: fetcher
    class(project_type), allocatable :: pr
    type(handle_type) :: buffer
    type(string_type), allocatable :: roots(:), external(:)
    character(len=:), allocatable :: saved_dir
    integer :: k

    call get_current_project(pr, status, maybe_prompt=.true.)
    if (.not. status%ok()) return
    fetcher%regexp = regexp
    fetcher%directory = pr%root()
    allocate (roots(0))
    call append_string(roots, pr%root())
    external = pr%external_roots()

    do k = 1, size(external)
        call append_string(roots, external(k)%text)
    end do

    saved_dir = current_directory()
    call set_directory(fetcher%directory)
    call pr%list_files(fetcher%files, status, roots)
    call set_directory(saved_dir)
    if (.not. status%ok()) return
    call show_regexp_matches(fetcher, buffer, status)
    end procedure find_project_or_external_regexp

    ! Show the matches FETCHER finds with `show_xrefs', from its
    ! directory (upstream's let-bound `default-directory').  Port of the
    ! (xref-show-xrefs ... nil) of project.el:project-find-regexp.
    recursive subroutine show_regexp_matches(fetcher, buffer, status)
        type(project_regexp_fetcher_type), intent(inout) :: fetcher
        type(handle_type), intent(out) :: buffer
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: saved_dir

        saved_dir = current_directory()
        call set_directory(fetcher%directory)
        call show_xrefs(fetcher, XREF_DISPLAY_ACTION_NONE, buffer, status)
        call set_directory(saved_dir)
    end subroutine show_regexp_matches

    ! The matches for the fetcher's regexp in its files: a `user_error'
    ! error condition occurs when there are no files or no matches.
    ! Port of project.el:project--find-regexp-in-files.
    recursive subroutine find_regexp_in_files(this, items, status)
        class(project_regexp_fetcher_type), intent(inout) :: this
        type(xref_item_type), allocatable, intent(out) :: items(:)
        type(status_type), intent(out) :: status
        character(len=:), allocatable :: files(:), saved_dir
        integer :: k, width

        allocate (items(0))
        status = status_type()

        if (size(this%files) == 0) then
            call set_error(status, FM_USER_ERROR, "Empty file list")
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

        saved_dir = current_directory()
        call set_directory(this%directory)
        call xref_matches_in_files(this%regexp, files, items, status)
        call set_directory(saved_dir)
        if (.not. status%ok()) return

        if (size(items) == 0) then
            call set_error(status, FM_USER_ERROR, "No matches for: "//this%regexp)
            return
        end if
    end subroutine find_regexp_in_files

    ! The current buffer's `default_directory' ("./" for none), as
    ! project_mod's private `default_directory' gives it (gfortran does
    ! not export a private procedure of the parent to a submodule).
    ! Not a port.
    function current_directory() result(dir)
        character(len=:), allocatable :: dir

        dir = "./"
        if (current == 0) return
        if (allocated(buffers(current)%directory)) dir = buffers(current)%directory
    end function current_directory

    ! Make DIR the current buffer's `default_directory'.  Not a port.
    subroutine set_directory(dir)
        character(len=*), intent(in) :: dir

        if (current == 0) return
        buffers(current)%directory = dir
    end subroutine set_directory
end submodule project_xref_mod
