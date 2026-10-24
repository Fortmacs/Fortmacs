! files_read_fm.f90 -- reading file names for files.el commands.
!
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

!> Part of files_fm.  A submodule can use minibuffer_fm even though it
!> depends on files_fm, which a module cannot.
submodule(files_fm) files_read_fm
    use minibuffer_fm, only: read_file_name, REQUIRE_MATCH_NIL, REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION, &
                             read_file_name_completion_ignore_case
    use fns_fm, only: compare_strings, COMPARE_EQUAL
    implicit none(type, external)

contains

    !> Convert FILENAME to be relative to DIRECTORY (default:
    !> `default_directory').
    !> This function returns a relative file name that is equivalent to
    !> FILENAME when used with that default directory as the default.
    !> If FILENAME is a relative file name, it will be interpreted as
    !> existing in `default_directory'.  Port of files.el:file-relative-name
    !> (no remote files; case is folded when
    !> `read_file_name_completion_ignore_case' is true).
    module procedure file_relative_name
    character(len=:), allocatable :: dir, file, file_dir, ancestor, base, rest
    type(status_type) :: st
    logical :: fold
    if (present(directory)) then
        base = directory
    else
        base = ""
        if (cur /= 0) then
            if (allocated(buffers(cur)%directory)) base = buffers(cur)%directory
        end if
    end if
    call expand_file_name(base, dir, st)
    dir = file_name_as_directory(dir)
    call expand_file_name(filename, file, st)
    fold = read_file_name_completion_ignore_case
    ancestor = "."
    file_dir = file_name_as_directory(file)
    do while (.not. (prefix_p(dir, file_dir) .or. prefix_p(dir, file)))
        dir = file_name_directory(dir(1:len(dir) - 1))
        if (ancestor == ".") then
            ancestor = ".."
        else
            ancestor = "../"//ancestor
        end if
    end do
    ! Now ancestor is empty, or .., or ../.., etc.
    if (prefix_p(dir, file)) then
        ! We matched within FILENAME's directory part.  Add the rest of
        ! FILENAME onto ANCESTOR.
        rest = file(len(dir) + 1:)
        if (ancestor == "." .and. len(rest) > 0) then
            ! But don't bother with ANCESTOR if it would give us `./'.
            rel = rest
        else
            rel = file_name_as_directory(ancestor)//rest
        end if
    else
        ! We matched FILENAME's directory equivalent.
        rel = ancestor
    end if
contains

    !> subr.el:string-prefix-p.
    logical function prefix_p(prefix, string)
        character(len=*), intent(in) :: prefix, string
        prefix_p = .false.
        if (len(string) < len(prefix)) return
        if (fold) then
            prefix_p = compare_strings(prefix, string(1:len(prefix)), ignore_case=.true.) == COMPARE_EQUAL
        else
            prefix_p = string(1:len(prefix)) == prefix
        end if
    end function prefix_p
    end procedure file_relative_name

    recursive module subroutine read_file_name_in_minibuffer(prompt, result, status, dir, &
                                                             default_filename, initial, confirm_nonexistent)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: dir, default_filename, initial
        logical, intent(in), optional :: confirm_nonexistent
        integer :: mustmatch
        mustmatch = REQUIRE_MATCH_NIL
        if (present(confirm_nonexistent)) then
            if (confirm_nonexistent) mustmatch = REQUIRE_MATCH_CONFIRM_AFTER_COMPLETION
        end if
        call read_file_name(prompt, result, status, dir, default_filename, mustmatch=mustmatch, initial=initial)
    end subroutine read_file_name_in_minibuffer
end submodule files_read_fm
