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
    use minibuffer_fm, only: read_file_name
    implicit none(type, external)

contains

    recursive module subroutine read_file_name_in_minibuffer(prompt, result, status, dir, &
                                                             default_filename, initial)
        character(len=*), intent(in) :: prompt
        character(len=:), allocatable, intent(out) :: result
        type(status_type), intent(out) :: status
        character(len=*), intent(in), optional :: dir, default_filename, initial

        call read_file_name(prompt, result, status, dir, default_filename, initial)
    end subroutine read_file_name_in_minibuffer

end submodule files_read_fm
