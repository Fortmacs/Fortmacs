! dired_fm.f90 -- directory listings.
! Ported from GNU Emacs 31.1 src/dired.c.
!
! Copyright (C) 1985-1986, 1993-1994, 1999-2026 Free Software
! Foundation, Inc.
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

!> `directory_files`: the names in a directory, as a list of strings.
!>
!> Not ported yet: MATCH (it needs regexps), `directory-files-and-attributes`,
!> file name completion, `file-attributes` as a list.
module dired_fm
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_fm, only: status_type
    use lisp_fm, only: value_type, make_string, make_list, nil
    use fileio_fm, only: expand_file_name, directory_file_name, file_name_as_directory, report_file_errno
    use sysdep_fm, only: sys_string_type, emacs_directory_entries
    implicit none(type, external)
    private

    public :: directory_files

contains

    !> Return a list of names of files in DIRECTORY.
    !> There are three optional arguments:
    !> If FULL is true, return absolute file names.  Otherwise return names
    !>  that are relative to the specified directory.
    !> If NOSORT is true, the list is not sorted--its order is unpredictable.
    !>  Otherwise, the list returned is sorted with `string-lessp'.
    !>  NOSORT is useful if you plan to sort the result yourself.
    !> If COUNT is present and a natural number, the function will return
    !>  COUNT number of file names (if so many are present).  Port of
    !> dired.c:Fdirectory_files and directory_files_internal (no MATCH).
    subroutine directory_files(directory, files, status, full, nosort, count)
        character(len=*), intent(in) :: directory
        type(value_type), intent(out) :: files
        type(status_type), intent(out) :: status
        logical, intent(in), optional :: full, nosort
        integer(int64), intent(in), optional :: count
        character(len=:), allocatable :: dir, dirfilename
        type(sys_string_type), allocatable :: names(:)
        type(value_type), allocatable :: elems(:)
        integer, allocatable :: order(:)
        integer(int32) :: err
        integer :: n, k, j, key
        logical :: absolute, sorted

        files = nil()
        absolute = .false.
        if (present(full)) absolute = full
        sorted = .true.
        if (present(nosort)) sorted = .not. nosort
        call expand_file_name(directory, dir, status)
        if (.not. status%ok()) return
        dirfilename = directory_file_name(dir)
        err = emacs_directory_entries(dirfilename, names)

        if (err /= 0) then
            call report_file_errno("Opening directory", dirfilename, err, status)
            return
        end if

        n = size(names)

        if (present(count)) then
            if (count >= 0) n = int(min(count, int(n, int64)))
        end if

        order = [(k, k=1, n)]

        ! Sorted with string-lessp (by character codes): an insertion sort.
        if (sorted) then
            do k = 2, n
                key = order(k)
                j = k - 1

                do while (j >= 1)
                    if (.not. lessp(names(key)%s, names(order(j))%s)) exit
                    order(j + 1) = order(j)
                    j = j - 1
                end do

                order(j + 1) = key
            end do
        end if

        allocate (elems(n))

        do k = 1, n
            if (absolute) then
                elems(k) = make_string(file_name_as_directory(dirfilename)//names(order(k))%s)
            else
                elems(k) = make_string(names(order(k))%s)
            end if
        end do

        files = make_list(elems)
    end subroutine directory_files

    !> Whether `a` sorts before `b`, comparing bytes (UTF-8 sorts as its
    !> code points do) with a prefix first.  As fns.c:Fstring_lessp.
    pure logical function lessp(a, b)
        character(len=*), intent(in) :: a, b
        integer :: k

        do k = 1, min(len(a), len(b))
            if (a(k:k) /= b(k:k)) then
                lessp = iachar(a(k:k)) < iachar(b(k:k))
                return
            end if
        end do

        lessp = len(a) < len(b)
    end function lessp
end module dired_fm
