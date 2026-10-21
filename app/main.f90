! main.f90 -- Fortmacs entry point.
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

!> Entry point.  Only `--version` and `--help` work so far; the editor
!> itself arrives with the command loop and terminal display.
program fortmacs
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
    use fm_version, only: version_string
    implicit none(type, external)

    character(len=:), allocatable :: arg
    integer :: nargs, arglen

    nargs = command_argument_count()
    if (nargs == 0) then
        write (output_unit, "(a)") version_string()
        write (output_unit, "(a)") "Not usable as an editor yet; see PORTING.md."
        stop
    end if

    call get_command_argument(1, length=arglen)
    allocate (character(len=arglen) :: arg)
    call get_command_argument(1, value=arg)

    select case (arg)
    case ("--version")
        write (output_unit, "(a)") version_string()
    case ("--help")
        write (output_unit, "(a)") "Usage: fortmacs [--version | --help]"
    case default
        write (error_unit, "(a)") "fortmacs: unknown option '"//arg//"'"
        write (error_unit, "(a)") "Usage: fortmacs [--version | --help]"
        stop 2
    end select
end program fortmacs
