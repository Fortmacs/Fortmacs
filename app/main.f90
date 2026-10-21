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

!> Entry point: --version and --help are answered here (as emacs.c does);
!> otherwise the editor runs in the terminal, visiting the files named.
program fortmacs
    use, intrinsic :: iso_fortran_env, only: output_unit, error_unit, int32
    use fm_version, only: version_string
    use fm_error, only: status_type, format_error
    use fm_sysdep, only: emacs_isatty
    use fm_emacs, only: emacs_main, arg_type
    implicit none(type, external)

    character(len=*), parameter :: usage = "Usage: fortmacs [--version | --help] [+LINE] [FILE]..."
    character(len=:), allocatable :: term
    type(arg_type), allocatable :: args(:)
    integer :: nargs, arglen, stat, k
    type(status_type) :: status

    nargs = command_argument_count()
    allocate (args(nargs))
    do k = 1, nargs
        call get_command_argument(k, length=arglen)
        allocate (character(len=arglen) :: args(k)%s)
        call get_command_argument(k, value=args(k)%s)
    end do

    if (nargs > 0) then
        select case (args(1)%s)
        case ("--version", "-version")
            write (output_unit, "(a)") version_string()
            stop
        case ("--help", "-help")
            write (output_unit, "(a)") usage
            write (output_unit, "(a)") "Edit the FILEs (or *scratch*) in the terminal; C-x C-s saves, C-x C-c quits."
            write (output_unit, "(a)") "+LINE puts point on line LINE of the next FILE."
            stop
        case default
            ! Everything else is for startup.el:command-line-1.
        end select
    end if

    ! emacs.c:main: a terminal is needed on standard input.
    if (.not. emacs_isatty(0_int32)) then
        write (error_unit, "(a)") "fortmacs: standard input is not a tty"
        stop 1, quiet = .true.
    end if
    call get_environment_variable("TERM", length=arglen, status=stat)
    if (stat /= 0 .or. arglen == 0) then
        write (error_unit, "(a)") "fortmacs: please set the environment variable TERM"
        stop 1, quiet = .true.
    end if
    allocate (character(len=arglen) :: term)
    call get_environment_variable("TERM", term)
    call emacs_main(term, 0_int32, status, args)
    if (.not. status%ok()) then
        write (error_unit, "(a)") "fortmacs: "//format_error(status)
        stop 1, quiet = .true.
    end if
end program fortmacs
