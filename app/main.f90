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

!> Entry point: runs the editor (`run' in emacs_fm) with the built-in
!> configuration, which changes nothing.  `fortmacs --recompile' builds
!> the user's init.f90 into a binary of its own (see config_fm), which an
!> installed fortmacs runs instead.
program fortmacs_main
    use emacs_fm, only: run
    ! Written against the API like an init.f90 (which also keeps fpm from
    ! pruning the `fortmacs` module out of an install).
    use fortmacs, only: status_type
    implicit none(type, external)
    integer :: exit_code
    call run(configure, exit_code)
    if (exit_code /= 0) stop exit_code, quiet = .true.
contains

    !> The default configuration: leave every default alone.
    subroutine configure(status)
        type(status_type), intent(out) :: status
        status = status_type()
    end subroutine configure
end program fortmacs_main
