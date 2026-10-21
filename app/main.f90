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

!> Entry point: runs the editor (`run` in emacs_fm) with the
!> configuration linked in, the `configure` of module `user_config`.
program fortmacs_main
    use emacs_fm, only: run
    use user_config, only: configure
    implicit none(type, external)

    integer :: exit_code

    call run(configure, exit_code)
    if (exit_code /= 0) stop exit_code, quiet = .true.
end program fortmacs_main
