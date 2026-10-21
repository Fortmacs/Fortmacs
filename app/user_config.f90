! user_config.f90 -- the default configuration: none.
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

!> The configuration built in when there is no init.f90: `configure`
!> changes nothing.  A user's ~/.config/fortmacs/init.f90 replaces this
!> module (see the `fortmacs` module for the API).
module user_config
    use fortmacs, only: status_type
    implicit none(type, external)
    private

    public :: configure

contains

    !> Configure the editor at startup; here, leave every default alone.
    subroutine configure(status)
        type(status_type), intent(out) :: status

        status = status_type()
    end subroutine configure

end module user_config
