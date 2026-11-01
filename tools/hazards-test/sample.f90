! sample.f90 -- the fixture of tools/test-hazards.sh: components of
! command_type that tools/hazards.awk must and must not report.
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

module sample
    implicit none(type, external)
    private

    type :: flat_type
        integer :: index = 0
    end type flat_type

    type :: value_type
        character(len=:), allocatable :: text
    end type value_type

    type :: holder_type
        type(value_type) :: v
    end type holder_type

    type, public :: command_type
        ! Not reported: intrinsic, flat and pointer components.
        character(len=:), allocatable :: name
        integer :: macro = 0
        type(flat_type), allocatable :: flats(:)
        type(value_type), pointer :: shared => null()
        ! Reported: allocatable components, directly or nested.
        type(value_type), allocatable :: events(:)
        type(holder_type) :: holder
        ! Not reported: a justified exception.
        ! hazard-ok(command-component): the fixture's exception
        type(value_type) :: excused
    end type command_type

    ! Not reported: another type.
    type, public :: other_type
        type(value_type), allocatable :: events(:)
    end type other_type
end module sample
