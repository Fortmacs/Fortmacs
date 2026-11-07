! sample.f90 -- the fixture of tools/test-hazards.sh: components of
! command_type and strings in array constructors that tools/hazards.awk
! must and must not report.
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
        ! Not reported: intrinsic, flat and pointer components; reported:
        character(len=:), allocatable :: name  ! an allocatable string
        integer :: macro = 0
        type(flat_type), allocatable :: flats(:)  ! and a flat allocatable array
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

    ! Reported: a string longer than the constructor's length.
    character(len=4), parameter :: short(2) = [character(len=4) :: "abcd", "abcde"]
    ! Not reported: strings that fit (a doubled quote is one character).
    character(len=4), parameter :: fit(3) = [character(len=4) :: "ab""c", 'abcd', &
                                             "a"]

    type, public :: store_type
        type(value_type), allocatable :: values(:)
    contains
        procedure :: get => get_values
    end type store_type

contains

    ! An allocatable array of a type with allocatable components.
    function make_values() result(values)
        type(value_type), allocatable :: values(:)

        allocate (values(2))
    end function make_values

    ! The same through a binding.
    function get_values(self) result(values)
        class(store_type), intent(in) :: self
        type(value_type), allocatable :: values(:)

        values = self%values
    end function get_values

    ! An allocatable array of a type without allocatable components.
    function make_flats() result(flats)
        type(flat_type), allocatable :: flats(:)

        allocate (flats(2))
    end function make_flats

    subroutine inquire(store, n)
        type(store_type), intent(in) :: store
        integer, intent(out) :: n
        type(value_type), allocatable :: kept(:)

        ! Reported: the size of such results, by name and by binding.
        n = size(make_values())
        n = size(store%get())
        ! Not reported: the result assigned first, a flat type.
        kept = make_values()
        n = size(kept) + size(make_flats())
    end subroutine inquire

    subroutine lengths(name)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: names(:)

        ! Reported: a string first, of a length known at run time.
        call take([character(len=len(name) + 32) :: "/does/not/exist", name])
        ! Not reported: a variable first, strings only, a constant length.
        call take([character(len=len(name)) :: name, "a"])
        call take([character(len=len(name)) :: "a", "b"])
        call take([character(len=40) :: "a", name])
        names = [character(len=len(name)) :: name]
    end subroutine lengths
end module sample
