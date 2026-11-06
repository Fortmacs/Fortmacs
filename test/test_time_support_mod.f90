! test_time_support_mod.f90 -- helpers shared by the date and time tests.
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

!> A decoded time as text, for the tests of time_date_mod, iso8601_mod
!> and parse_time_mod to compare with what upstream's tests expect.
module test_time_support_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use time_date_mod, only: partial_decoded_time_type
    implicit none(type, external)
    private

    public :: render_partial_decoded_time

contains

    !> The known components of TIME as "second=30 minute=47 hour=13
    !> day=2 month=3 year=2008 daylight_saving_time=-1 zone=3600", in that order (and
    !> weekday before daylight_saving_time); a second with a fraction is TICKS/COUNT_RATE,
    !> and daylight_saving_time is always there.  Not a port.
    function render_partial_decoded_time(time) result(text)
        type(partial_decoded_time_type), intent(in) :: time
        character(len=:), allocatable :: text
        character(len=48) :: buffer

        text = ""

        if (allocated(time%second)) then
            if (time%second%count == 0) then
                write (buffer, "(a, i0)") "second=", time%second%seconds
            else
                write (buffer, "(a, i0, '/', i0)") "second=", time%second%seconds*time%second%count_rate + &
                    time%second%count, time%second%count_rate
            end if

            text = text//trim(buffer)//" "
        end if

        if (allocated(time%minute)) call add("minute=", int(time%minute, int64))
        if (allocated(time%hour)) call add("hour=", int(time%hour, int64))
        if (allocated(time%day)) call add("day=", int(time%day, int64))
        if (allocated(time%month)) call add("month=", int(time%month, int64))
        if (allocated(time%year)) call add("year=", time%year)
        if (allocated(time%weekday)) call add("weekday=", int(time%weekday, int64))
        call add("daylight_saving_time=", int(time%daylight_saving_time, int64))
        if (allocated(time%zone)) call add("zone=", int(time%zone, int64))
        text = text(1:len(text) - 1)

    contains

        ! Append NAME and VALUE, then a space.
        subroutine add(name, value)
            character(len=*), intent(in) :: name
            integer(int64), intent(in) :: value

            write (buffer, "(a, i0)") name, value
            text = text//trim(buffer)//" "
        end subroutine add
    end function render_partial_decoded_time
end module test_time_support_mod
