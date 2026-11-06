! buffer_local_setting_mod.f90 -- the buffer-local bookkeeping of setting objects.
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

!> The parent of the setting objects whose buffers can have their own
!> values and whose value is not one of the forwarded kinds data_mod
!> swaps with the buffer: a procedure of a given interface
!> (`blink_matching_check_function', `fill_paragraph_function'), or a
!> value of a type of its own (`electric_indent_characters',
!> `skeleton_pairs').  Not a port: upstream's buffer-local variables
!> (data.c's SYMBOL_LOCALIZED) hold any Lisp value.
!>
!> Fortran has no procedure pointer component of a generic interface,
!> so each interface gets an extension of `buffer_local_setting_type'
!> holding the global value (`procedure' for a procedure setting) and
!> the buffers' own values (`locals', entry K for the buffer whose
!> serial number is `serials(K)'); this parent keeps the serial numbers
!> and finds, makes and clears the entries, reusing the entries of
!> closed buffers.  Each extension binds `set_local', `get_current',
!> and the two hooks this type defers: `append_local_entry' (grow
!> `locals' by one entry) and `clear_local_entry' (reset entry K).
!>
!>     call blink_matching_check_function%set_local(check)
!>     check_now => blink_matching_check_function%get_current()
!>     call blink_matching_check_function%clear_local()
!>
!> The setting is a module variable, never an element of a registry, so
!> growing `serials' or `locals' does not move `self' (PORTING.md
!> "Hazards", a registry growing under an element), and nothing here
!> creates buffers.
module buffer_local_setting_mod
    use, intrinsic :: iso_fortran_env, only: int64
    use buffer_mod, only: buffers, current, buffer_serial_live
    implicit none(type, external)
    private

    !> The buffer-local bookkeeping of a setting object (see the module).
    type, abstract, public :: buffer_local_setting_type
        !> The serial numbers of the buffers with their own value, 0 for
        !> a free entry; `locals(K)' of the extension is that of
        !> `serials(K)'.
        integer(int64), allocatable :: serials(:)
    contains
        !> The index of the current buffer's own entry, 0 if it has none.
        procedure :: get_local_index => get_buffer_local_index
        !> Find or make the current buffer's own entry, its index K (0
        !> without a current buffer).
        procedure :: make_local_entry => make_buffer_local_entry
        !> Whether the current buffer has its own value.
        procedure :: is_local => is_buffer_local
        !> Make the current buffer use the global value again (also
        !> what `clear_all_local_variables' does to it).
        procedure :: clear_local => clear_buffer_local
        !> Grow the extension's `locals' by one entry.
        procedure(append_local_entry_procedure), deferred :: append_local_entry
        !> Give entry K of the extension's `locals' its initial value.
        procedure(clear_local_entry_procedure), deferred :: clear_local_entry
    end type buffer_local_setting_type

    abstract interface
        !> Grow the extension's `locals' by one entry.  Not a port.
        subroutine append_local_entry_procedure(self)
            import :: buffer_local_setting_type
            implicit none(type, external)
            class(buffer_local_setting_type), intent(inout) :: self
        end subroutine append_local_entry_procedure

        !> Give entry K of the extension's `locals' its initial value.
        !> Not a port.
        subroutine clear_local_entry_procedure(self, k)
            import :: buffer_local_setting_type
            implicit none(type, external)
            class(buffer_local_setting_type), intent(inout) :: self
            integer, intent(in) :: k
        end subroutine clear_local_entry_procedure
    end interface

contains

    !> The index in `serials' of the current buffer's own entry, 0 if it
    !> has none.  Not a port.
    integer function get_buffer_local_index(self) result(k)
        class(buffer_local_setting_type), intent(in) :: self

        k = 0
        if (current == 0) return
        if (.not. allocated(self%serials)) return
        k = findloc(self%serials, buffers(current)%serial, dim=1)
    end function get_buffer_local_index

    !> Find or make the current buffer's own entry, reusing the entry of a
    !> closed buffer; K is its index (0 without a current buffer).  A new
    !> entry starts with the initial value `clear_local_entry' gives it.
    !> Not a port.
    subroutine make_buffer_local_entry(self, k)
        class(buffer_local_setting_type), intent(inout) :: self
        integer, intent(out) :: k

        k = self%get_local_index()
        if (k > 0 .or. current == 0) return
        if (.not. allocated(self%serials)) allocate (self%serials(0))

        do k = 1, size(self%serials)
            if (.not. buffer_serial_live(self%serials(k))) exit
        end do

        if (k > size(self%serials)) then
            self%serials = [self%serials, 0_int64]
            call self%append_local_entry()
        end if

        call self%clear_local_entry(k)
        self%serials(k) = buffers(current)%serial
    end subroutine make_buffer_local_entry

    !> Whether the current buffer has its own value.  Not a port.
    logical function is_buffer_local(self)
        class(buffer_local_setting_type), intent(in) :: self

        is_buffer_local = self%get_local_index() > 0
    end function is_buffer_local

    !> Make the current buffer use the global value again (also what
    !> `clear_all_local_variables' does to it).  Not a port.
    subroutine clear_buffer_local(self)
        class(buffer_local_setting_type), intent(inout) :: self
        integer :: k

        k = self%get_local_index()
        if (k == 0) return
        self%serials(k) = 0
        call self%clear_local_entry(k)
    end subroutine clear_buffer_local
end module buffer_local_setting_mod
