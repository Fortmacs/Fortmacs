! fm_lread.f90 -- the obarray: interning symbols by name.
! Ported from GNU Emacs 31.1 src/lread.c (the obarray part; there is no
! Lisp reader).
!
! Copyright (C) 1985-1989, 1993-1995, 1997-2026 Free Software Foundation,
! Inc.
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

!> One global obarray, an open-addressing hash table of symbol ids.
!> Symbols are never removed (upstream's `unintern` is not ported).
!> Names starting with ':' are keywords: constants whose value is
!> themselves, as in lread.c:intern_sym.
module fm_lread
    use, intrinsic :: iso_fortran_env, only: int64
    use fm_lisp, only: symbols, nsymbols, symbol_type, value_type, &
                       make_symbol_ref, nil, SYM_NIL, SYM_T
    implicit none(type, external)
    private

    public :: intern, intern_soft, sym, ensure_obarray

    !> Hash slots holding symbol ids (0 = empty); size is a power of two.
    integer(int64), allocatable :: slots(:)

contains

    !> Hash of `name` (polynomial, kept below 2**31 so it cannot overflow).
    pure integer(int64) function hash_name(name)
        character(len=*), intent(in) :: name
        integer :: k

        hash_name = 0
        do k = 1, len(name)
            hash_name = mod(hash_name*31 + ichar(name(k:k), int64), 2147483647_int64)
        end do
    end function hash_name

    !> Slot where `name` is, or the empty slot where it would go.
    integer(int64) function find_slot(name)
        character(len=*), intent(in) :: name
        integer(int64) :: mask, id

        mask = size(slots, kind=int64) - 1
        find_slot = iand(hash_name(name), mask) + 1
        do
            id = slots(find_slot)
            if (id == 0) return
            if (len(symbols(id)%name) == len(name)) then
                if (symbols(id)%name == name) return
            end if
            find_slot = iand(find_slot, mask) + 1
        end do
    end function find_slot

    !> Create the tables and the symbols nil and t.
    !> Port of lread.c:init_obarray_once.
    subroutine init_obarray()
        integer(int64) :: id

        allocate (slots(64))
        slots = 0
        allocate (symbols(32))
        nsymbols = 0
        id = add_symbol("nil")
        symbols(id)%constant = .true.
        symbols(id)%bound = .true.
        symbols(id)%val = nil()
        id = add_symbol("t")
        symbols(id)%constant = .true.
        symbols(id)%bound = .true.
        symbols(id)%val = make_symbol_ref(SYM_T)
    end subroutine init_obarray

    !> Append a new symbol named `name` and index it.
    integer(int64) function add_symbol(name) result(id)
        character(len=*), intent(in) :: name
        type(symbol_type), allocatable :: grown(:)

        if (nsymbols == size(symbols, kind=int64)) then
            allocate (grown(2*nsymbols))
            grown(1:nsymbols) = symbols(1:nsymbols)
            call move_alloc(grown, symbols)
        end if
        ! Keep the table at most half full.
        if (2*(nsymbols + 1) > size(slots, kind=int64)) call rehash(2*size(slots, kind=int64))

        nsymbols = nsymbols + 1
        id = nsymbols
        symbols(id) = symbol_type()
        symbols(id)%name = name
        slots(find_slot(name)) = id

        ! Keywords are constants that evaluate to themselves.
        if (len(name) > 0) then
            if (name(1:1) == ":") then
                symbols(id)%constant = .true.
                symbols(id)%bound = .true.
                symbols(id)%val = make_symbol_ref(id)
            end if
        end if
    end function add_symbol

    subroutine rehash(new_size)
        integer(int64), intent(in) :: new_size
        integer(int64) :: id

        deallocate (slots)
        allocate (slots(new_size))
        slots = 0
        do id = 1, nsymbols
            slots(find_slot(symbols(id)%name)) = id
        end do
    end subroutine rehash

    !> Create nil and t if no symbol exists yet.  Anything that indexes
    !> `symbols` without interning first must call this.
    subroutine ensure_obarray()
        if (.not. allocated(slots)) call init_obarray()
    end subroutine ensure_obarray

    !> Id of the symbol named `name`, creating it if needed.
    !> Port of lread.c:Fintern (initial obarray).
    integer(int64) function intern(name) result(id)
        character(len=*), intent(in) :: name

        if (.not. allocated(slots)) call init_obarray()
        id = slots(find_slot(name))
        if (id == 0) id = add_symbol(name)
    end function intern

    !> Id of the symbol named `name`, or 0 if there is none.
    !> Port of lread.c:Fintern_soft.
    integer(int64) function intern_soft(name) result(id)
        character(len=*), intent(in) :: name

        if (.not. allocated(slots)) call init_obarray()
        id = slots(find_slot(name))
    end function intern_soft

    !> The symbol named `name` as a value (nil for "nil"); the Fortmacs
    !> spelling of 'name.  Interns it if needed.
    function sym(name) result(v)
        character(len=*), intent(in) :: name
        type(value_type) :: v

        v = make_symbol_ref(intern(name))
    end function sym

end module fm_lread
