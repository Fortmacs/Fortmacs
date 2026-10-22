! data_fm.f90 -- primitive operations on values and symbols.
! Ported from GNU Emacs 31.1 src/data.c.
!
! Copyright (C) 1985-1986, 1988, 1993-1995, 1997-2026 Free Software
! Foundation, Inc.
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

!> Type checks, `eq`, list access, and global symbol values.
!>
!> Wrong-type errors carry the predicate name as detail (upstream's DATA is
!> `(PREDICATE VALUE)`).  Dynamic binding (`let`), buffer-local values,
!> variable watchers and aliases are not here yet.
module data_fm
    use, intrinsic :: iso_fortran_env, only: int64
    use error_fm, only: status_type, set_error, FM_WRONG_TYPE_ARGUMENT, &
                        FM_VOID_VARIABLE, FM_SETTING_CONSTANT
    use lisp_fm, only: fixnump, list, list_elt, list_tail, listp, nil, stringp, symbolp, symbols, &
                       v_buffer, v_int, v_keymap, v_list, v_marker, v_nil, v_symbol, value_type, &
                       vtag, xsymbol
    use lread_fm, only: ensure_obarray
    implicit none(type, external)
    private

    public :: check_fixnum, check_symbol, check_string, check_list
    public :: value_eq, car, cdr, car_safe, cdr_safe
    public :: symbol_name, keywordp, boundp, symbol_value, set, makunbound

contains

    ! ------------------------------------------------------------------
    ! Type checks (lisp.h CHECK_*)
    ! ------------------------------------------------------------------

    subroutine wrong_type(status, predicate)
        type(status_type), intent(inout) :: status
        character(len=*), intent(in) :: predicate

        call set_error(status, FM_WRONG_TYPE_ARGUMENT, predicate)
    end subroutine wrong_type


    !> Port of lisp.h:CHECK_FIXNUM.
    subroutine check_fixnum(v, status)
        type(value_type), intent(in) :: v
        type(status_type), intent(out) :: status

        if (.not. fixnump(v)) call wrong_type(status, "fixnump")
    end subroutine check_fixnum


    !> Port of lisp.h:CHECK_SYMBOL.
    subroutine check_symbol(v, status)
        type(value_type), intent(in) :: v
        type(status_type), intent(out) :: status

        if (.not. symbolp(v)) call wrong_type(status, "symbolp")
    end subroutine check_symbol


    !> Port of lisp.h:CHECK_STRING.
    subroutine check_string(v, status)
        type(value_type), intent(in) :: v
        type(status_type), intent(out) :: status

        if (.not. stringp(v)) call wrong_type(status, "stringp")
    end subroutine check_string


    !> Port of lisp.h:CHECK_LIST.
    subroutine check_list(v, status)
        type(value_type), intent(in) :: v
        type(status_type), intent(out) :: status

        if (.not. listp(v)) call wrong_type(status, "listp")
    end subroutine check_list


    ! ------------------------------------------------------------------
    ! Identity and lists
    ! ------------------------------------------------------------------

    !> `eq`: same integer, symbol or object handle.  Strings and lists have
    !> no identity once copied, so they are never `eq` (upstream: only the
    !> very same object is).  Port of data.c:Feq.
    elemental logical function value_eq(a, b)
        type(value_type), intent(in) :: a, b

        value_eq = .false.
        if (vtag(a) /= vtag(b)) return

        select case (vtag(a))
        case (V_NIL)
            value_eq = .true.
        case (V_INT, V_SYMBOL)
            value_eq = a%a(1)%i == b%a(1)%i
        case (V_BUFFER, V_MARKER, V_KEYMAP)
            value_eq = a%a(1)%h%index == b%a(1)%h%index .and. &
                       a%a(1)%h%generation == b%a(1)%h%generation
        case default
            ! Nothing to do for other values.
        end select
    end function value_eq


    !> First element of a list; nil for nil.  Port of data.c:Fcar.
    subroutine car(list, v, status)
        type(value_type), intent(in) :: list
        type(value_type), intent(out) :: v
        type(status_type), intent(out) :: status

        select case (vtag(list))
        case (V_LIST)
            v = list_elt(list, 1_int64)
        case (V_NIL)
            v = nil()
        case default
            call wrong_type(status, "listp")
        end select
    end subroutine car


    !> The list without its first element; nil for nil.
    !> Port of data.c:Fcdr.
    subroutine cdr(list, v, status)
        type(value_type), intent(in) :: list
        type(value_type), intent(out) :: v
        type(status_type), intent(out) :: status

        select case (vtag(list))
        case (V_LIST)
            v = list_tail(list, 1_int64)
        case (V_NIL)
            v = nil()
        case default
            call wrong_type(status, "listp")
        end select
    end subroutine cdr


    !> Like `car`, but nil for a non-list.  Port of data.c:Fcar_safe.
    pure function car_safe(list) result(v)
        type(value_type), intent(in) :: list
        type(value_type) :: v

        if (vtag(list) == V_LIST) v = list_elt(list, 1_int64)
    end function car_safe


    !> Like `cdr`, but nil for a non-list.  Port of data.c:Fcdr_safe.
    pure function cdr_safe(list) result(v)
        type(value_type), intent(in) :: list
        type(value_type) :: v

        if (vtag(list) == V_LIST) v = list_tail(list, 1_int64)
    end function cdr_safe


    ! ------------------------------------------------------------------
    ! Symbols
    ! ------------------------------------------------------------------

    !> Port of data.c:Fsymbol_name.
    subroutine symbol_name(symbol, name, status)
        type(value_type), intent(in) :: symbol
        character(len=:), allocatable, intent(out) :: name
        type(status_type), intent(out) :: status

        call check_symbol(symbol, status)
        if (.not. status%ok()) return
        call ensure_obarray()
        name = symbols(xsymbol(symbol))%name
    end subroutine symbol_name


    !> True for symbols whose name starts with ':'.  Port of data.c:Fkeywordp.
    logical function keywordp(v)
        type(value_type), intent(in) :: v

        keywordp = .false.
        if (vtag(v) /= V_SYMBOL) return
        call ensure_obarray()

        associate (name => symbols(xsymbol(v))%name)
            if (len(name) > 0) keywordp = name(1:1) == ":"
        end associate
    end function keywordp


    !> Port of data.c:Fboundp (global value only).
    subroutine boundp(symbol, bound, status)
        type(value_type), intent(in) :: symbol
        logical, intent(out) :: bound
        type(status_type), intent(out) :: status

        bound = .false.
        call check_symbol(symbol, status)
        if (.not. status%ok()) return
        call ensure_obarray()
        bound = symbols(xsymbol(symbol))%bound
    end subroutine boundp


    !> The value of `symbol`; void-variable if unbound.
    !> Port of data.c:Fsymbol_value (global value only).
    subroutine symbol_value(symbol, v, status)
        type(value_type), intent(in) :: symbol
        type(value_type), intent(out) :: v
        type(status_type), intent(out) :: status
        integer(int64) :: id

        call check_symbol(symbol, status)
        if (.not. status%ok()) return
        call ensure_obarray()
        id = xsymbol(symbol)

        if (.not. symbols(id)%bound) then
            call set_error(status, FM_VOID_VARIABLE, symbols(id)%name)
            return
        end if

        v = symbols(id)%val
    end subroutine symbol_value


    !> Give `symbol` the value `newval`.  Constants (nil, t, keywords)
    !> signal setting-constant, except a keyword set to itself.
    !> Port of data.c:Fset / set_internal (global value only).
    subroutine set(symbol, newval, status)
        type(value_type), intent(in) :: symbol, newval
        type(status_type), intent(out) :: status
        integer(int64) :: id

        call check_symbol(symbol, status)
        if (.not. status%ok()) return
        call ensure_obarray()
        id = xsymbol(symbol)

        if (symbols(id)%constant) then
            if (.not. (keywordp(symbol) .and. value_eq(newval, symbols(id)%val))) then
                call set_error(status, FM_SETTING_CONSTANT, symbols(id)%name)
            end if

            return
        end if

        symbols(id)%val = newval
        symbols(id)%bound = .true.
    end subroutine set


    !> Make `symbol` unbound.  Port of data.c:Fmakunbound.
    subroutine makunbound(symbol, status)
        type(value_type), intent(in) :: symbol
        type(status_type), intent(out) :: status
        integer(int64) :: id

        call check_symbol(symbol, status)
        if (.not. status%ok()) return
        call ensure_obarray()
        id = xsymbol(symbol)

        if (symbols(id)%constant) then
            call set_error(status, FM_SETTING_CONSTANT, symbols(id)%name)
            return
        end if

        symbols(id)%val = nil()
        symbols(id)%bound = .false.
    end subroutine makunbound

end module data_fm
