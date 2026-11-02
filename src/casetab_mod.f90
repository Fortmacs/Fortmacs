! casetab_mod.f90 -- case tables: downcase, upcase and canonicalize.
! Ported from GNU Emacs 31.1 src/casetab.c, the case parts of
! src/buffer.h and lisp/international/characters.el.
!
! Copyright (C) 1993-1994, 2001-2026 Free Software Foundation, Inc.
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

!> Case tables.  A case table is a char-table (chartab_mod) of subtype
!> "case_table" mapping characters to their lower case; its three extra
!> slots hold the UPCASE, CANONICALIZE and EQUIVALENCES char-tables (see
!> `set_case_table').  Each buffer has a case table, the standard one
!> unless `set_case_table' gave it another (BVAR downcase_table and its
!> companions in buffer_mod; null handles there mean the standard ones,
!> and `set_standard_case_table' first pins the buffers using the old
!> standard table to it, as upstream's buffers hold the table they got).
!>
!> The standard table is made on first use (`standard_case_table'):
!> ASCII as in casetab.c:init_casetab_once, the rest from the Unicode
!> simple case mappings as characters.el sets them (generated into
!> uni_case_mod: the entries that are not the identity, plus the identity
!> entries characters.el makes for every cased character), then
!> `set_case_table' computes its canonicalize and equivalence tables.
!> Before that, `downcase', `upcase' and `case_canon' look the same
!> mappings up in uni_case_mod directly, which gives the same values (and
!> keeps them pure).
module casetab_mod
    use, intrinsic :: iso_fortran_env, only: int32, int64
    use error_mod, only: status_type, set_error, FM_WRONG_TYPE_ARGUMENT
    use handle_mod, only: handle_type, null_handle
    use buffer_mod, only: buffers, current
    use chartab_mod, only: make_char_table, is_char_table, char_table_subtype, char_table_extra_table, &
                           set_char_table_extra_table, char_table_ref, char_table_set, set_char_table_range, &
                           map_char_table, copy_char_table
    use uni_case_mod, only: down_count, down_keys, down_values, up_count, up_keys, up_values, &
                            canon_count, canon_keys, canon_values
    implicit none(type, external)
    private

    public :: downcase, upcase, case_canon, is_uppercase, is_lowercase
    public :: is_case_table, current_case_table, standard_case_table, set_case_table, set_standard_case_table

    !> The standard case table and its companions (casetab.c
    !> Vascii_downcase_table, Vascii_upcase_table, Vascii_canon_table,
    !> Vascii_eqv_table); null until `init_casetab_once' runs.
    type(handle_type) :: ascii_downcase_table, ascii_upcase_table, ascii_canon_table, ascii_eqv_table

    !> An error found while mapping over a case table (`set_canon').
    type(status_type) :: map_status

contains

    ! ------------------------------------------------------------------
    ! Lookups (buffer.h)
    ! ------------------------------------------------------------------

    !> The value for `c' in the sorted table `keys'/`values'; `c' if absent.
    !> Not a port.
    pure integer(int32) function lookup(c, keys, values, n) result(v)
        integer(int32), intent(in) :: c
        integer, intent(in) :: n
        integer(int32), intent(in) :: keys(n), values(n)
        integer :: lo, hi, mid

        v = c
        lo = 1
        hi = n

        do while (lo <= hi)
            mid = (lo + hi)/2

            if (keys(mid) == c) then
                v = values(mid)
                return
            else if (keys(mid) < c) then
                lo = mid + 1
            else
                hi = mid - 1
            end if
        end do
    end function lookup

    !> The current buffer's case table (`which' 1), upcase table (2) or
    !> canonicalize table (3), or the null handle before the standard
    !> table is made.  Not a port (BVAR).
    pure function buffer_table(which) result(h)
        integer, intent(in) :: which
        type(handle_type) :: h

        select case (which)
        case (1)
            h = ascii_downcase_table

            if (current /= 0) then
                if (buffers(current)%downcase_table%index /= 0) h = buffers(current)%downcase_table
            end if
        case (2)
            h = ascii_upcase_table

            if (current /= 0) then
                if (buffers(current)%upcase_table%index /= 0) h = buffers(current)%upcase_table
            end if
        case default
            h = ascii_canon_table

            if (current /= 0) then
                if (buffers(current)%case_canon_table%index /= 0) h = buffers(current)%case_canon_table
            end if
        end select
    end function buffer_table

    !> Downcase a character C, or make no change if that cannot be done.
    !> Port of buffer.h:downcase.
    elemental integer(int32) function downcase(c)
        integer(int32), intent(in) :: c
        type(handle_type) :: h
        integer(int64) :: down

        h = buffer_table(1)

        if (is_char_table(h)) then
            down = char_table_ref(h, c)
            downcase = c
            if (down >= 0) downcase = int(down, int32)
        else if (c < 128) then
            downcase = c
            if (c >= iachar("A") .and. c <= iachar("Z")) downcase = c + 32
        else
            downcase = lookup(c, down_keys, down_values, down_count)
        end if
    end function downcase

    !> Upcase a character C, or make no change if that cannot be done.
    !> Port of buffer.h:upcase.
    elemental integer(int32) function upcase(c)
        integer(int32), intent(in) :: c
        type(handle_type) :: h
        integer(int64) :: up

        h = buffer_table(2)

        if (is_char_table(h)) then
            up = char_table_ref(h, c)
            upcase = c
            if (up >= 0) upcase = int(up, int32)
        else if (c < 128) then
            upcase = c
            if (c >= iachar("a") .and. c <= iachar("z")) upcase = c - 32
        else
            upcase = lookup(c, up_keys, up_values, up_count)
        end if
    end function upcase

    !> The canonical form of `c' for case-insensitive comparison: the
    !> current buffer's case_canon_table entry (as TRANSLATE gives it in
    !> search.c and regex-emacs.c), so that σ, ς and Σ all compare equal.
    !> Port of character.h:char_table_translate on the case canon table.
    elemental integer(int32) function case_canon(c)
        integer(int32), intent(in) :: c
        type(handle_type) :: h
        integer(int64) :: ch

        h = buffer_table(3)

        if (is_char_table(h)) then
            ch = char_table_ref(h, c)
            case_canon = c
            if (ch >= 0) case_canon = int(ch, int32)
        else if (c < 128) then
            case_canon = downcase(c)
        else
            case_canon = lookup(c, canon_keys, canon_values, canon_count)
        end if
    end function case_canon

    !> True if C is upper case.  Port of buffer.h:uppercasep.
    elemental logical function is_uppercase(c)
        integer(int32), intent(in) :: c

        is_uppercase = downcase(c) /= c
    end function is_uppercase

    !> True if C is lower case.  Port of buffer.h:lowercasep.
    elemental logical function is_lowercase(c)
        integer(int32), intent(in) :: c

        is_lowercase = .not. is_uppercase(c) .and. upcase(c) /= c
    end function is_lowercase

    ! ------------------------------------------------------------------
    ! Case tables
    ! ------------------------------------------------------------------

    !> Return true if OBJECT is a case table.
    !> See `set_case_table' for more information on these data structures.
    !> Port of casetab.c:Fcase_table_p.
    logical function is_case_table(object)
        type(handle_type), intent(in) :: object
        type(handle_type) :: up, canon, eqv
        type(status_type) :: status

        is_case_table = .false.
        if (.not. is_char_table(object)) return
        if (char_table_subtype(object, status) /= "case_table") return
        up = char_table_extra_table(object, 0, status)
        if (.not. status%ok()) return
        canon = char_table_extra_table(object, 1, status)
        eqv = char_table_extra_table(object, 2, status)
        is_case_table = up%index == 0 .or. is_char_table(up)

        if (is_case_table) then
            is_case_table = canon%index == 0 .and. eqv%index == 0
            if (.not. is_case_table .and. is_char_table(canon)) is_case_table = eqv%index == 0 .or. is_char_table(eqv)
        end if
    end function is_case_table

    !> A `wrong_type_argument' error condition occurs unless OBJ is a case table.  Port of
    !> casetab.c:check_case_table.
    subroutine check_case_table(obj, status)
        type(handle_type), intent(in) :: obj
        type(status_type), intent(out) :: status

        status = status_type()
        if (.not. is_case_table(obj)) call set_error(status, FM_WRONG_TYPE_ARGUMENT, "expected case table type(char_table_type)")
    end subroutine check_case_table

    !> Return the case table of the current buffer.  Port of
    !> casetab.c:Fcurrent_case_table.
    function current_case_table() result(h)
        type(handle_type) :: h

        h = standard_case_table()
        if (current == 0) return
        if (buffers(current)%downcase_table%index /= 0) h = buffers(current)%downcase_table
    end function current_case_table

    !> Return the standard case table.
    !> This is the one used for new buffers.  Port of
    !> casetab.c:Fstandard_case_table (made on first use).
    function standard_case_table() result(h)
        type(handle_type) :: h

        if (.not. is_char_table(ascii_downcase_table)) call init_casetab_once()
        h = ascii_downcase_table
    end function standard_case_table

    !> Select a new case table for the current buffer.
    !> A case table is a char-table which maps characters
    !> to their lower-case equivalents.  It also has three "extra" slots
    !> which may be additional char-tables or null handles.
    !> These slots are called UPCASE, CANONICALIZE and EQUIVALENCES.
    !> UPCASE maps each non-upper-case character to its upper-case equivalent.
    !>  (The value in UPCASE for an upper-case character is never used.)
    !>  If lower and upper case characters are in 1-1 correspondence,
    !>  you may use a null handle and the upcase table will be deduced from DOWNCASE.
    !> CANONICALIZE maps each character to a canonical equivalent;
    !>  any two characters that are related by case-conversion have the same
    !>  canonical equivalent character; it may be a null handle, in which case it is
    !>  deduced from DOWNCASE and UPCASE.
    !> EQUIVALENCES is a map that cyclically permutes each equivalence class
    !>  (of characters with the same canonical equivalent); it may be a null handle,
    !>  in which case it is deduced from CANONICALIZE.  Port of
    !> casetab.c:Fset_case_table.
    subroutine set_case_table(table, status)
        type(handle_type), intent(in) :: table
        type(status_type), intent(out) :: status

        if (.not. is_char_table(ascii_downcase_table)) call init_casetab_once()
        call set_case_table_1(table, .false., status)
    end subroutine set_case_table

    !> Select a new standard case table for new buffers.
    !> See `set_case_table' for more info on case tables.  Port of
    !> casetab.c:Fset_standard_case_table.
    subroutine set_standard_case_table(table, status)
        type(handle_type), intent(in) :: table
        type(status_type), intent(out) :: status
        integer(int64) :: i

        if (.not. is_char_table(ascii_downcase_table)) call init_casetab_once()
        call check_case_table(table, status)
        if (.not. status%ok()) return

        ! The buffers using the old standard table keep it.
        if (allocated(buffers)) then
            do i = 1, size(buffers, kind=int64)
                if (buffers(i)%downcase_table%index /= 0) cycle
                buffers(i)%downcase_table = ascii_downcase_table
                buffers(i)%upcase_table = ascii_upcase_table
                buffers(i)%case_canon_table = ascii_canon_table
                buffers(i)%case_eqv_table = ascii_eqv_table
            end do
        end if

        call set_case_table_1(table, .true., status)
    end subroutine set_standard_case_table

    !> Fill in the extra slots of case table TABLE that are empty, and
    !> make it the standard case table (STANDARD) or the current buffer's.
    !> Port of casetab.c:set_case_table.
    subroutine set_case_table_1(table, standard, status)
        type(handle_type), intent(in) :: table
        logical, intent(in) :: standard
        type(status_type), intent(out) :: status
        type(handle_type) :: up, canon, eqv
        type(status_type) :: st

        call check_case_table(table, status)
        if (.not. status%ok()) return
        up = char_table_extra_table(table, 0, st)
        canon = char_table_extra_table(table, 1, st)
        eqv = char_table_extra_table(table, 2, st)

        if (up%index == 0) then
            call make_char_table(up, "case_table", st)
            call map_char_table(set_identity, table, st, up)
            call map_char_table(shuffle, table, st, up)
            call set_char_table_extra_table(table, 0, up, st)
        end if

        if (canon%index == 0) then
            call make_char_table(canon, "case_table", st)
            call set_char_table_extra_table(table, 1, canon, st)
            map_status = status_type()
            call map_char_table(set_canon, table, st, table)

            if (.not. map_status%ok()) then
                status = map_status
                return
            end if
        end if

        if (eqv%index == 0) then
            call make_char_table(eqv, "case_table", st)
            call map_char_table(set_identity, canon, st, eqv)
            call map_char_table(shuffle, canon, st, eqv)
            call set_char_table_extra_table(table, 2, eqv, st)
        end if

        ! This is so set_image_of_range_1 in regex-emacs.c can find the EQV
        ! table.
        call set_char_table_extra_table(canon, 2, eqv, st)

        if (standard) then
            ascii_downcase_table = table
            ascii_upcase_table = up
            ascii_canon_table = canon
            ascii_eqv_table = eqv
        else if (current /= 0) then
            buffers(current)%downcase_table = table
            buffers(current)%upcase_table = up
            buffers(current)%case_canon_table = canon
            buffers(current)%case_eqv_table = eqv
        end if
    end subroutine set_case_table_1

    !> Set CANON char-table element for characters in FROM..TO to a
    !> translated ELT by UP and DOWN char-tables.  This is done only when
    !> ELT is a character.  The char-tables CANON, UP, and DOWN are in
    !> CASE_TABLE.  Port of casetab.c:set_canon.
    recursive subroutine set_canon(case_table, from, to, elt)
        type(handle_type), intent(in) :: case_table
        integer, intent(in) :: from, to
        integer(int64), intent(in) :: elt
        type(handle_type) :: up, canon
        type(status_type) :: st
        integer(int64) :: u

        if (elt < 0) return
        up = char_table_extra_table(case_table, 0, st)
        canon = char_table_extra_table(case_table, 1, st)
        u = char_table_ref(up, int(elt))

        if (u < 0) then
            call set_error(map_status, FM_WRONG_TYPE_ARGUMENT, "expected integer (a character code)")
            return
        end if

        call set_char_table_range(canon, char_table_ref(case_table, int(u)), st, from, to)
    end subroutine set_canon

    !> Set elements of char-table TABLE for FROM..TO to themselves.  This
    !> is done only when ELT is a character.  This is called in
    !> `map_char_table'.  Port of casetab.c:set_identity.
    recursive subroutine set_identity(table, from, to, elt)
        type(handle_type), intent(in) :: table
        integer, intent(in) :: from, to
        integer(int64), intent(in) :: elt
        type(status_type) :: st
        integer :: c

        if (elt < 0) return

        do c = from, to
            call char_table_set(table, c, int(c, int64), st)
        end do
    end subroutine set_identity

    !> Permute the elements of TABLE (which is initially an identity
    !> mapping) so that it has one cycle for each equivalence class
    !> induced by the translation table on which `map_char_table' is
    !> operated.  Port of casetab.c:shuffle.
    recursive subroutine shuffle(table, from, to, elt)
        type(handle_type), intent(in) :: table
        integer, intent(in) :: from, to
        integer(int64), intent(in) :: elt
        type(status_type) :: st
        integer(int64) :: tem
        integer :: c

        if (elt < 0) return

        do c = from, to
            tem = char_table_ref(table, int(elt))
            call char_table_set(table, int(elt), int(c, int64), st)
            call char_table_set(table, c, tem, st)
        end do
    end subroutine shuffle

    !> Make the standard case table: ASCII as upstream, then
    !> characters.el's entries from the Unicode case mappings (the ones
    !> that differ from the identity, and the identity entries it makes
    !> for the other cased characters: every character with an upcase
    !> mapping and every lower case form gets a downcase entry, every
    !> upper case form and every character with a downcase mapping an
    !> upcase entry), with its canonicalize and equivalence slots empty,
    !> so that `set_case_table' computes them.  Port of
    !> casetab.c:init_casetab_once, with characters.el's case entries.
    subroutine init_casetab_once()
        type(handle_type) :: down, up, canon, eqv
        type(status_type) :: st
        integer :: i, c

        call make_char_table(down, "case_table", st)

        do i = 0, 127
            c = i
            if (i >= iachar("A") .and. i <= iachar("Z")) c = i + 32
            call char_table_set(down, i, int(c, int64), st)
        end do

        call copy_char_table(down, canon, st)
        call set_char_table_extra_table(down, 1, canon, st)
        call make_char_table(up, "case_table", st)
        call set_char_table_extra_table(down, 0, up, st)

        do i = 0, 127
            c = i
            if (i >= iachar("a") .and. i <= iachar("z")) c = i - 32
            call char_table_set(up, i, int(c, int64), st)
        end do

        call make_char_table(eqv, "case_table", st)

        do i = 0, 127
            c = i

            if (i >= iachar("A") .and. i <= iachar("Z")) then
                c = i + 32
            else if (i >= iachar("a") .and. i <= iachar("z")) then
                c = i - 32
            end if

            call char_table_set(eqv, i, int(c, int64), st)
        end do

        call set_char_table_extra_table(down, 2, eqv, st)
        ! Fill in what isn't filled in.
        call set_case_table_1(down, .true., st)
        ! characters.el: the Unicode entries, then the extra slots cleared
        ! so that they are recomputed from the downcase and upcase tables.
        call characters_case_entries(down, up)
        call set_char_table_extra_table(down, 1, null_handle(), st)
        call set_char_table_extra_table(down, 2, null_handle(), st)
        call set_case_table_1(down, .true., st)
    end subroutine init_casetab_once

    !> characters.el's case entries above ASCII in DOWN and UP, from the
    !> generated tables.  Not a port.
    subroutine characters_case_entries(down, up)
        type(handle_type), intent(in) :: down, up
        type(status_type) :: st
        integer :: k

        ! The identity entries of the cased characters.
        do k = 1, up_count
            call char_table_set(down, up_keys(k), int(up_keys(k), int64), st)
            call char_table_set(up, up_values(k), int(up_values(k), int64), st)
        end do

        do k = 1, down_count
            call char_table_set(down, down_values(k), int(down_values(k), int64), st)
            call char_table_set(up, down_keys(k), int(down_keys(k), int64), st)
        end do

        ! The mappings.
        do k = 1, up_count
            call char_table_set(up, up_keys(k), int(up_values(k), int64), st)
        end do

        do k = 1, down_count
            call char_table_set(down, down_keys(k), int(down_values(k), int64), st)
        end do
    end subroutine characters_case_entries
end module casetab_mod
